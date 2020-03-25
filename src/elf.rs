use std::path::Path;
use std::fs::File;
use std::io::Write;
use std::io;

pub struct Elf {
    pub entry_point: u32,
    pub section_strtab_idx: u16,
    pub program_headers: Vec<ProgramHeader>,
    pub section_headers: Vec<SectionHeader>,
    pub data_blocks: Vec<DataBlock>,
}

pub struct DataBlock {
    pub data: Vec<u8>,
    pub align: u32,
}

pub enum ProgramHeader {
    Null,
    Load {
        data_block_idx: usize,
        vaddr: u32,
        flags: u32,
    },
}

pub enum SectionHeader {
    Null,
    ProgBits {
        name_idx: u32,
        data_block_idx: usize,
        flags: u32,
        vaddr: u32,
    },
    // SymTab,
    StrTab {
        name_idx: u32,
        data_block_idx: usize,
    }
}

struct TrackingFile {
    f: File,
    pos: u32,
}

impl TrackingFile {
    pub fn new(f: File) -> Self {
        Self {
            f, pos: 0
        }
    }

    pub fn pos(&self) -> u32 {
        self.pos
    }

    pub fn write_u32(&mut self, i: u32) -> Result<(), io::Error> {
        self.pos += 4;
        self.f.write_all(&[
            ((i >> 0 ) & 0xff) as u8,
            ((i >> 8 ) & 0xff) as u8,
            ((i >> 16) & 0xff) as u8,
            ((i >> 24) & 0xff) as u8,
        ])
    }

    pub fn write_u16(&mut self, i: u16) -> Result<(), io::Error> {
        self.pos += 2;
        self.f.write_all(&[
            ((i >> 0 ) & 0xff) as u8,
            ((i >> 8 ) & 0xff) as u8,
        ])
    }

    pub fn write_u8(&mut self, i: u8) -> Result<(), io::Error> {
        self.pos += 1;
        self.f.write_all(&[i])
    }

    pub fn write(&mut self, data: &[u8]) -> Result<(), io::Error> {
        self.pos += data.len() as u32;
        self.f.write_all(data)
    }
}

impl Elf {
    const FILE_HEADER_SIZE: u16 = 0x34;
    const PROGRAM_HEADER_SIZE: u16 = 0x20;
    const SECTION_HEADER_SIZE: u16 = 0x28;

    fn align(loc: u32, align: u32) -> u32 {
        if loc % align == 0 {
            loc
        } else {
            loc + (align - loc%align)
        }
    }

    pub fn file_header_loc(&self) -> u32 { 0 }
    pub fn program_header_loc(&self) -> u32 { self.file_header_loc() + Self::FILE_HEADER_SIZE as u32 }
    pub fn section_header_loc(&self) -> u32 {
        self.program_header_loc() +
            self.program_headers.len() as u32 * Self::PROGRAM_HEADER_SIZE as u32
    }
    pub fn data_loc(&self, index: usize) -> u32 {
        let mut loc = self.section_header_loc() +
            self.section_headers.len() as u32 * Self::SECTION_HEADER_SIZE as u32;

        for block in &self.data_blocks[0..index] {
            loc = Self::align(loc, block.align);
            loc += block.data.len() as u32;
        }

        Self::align(loc, self.data_blocks[index].align)
    }

    fn write_file_header(&self, f: &mut TrackingFile) -> Result<(), io::Error> {
        /* magic */         f.write(&[0x7f, 0x45, 0x4c, 0x46])?;
        /* 32-bit */        f.write_u8(0x01)?;
        /* little-endian */ f.write_u8(0x01)?;
        /* ELF v1 */        f.write_u8(0x01)?;
        /* System V */      f.write_u8(0x00)?;
        /* no ABI */        f.write_u8(0x00)?;
        /* (pad) */         f.write(&[0x00; 7])?;

        /* executable */    f.write_u16(0x0002)?;
        /* ISA: 386 */      f.write_u16(0x0003)?;
        /* ELF v1 */        f.write_u32(1)?;

        f.write_u32(self.entry_point)?;
        f.write_u32(if self.program_headers.is_empty() { 0 } else { self.program_header_loc() })?;
        f.write_u32(if self.section_headers.is_empty() { 0 } else { self.section_header_loc() })?;

        /* target flags */  f.write_u32(0)?;

        f.write_u16(Self::FILE_HEADER_SIZE)?;
        f.write_u16(Self::PROGRAM_HEADER_SIZE)?;
        f.write_u16(self.program_headers.len() as u16)?;
        f.write_u16(Self::SECTION_HEADER_SIZE)?;
        f.write_u16(self.section_headers.len() as u16)?;

        f.write_u16(self.section_strtab_idx)?;

        Ok(())
    }

    fn write_program_header(f: &mut TrackingFile,
        typ: u32,
        offset: u32,
        vaddr: u32,
        paddr: u32,
        file_size: u32,
        mem_size: u32,
        flags: u32,
        align: u32
    ) -> Result<(), io::Error> {
        f.write_u32(typ)?;
        f.write_u32(offset)?;
        f.write_u32(vaddr)?;
        f.write_u32(paddr)?;
        f.write_u32(file_size)?;
        f.write_u32(mem_size)?;
        f.write_u32(flags)?;
        f.write_u32(align)?;
        Ok(())
    }

    fn write_program_headers(&self, f: &mut TrackingFile) -> Result<(), io::Error> {
        for header in self.program_headers.iter() {
            match header {
                ProgramHeader::Null => {
                    Self::write_program_header(f, 0, 0, 0, 0, 0, 0, 0, 0)?;
                }
                ProgramHeader::Load { data_block_idx, vaddr, flags } => {
                    Self::write_program_header(f,
                        1,
                        self.data_loc(*data_block_idx),
                        *vaddr,
                        *vaddr,
                        self.data_blocks[*data_block_idx].data.len() as u32,
                        self.data_blocks[*data_block_idx].data.len() as u32,
                        *flags,
                        self.data_blocks[*data_block_idx].align,
                    )?;
                }
            }
        }

        Ok(())
    }

    fn write_section_header(f: &mut TrackingFile,
        name_index: u32,
        typ: u32,
        flags: u32,
        vaddr: u32,
        offset: u32,
        file_size: u32,
        link: u32,
        info: u32,
        addr_align: u32,
        ent_size: u32,
    ) -> Result<(), io::Error> {
        f.write_u32(name_index)?;
        f.write_u32(typ)?;
        f.write_u32(flags)?;
        f.write_u32(vaddr)?;
        f.write_u32(offset)?;
        f.write_u32(file_size)?;
        f.write_u32(link)?;
        f.write_u32(info)?;
        f.write_u32(addr_align)?;
        f.write_u32(ent_size)?;
        Ok(())
    }

    fn write_section_headers(&self, f: &mut TrackingFile) -> Result<(), io::Error> {
        for header in self.section_headers.iter() {
            match header {
                SectionHeader::Null => {
                    Self::write_section_header(f, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)?;
                }
                SectionHeader::ProgBits { name_idx, data_block_idx, flags, vaddr } => {
                    Self::write_section_header(f,
                        *name_idx,
                        1,
                        *flags,
                        *vaddr,
                        self.data_loc(*data_block_idx),
                        self.data_blocks[*data_block_idx].data.len() as u32,
                        0,
                        0,
                        0x10,
                        0
                    )?;
                }
                SectionHeader::StrTab { name_idx, data_block_idx } => {
                    Self::write_section_header(f,
                        *name_idx,
                        3,
                        0,
                        0,
                        self.data_loc(*data_block_idx),
                        self.data_blocks[*data_block_idx].data.len() as u32,
                        0,
                        0,
                        1,
                        0
                    )?;
                }
            }
        }

        Ok(())
    }

    fn write_data(&self, f: &mut TrackingFile) -> Result<(), io::Error> {
        for block in self.data_blocks.iter() {
            let align_zeros = Self::align(f.pos(), block.align) - f.pos();
            for _ in 0..align_zeros {
                f.write_u8(0x00)?;
            }

            f.write(&block.data)?;
        }

        Ok(())
    }

    pub fn write_to(&self, path: &Path) -> Result<(), io::Error> {
        let mut f = TrackingFile::new(File::create(path)?);

        self.write_file_header(&mut f)?;
        self.write_program_headers(&mut f)?;
        self.write_section_headers(&mut f)?;
        self.write_data(&mut f)?;

        Ok(())
    }
}