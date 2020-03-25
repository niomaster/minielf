#![cfg_attr(debug_assertions, allow(dead_code))]

mod elf;

use std::path::Path;

fn main() {
    let path = Path::new("test");
    let blocks = vec![
        elf::DataBlock { data: "\0.shstrtab\0.text\0".as_bytes().to_vec(), align: 1 },
        elf::DataBlock { data: vec![0xb8, 0x01, 0x00, 0x00, 0x00, 0xbb, 0x2a, 0x00, 0x00, 0x00, 0xcd, 0x80], align: 0x1000 },
    ];
    elf::Elf {
        entry_point: 0x8048000,
        section_strtab_idx: 1,
        program_headers: vec![
            elf::ProgramHeader::Load {
                data_block_idx: 1,
                vaddr: 0x8048000,
                flags: 0x5,
            },
        ],
        section_headers: vec![
            elf::SectionHeader::Null,
            elf::SectionHeader::StrTab {
                name_idx: 1,
                data_block_idx: 0,
            },
            elf::SectionHeader::ProgBits {
                name_idx: 11,
                data_block_idx: 1,
                flags: 0x6,
                vaddr: 0x8048000,
            }
        ],
        data_blocks: blocks,
    }.write_to(path).unwrap();
}