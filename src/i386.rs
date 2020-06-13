/*
These are the addressing forms for 16-bit mode, which I don't want to model now.

enum ModRM {
    DBxSi    = 0b00_000_000,
    DBxDi    = 0b00_000_001,
    DBpSi    = 0b00_000_010,
    DBpDi    = 0b00_000_011,
    DSi      = 0b00_000_100,
    DDi      = 0b00_000_101,
    D16      = 0b00_000_110,
    DBx      = 0b00_000_111,

    DBxSiD8  = 0b01_000_000,
    DBxDiD8  = 0b01_000_001,
    DBpSiD8  = 0b01_000_010,
    DBpDiD8  = 0b01_000_011,
    DSiD8    = 0b01_000_100,
    DDiD8    = 0b01_000_101,
    DBpD8    = 0b01_000_110,
    DBxD8    = 0b01_000_111,

    DBxSiD16 = 0b10_000_000,
    DBxDiD16 = 0b10_000_001,
    DBpSiD16 = 0b10_000_010,
    DBpDiD16 = 0b10_000_011,
    DSiD16   = 0b10_000_100,
    DDiD16   = 0b10_000_101,
    DBpD16   = 0b10_000_110,
    DBxD16   = 0b10_000_111,

    EaxAxAl  = 0b11_000_000,
    EcxCxCl  = 0b11_000_001,
    EdxDxDl  = 0b11_000_010,
    EbxBxBl  = 0b11_000_011,
    EspSpAh  = 0b11_000_100,
    EbpBpCh  = 0b11_000_101,
    EsiSiDh  = 0b11_000_110,
    EdiDiBh  = 0b11_000_111,
}
*/

/* === Byte representation of instruction === */

#[derive(Debug, Clone, Copy)]
enum ModRM {
    DEax     = 0b00_000_000,
    DEcx     = 0b00_000_001,
    DEdx     = 0b00_000_010,
    DEbx     = 0b00_000_011,
    SIB      = 0b00_000_100,
    D32      = 0b00_000_101,
    DEsi     = 0b00_000_110,
    DEdi     = 0b00_000_111,

    DEaxD8   = 0b01_000_000,
    DEcxD8   = 0b01_000_001,
    DEdxD8   = 0b01_000_010,
    DEbxD8   = 0b01_000_011,
    SIBD8    = 0b01_000_100,
    DEbpD8   = 0b01_000_101,
    DEsiD8   = 0b01_000_110,
    DEdiD8   = 0b01_000_111,

    DEaxD32  = 0b10_000_000,
    DEcxD32  = 0b10_000_001,
    DEdxD32  = 0b10_000_010,
    DEbxD32  = 0b10_000_011,
    SIBD32   = 0b10_000_100,
    DEbpD32  = 0b10_000_101,
    DEsiD32  = 0b10_000_110,
    DEdiD32  = 0b10_000_111,

    EaxAxAl  = 0b11_000_000,
    EcxCxCl  = 0b11_000_001,
    EdxDxDl  = 0b11_000_010,
    EbxBxBl  = 0b11_000_011,
    EspSpAh  = 0b11_000_100,
    EbpBpCh  = 0b11_000_101,
    EsiSiDh  = 0b11_000_110,
    EdiDiBh  = 0b11_000_111,
}

#[derive(Debug, Clone, Copy)]
enum ModRMReg {
    EaxAxAl = 0b00_000_000,
    EcxCxCl = 0b00_001_000,
    EdxDxDl = 0b00_010_000,
    EbxBxBl = 0b00_011_000,
    EspSpAh = 0b00_100_000,
    EbpBpCh = 0b00_101_000,
    EsiSiDh = 0b00_110_000,
    EdiDiBh = 0b00_111_000,
}

#[derive(Debug, Clone, Copy)]
enum ModRMOp {
    Op0 = 0b00_000_000,
    Op1 = 0b00_001_000,
    Op2 = 0b00_010_000,
    Op3 = 0b00_011_000,
    Op4 = 0b00_100_000,
    Op5 = 0b00_101_000,
    Op6 = 0b00_110_000,
    Op7 = 0b00_111_000,
}

#[derive(Debug, Clone, Copy)]
enum ModRMByte {
    Skip,
    Op(ModRM, ModRMOp),
    Reg(ModRM, ModRMReg),
}

#[derive(Debug, Clone, Copy)]
enum RB {
    AL = 0,
    CL = 1,
    DL = 2,
    BL = 3,
    AH = 4,
    CH = 5,
    DH = 6,
    BH = 7,
}

#[derive(Debug, Clone, Copy)]
enum RW {
    AX = 0,
    CX = 1,
    DX = 2,
    BX = 3,
    SP = 4,
    BP = 5,
    SI = 6,
    DI = 7,
}

#[derive(Debug, Clone, Copy)]
enum RD {
    EAX = 0,
    ECX = 1,
    EDX = 2,
    EBX = 3,
    ESP = 4,
    EBP = 5,
    ESI = 6,
    EDI = 7,
}

#[derive(Debug, Clone, Copy)]
enum Plus {
    Skip,
    RB(RB),
    RW(RW),
    RD(RD),
}

#[derive(Debug, Clone, Copy)]
enum Disp {
    Skip,
    Byte(i8),
    Word(i16),
    DWord(i32),
}

#[derive(Debug, Clone, Copy)]
enum Imm {
    Skip,
    Byte(i8),
    Word(i16),
    DWord(i32),
}

#[derive(Debug, Clone, Copy)]
pub struct IOp {
    op_prefix: Option<u8>,
    op: u8,
    plus: Plus,
    modrm: ModRMByte,
    disp: Disp,
    imm: Imm,
}

/* === Slightly more high-level representation of instructions === */

// Range of a byte. Can't be a constant, because match arms don't accept a ops::RangeInclusive
macro_rules! brng { () => (-128..=127); }

#[derive(Debug, Clone, Copy)]
pub enum BReg {
    AL,
    BL,
    CL,
    DL,
    AH,
    BH,
    CH,
    DH,
}

//#[derive(Debug, Clone, Copy)]
//pub enum WReg {
//    AX,
//    BX,
//    CX,
//    DX,
//    SP,
//    BP,
//    SI,
//    DI,
//}

#[derive(Debug, Clone, Copy)]
pub enum DReg {
    EAX,
    EBX,
    ECX,
    EDX,
    ESP,
    EBP,
    ESI,
    EDI,
}

#[derive(Debug, Clone, Copy)]
pub enum Reg {
    B(BReg),
//    W(WReg),
    D(DReg),
}

impl BReg {
    fn rb(&self) -> RB {
        match self {
            BReg::AL => RB::AL,
            BReg::BL => RB::BL,
            BReg::CL => RB::CL,
            BReg::DL => RB::DL,
            BReg::AH => RB::AH,
            BReg::BH => RB::BH,
            BReg::CH => RB::CH,
            BReg::DH => RB::DH,
        }
    }

    fn modrm(&self) -> ModRM {
        match self {
            BReg::AL => ModRM::EaxAxAl,
            BReg::BL => ModRM::EbxBxBl,
            BReg::CL => ModRM::EcxCxCl,
            BReg::DL => ModRM::EdxDxDl,
            BReg::AH => ModRM::EspSpAh,
            BReg::BH => ModRM::EdiDiBh,
            BReg::CH => ModRM::EbpBpCh,
            BReg::DH => ModRM::EsiSiDh,
        }
    }

    fn modreg(&self) -> ModRMReg {
        match self {
            BReg::AL => ModRMReg::EaxAxAl,
            BReg::BL => ModRMReg::EbxBxBl,
            BReg::CL => ModRMReg::EcxCxCl,
            BReg::DL => ModRMReg::EdxDxDl,
            BReg::AH => ModRMReg::EspSpAh,
            BReg::BH => ModRMReg::EdiDiBh,
            BReg::CH => ModRMReg::EbpBpCh,
            BReg::DH => ModRMReg::EsiSiDh,
        }
    }
}

//impl WReg {
//    fn rw(&self) -> RW {
//        match self {
//            WReg::AX => RW::AX,
//            WReg::BX => RW::BX,
//            WReg::CX => RW::CX,
//            WReg::DX => RW::DX,
//            WReg::SP => RW::SP,
//            WReg::BP => RW::BP,
//            WReg::SI => RW::SI,
//            WReg::DI => RW::DI,
//        }
//    }
//
//    fn modrm(&self) -> ModRM {
//        match self {
//            WReg::AX => ModRM::EaxAxAl,
//            WReg::BX => ModRM::EbxBxBl,
//            WReg::CX => ModRM::EcxCxCl,
//            WReg::DX => ModRM::EdxDxDl,
//            WReg::SP => ModRM::EspSpAh,
//            WReg::BP => ModRM::EbpBpCh,
//            WReg::SI => ModRM::EsiSiDh,
//            WReg::DI => ModRM::EdiDiBh,
//        }
//    }
//
//    fn modreg(&self) -> ModRMReg {
//        match self {
//            WReg::AX => ModRMReg::EaxAxAl,
//            WReg::BX => ModRMReg::EbxBxBl,
//            WReg::CX => ModRMReg::EcxCxCl,
//            WReg::DX => ModRMReg::EdxDxDl,
//            WReg::SP => ModRMReg::EspSpAh,
//            WReg::BP => ModRMReg::EbpBpCh,
//            WReg::SI => ModRMReg::EsiSiDh,
//            WReg::DI => ModRMReg::EdiDiBh,sn
//        }
//    }
//}

impl DReg {
    fn rd(&self) -> RD {
        match self {
            DReg::EAX => RD::EAX,
            DReg::EBX => RD::EBX,
            DReg::ECX => RD::ECX,
            DReg::EDX => RD::EDX,
            DReg::ESP => RD::ESP,
            DReg::EBP => RD::EBP,
            DReg::ESI => RD::ESI,
            DReg::EDI => RD::EDI,
        }
    }

    fn modrm(&self) -> ModRM {
        match self {
            DReg::EAX => ModRM::EaxAxAl,
            DReg::EBX => ModRM::EbxBxBl,
            DReg::ECX => ModRM::EcxCxCl,
            DReg::EDX => ModRM::EdxDxDl,
            DReg::ESP => ModRM::EspSpAh,
            DReg::EBP => ModRM::EbpBpCh,
            DReg::ESI => ModRM::EsiSiDh,
            DReg::EDI => ModRM::EdiDiBh,
        }
    }

    fn modreg(&self) -> ModRMReg {
        match self {
            DReg::EAX => ModRMReg::EaxAxAl,
            DReg::EBX => ModRMReg::EbxBxBl,
            DReg::ECX => ModRMReg::EcxCxCl,
            DReg::EDX => ModRMReg::EdxDxDl,
            DReg::ESP => ModRMReg::EspSpAh,
            DReg::EBP => ModRMReg::EbpBpCh,
            DReg::ESI => ModRMReg::EsiSiDh,
            DReg::EDI => ModRMReg::EdiDiBh,
        }
    }
}

impl Reg {
    fn plus(&self) -> Plus {
        match self {
            Reg::B(breg) => Plus::RB(breg.rb()),
//            Reg::W(wreg) => Plus::RW(wreg.rw()),
            Reg::D(dreg) => Plus::RD(dreg.rd()),
        }
    }

    fn modrm(&self) -> ModRM {
        match self {
            Reg::B(breg) => breg.modrm(),
//            Reg::W(wreg) => wreg.modrm(),
            Reg::D(dreg) => dreg.modrm(),
        }
    }

    fn modreg(&self) -> ModRMReg {
        match self {
            Reg::B(breg) => breg.modreg(),
//            Reg::W(wreg) => wreg.modreg(),
            Reg::D(dreg) => dreg.modreg(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Mem {
    EAX(i32),
    EBX(i32),
    ECX(i32),
    EDX(i32),
    ESI(i32),
    EDI(i32),
    Imm(i32),
    EBP(i32),
}

impl Mem {
    fn modrm(&self) -> ModRM {
        match self {
            Mem::EAX(0) => ModRM::DEax,
            Mem::EAX(brng!()) => ModRM::DEaxD8,
            Mem::EAX(_) => ModRM::DEaxD32,

            Mem::EBX(0) => ModRM::DEbx,
            Mem::EBX(brng!()) => ModRM::DEbxD8,
            Mem::EBX(_) => ModRM::DEbxD32,

            Mem::ECX(0) => ModRM::DEcx,
            Mem::ECX(brng!()) => ModRM::DEcxD8,
            Mem::ECX(_) => ModRM::DEcxD32,

            Mem::EDX(0) => ModRM::DEdx,
            Mem::EDX(brng!()) => ModRM::DEdxD8,
            Mem::EDX(_) => ModRM::DEdxD32,

            Mem::ESI(0) => ModRM::DEsi,
            Mem::ESI(brng!()) => ModRM::DEsiD8,
            Mem::ESI(_) => ModRM::DEsiD32,

            Mem::EDI(0) => ModRM::DEdi,
            Mem::EDI(brng!()) => ModRM::DEdiD8,
            Mem::EDI(_) => ModRM::DEdiD32,

            Mem::Imm(_) => ModRM::D32,

            Mem::EBP(brng!()) => ModRM::DEbpD8,
            Mem::EBP(_) => ModRM::DEbpD32,
        }
    }

    fn disp(&self) -> Disp {
        match self {
            Mem::EAX(0) | Mem::EBX(0) | Mem::ECX(0) | Mem::EDX(0) | Mem::ESI(0) | Mem::EDI(0) =>
                Disp::Skip,
            Mem::EAX(b@brng!()) | Mem::EBX(b@brng!()) | Mem::ECX(b@brng!()) | Mem::EDX(b@brng!()) | Mem::ESI(b@brng!()) | Mem::EDI(b@brng!()) =>
                Disp::Byte(*b as i8),
            Mem::EAX(d) | Mem::EBX(d) | Mem::ECX(d) | Mem::EDX(d) | Mem::ESI(d) | Mem::EDI(d) =>
                Disp::DWord(*d),

            Mem::Imm(d) => Disp::DWord(*d),
            Mem::EBP(b@brng!()) => Disp::Byte(*b as i8),
            Mem::EBP(d) => Disp::DWord(*d),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Loc {
    MemB(Mem),
    MemD(Mem),
    Reg(Reg),
}

impl Loc {
    fn modrm(&self) -> ModRM {
        match self {
            Loc::MemB(m) | Loc::MemD(m) => m.modrm(),
            Loc::Reg(r) => r.modrm(),
        }
    }

    fn disp(&self) -> Disp {
        match self {
            Loc::MemB(m) | Loc::MemD(m) => m.disp(),
            Loc::Reg(_) => Disp::Skip,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Val {
    Loc(Loc),
    Imm(i32),
}

impl Val {
    fn imm(&self) -> Imm {
        match self {
            Val::Loc(_) => Imm::Skip,
            Val::Imm(b@brng!()) => Imm::Byte(*b as i8),
            Val::Imm(d) => Imm::DWord(*d),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Instr {
    /*
    Skipped:
    - ASCII adjust
    - Decimal adjust
    - ARPL
    - CALL
    - CMPS{,B,W,D}
    - ENTER
    */
    ADC(Loc, Val),
    ADD(Loc, Val),
    AND(Loc, Val),
    BOUND(Mem, DReg),
    BSF(Loc, DReg),
    BSR(Loc, DReg),
    BT(Loc, Val),
    BTC(Loc, Val),
    BTR(Loc, Val),
    BTS(Loc, Val),
    CBDE,
    CLC,
    CLD,
    CLI,
    CLTS,
    CMC,
    CMP(Loc, Val),
    CDQ,
    DEC(Loc),
    DIV(Loc, Val),
    HLT,
    IDIV(Loc, Val),
    IMUL(Loc, Val),
}

impl Instr {
    fn calc_op(loc: &Loc, val: &Val,
               al_op: u8, eax_op: u8,
               im: ModRMOp,
               r8_op: u8, r32_op:
              u8, tr8_op: u8, tr32_op: u8
    ) -> Result<IOp, &'static str> {
        Ok(match val {
            Val::Imm(brng!()) => match loc {
                Loc::Reg(Reg::B(BReg::AL)) =>
                    IOp::imm(al_op, val.imm()),
                Loc::Reg(Reg::B(_)) | Loc::MemB(_) =>
                    IOp::imm_op(0x80, im, loc.modrm(), loc.disp(), val.imm()),
                Loc::Reg(Reg::D(_)) | Loc::MemD(_) =>
                    IOp::imm_op(0x83, im, loc.modrm(), loc.disp(), val.imm()),
            },
            Val::Imm(_) => match loc {
                Loc::Reg(Reg::D(DReg::EAX)) =>
                    IOp::imm(eax_op, val.imm()),
                Loc::Reg(Reg::B(_)) | Loc::MemB(_) =>
                    { return Err("Cannot apply dword immediate to byte location") }
                Loc::Reg(Reg::D(_)) | Loc::MemD(_) =>
                    IOp::imm_op(0x81, im, loc.modrm(), loc.disp(), val.imm())
            }
            Val::Loc(Loc::Reg(Reg::B(val))) => match loc {
                Loc::Reg(Reg::B(_)) | Loc::MemB(_) =>
                    IOp::reg_rm(r8_op, val.modreg(), loc.modrm(), loc.disp()),
                Loc::Reg(Reg::D(_)) | Loc::MemD(_) =>
                    { return Err("Cannot apply byte register to dword target") }
            }
            Val::Loc(Loc::Reg(Reg::D(val))) => match loc {
                Loc::Reg(Reg::D(_)) | Loc::MemD(_) =>
                    IOp::reg_rm(r32_op, val.modreg(), loc.modrm(), loc.disp()),
                Loc::Reg(Reg::B(_)) | Loc::MemB(_) =>
                    { return Err("Cannot apply dword register to byte target") }
            }
            Val::Loc(Loc::MemB(val)) => match loc {
                Loc::Reg(Reg::B(loc)) =>
                    IOp::reg_rm(tr8_op, loc.modreg(), val.modrm(), val.disp()),
                Loc::Reg(Reg::D(_)) =>
                    { return Err("Cannot apply byte mem to dword register") }
                Loc::MemB(_) | Loc::MemD(_) =>
                    { return Err("Memory/memory operations are unrepresentable") }
            }
            Val::Loc(Loc::MemD(val)) => match loc {
                Loc::Reg(Reg::D(loc)) =>
                    IOp::reg_rm(tr32_op, loc.modreg(), val.modrm(), val.disp()),
                Loc::Reg(Reg::B(_)) =>
                    { return Err("Cannot apply dword mem to byte register") }
                Loc::MemB(_) | Loc::MemD(_) =>
                    { return Err("Memory/memory operations are unrepresentable") }
            }
        })
    }

    fn bt_op(val: &Loc, index: &Val, reg_op: u8, imm_op: ModRMOp) -> Result<IOp, &'static str> {
        Ok(match index {
            Val::Imm(brng!()) =>
                IOp::imm_op_0f(0xba, imm_op, val.modrm(), val.disp(), index.imm()),
            Val::Imm(_) =>
                { return Err("Immediate index to bit test out of range") }
            Val::Loc(Loc::Reg(Reg::D(dreg))) =>
                IOp::reg_rm_0f(reg_op, dreg.modreg(), val.modrm(), val.disp()),
            Val::Loc(Loc::Reg(Reg::B(_))) =>
                { return Err("Cannot use non-DWord-register to index into bits") }
            Val::Loc(Loc::MemB(_)) | Val::Loc(Loc::MemD(_)) =>
                { return Err("Cannot use memory to index into bits") }
        })
    }

    fn div_op(loc: &Loc, val: &Val, op: ModRMOp) -> Result<IOp, &'static str> {
        Ok(match loc {
            Loc::Reg(Reg::B(BReg::AL)) => match val {
                Val::Imm(_) =>
                    { return Err("divisor may not be immediate") }
                Val::Loc(val) => match val {
                    Loc::MemB(_) | Loc::Reg(Reg::B(_)) =>
                        IOp::op(0xf6, op, val.modrm()),
                    Loc::MemD(_) | Loc::Reg(Reg::D(_)) =>
                        { return Err("Cannot apply dword divisor to byte dividend") }
                }
            }
            Loc::Reg(Reg::D(DReg::EAX)) => match val {
                Val::Imm(_) =>
                    { return Err("divisor may not be immediate") }
                Val::Loc(val) => match val {
                    Loc::MemD(_) | Loc::Reg(Reg::D(_)) =>
                        IOp::op(0xf7, op, val.modrm()),
                    Loc::MemB(_) | Loc::Reg(Reg::B(_)) =>
                        { return Err("Cannot apply byte divisor to dword dividend") }
                }
            }
            Loc::MemD(_) | Loc::MemB(_) | Loc::Reg(_) =>
                { return Err("dividend may only be AL or EAX") }
        })
    }

    pub fn as_op(&self) -> Result<IOp, &'static str> {
        Ok(match self {
            Instr::ADC(loc, val) =>
                Self::calc_op(loc, val, 0x14, 0x15, ModRMOp::Op2, 0x10, 0x11, 0x12, 0x13)?,
            Instr::ADD(loc, val) =>
                Self::calc_op(loc, val, 0x04, 0x05, ModRMOp::Op0, 0x00, 0x01, 0x02, 0x03)?,
            Instr::AND(loc, val) =>
                Self::calc_op(loc, val, 0x24, 0x25, ModRMOp::Op4, 0x20, 0x21, 0x22, 0x23)?,
            Instr::BOUND(bound, idx) =>
                IOp::reg_rm(0x62, idx.modreg(), bound.modrm(), bound.disp()),
            Instr::BSF(bits, counter) =>
                IOp::reg_rm_0f(0xbc, counter.modreg(), bits.modrm(), bits.disp()),
            Instr::BSR(bits, counter) =>
                IOp::reg_rm_0f(0xbd, counter.modreg(), bits.modrm(), bits.disp()),
            Instr::BT(val, index) =>
                Self::bt_op(val, index, 0xa3, ModRMOp::Op4)?,
            Instr::BTC(val, index) =>
                Self::bt_op(val, index, 0xbb, ModRMOp::Op7)?,
            Instr::BTR(val, index) =>
                Self::bt_op(val, index, 0xb3, ModRMOp::Op6)?,
            Instr::BTS(val, index) =>
                Self::bt_op(val, index, 0xab, ModRMOp::Op5)?,
            Instr::CBDE =>
                IOp::unit(0x98),
            Instr::CLC =>
                IOp::unit(0xf8),
            Instr::CLD =>
                IOp::unit(0xfc),
            Instr::CLI =>
                IOp::unit(0xfa),
            Instr::CLTS =>
                IOp::unit_0f(0x06),
            Instr::CMC =>
                IOp::unit(0xf5),
            Instr::CMP(loc, val) =>
                Self::calc_op(loc, val, 0x3c, 0x3d, ModRMOp::Op7, 0x38, 0x39, 0x3a, 0x3b)?,
            Instr::CDQ =>
                IOp::unit(0x99),
            Instr::DEC(loc) => match loc {
                Loc::Reg(Reg::D(dreg)) =>
                    IOp::plus(0x48, Plus::RD(dreg.rd())),
                Loc::Reg(Reg::B(breg)) =>
                    IOp::op(0xfe, ModRMOp::Op1, breg.modrm()),
                Loc::MemB(loc) =>
                    IOp::op(0xfe, ModRMOp::Op1, loc.modrm()),
                Loc::MemD(loc) =>
                    IOp::op(0xff, ModRMOp::Op1, loc.modrm())
            }
            Instr::DIV(loc, val) =>
                Self::div_op(loc, val, ModRMOp::Op6)?,
            Instr::HLT =>
                IOp::unit(0xf4),
            Instr::IDIV(loc, val) =>
                Self::div_op(loc, val, ModRMOp::Op7)?,
            Instr::IMUL(loc, val) => match loc {
                Loc::Reg(Reg::B(BReg::AL)) => match val {
                    Val::Loc(val) =>
                        IOp::op(0xf6, ModRMOp::Op5, val.modrm()),
                    
                }
            },

        })
    }
}

impl IOp {
    fn unit(op: u8) -> Self {
        Self::new(None, op, Plus::Skip, ModRMByte::Skip, Disp::Skip, Imm::Skip)
    }

    fn unit_0f(op: u8) -> Self {
        Self::new(Some(0x0f), op, Plus::Skip, ModRMByte::Skip, Disp::Skip, Imm::Skip)
    }

    fn op(op:u8, eop: ModRMOp, modrm: ModRM) -> Self {
        Self::new(None, op, Plus::Skip, ModRMByte::Op(modrm, eop), Disp::Skip, Imm::Skip)
    }

    fn imm(op: u8, imm: Imm) -> Self {
        Self::new(None, op, Plus::Skip, ModRMByte::Skip, Disp::Skip, imm)
    }

    fn imm_op(op: u8, eop: ModRMOp, modrm: ModRM, disp: Disp, imm: Imm) -> Self {
        Self::new(None, op, Plus::Skip, ModRMByte::Op(modrm, eop), disp, imm)
    }

    fn plus(op:u8, plus: Plus) -> Self {
        Self::new(None, op, plus, ModRMByte::Skip, Disp::Skip, Imm::Skip)
    }

    fn reg_rm(op: u8, reg: ModRMReg, modrm: ModRM, disp: Disp) -> Self {
        Self::new(None, op, Plus::Skip, ModRMByte::Reg(modrm, reg), disp, Imm::Skip)
    }

    fn imm_op_0f(op: u8, eop: ModRMOp, modrm: ModRM, disp: Disp, imm: Imm) -> Self {
        Self::new(None, op, Plus::Skip, ModRMByte::Op(modrm, eop), disp, imm)
    }

    fn reg_rm_0f(op: u8, reg: ModRMReg, modrm: ModRM, disp: Disp) -> Self {
        Self::new(Some(0x0f), op, Plus::Skip, ModRMByte::Reg(modrm, reg), disp, Imm::Skip)
    }

    fn new(op_prefix: Option<u8>, op: u8, plus: Plus, modrm: ModRMByte, disp: Disp, imm: Imm) -> Self {
        Self { op_prefix, op, plus, modrm, disp, imm }
    }
}