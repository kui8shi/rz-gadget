//! Provides structures for JSON encoding and decoding
use std::boxed::Box;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(tag = "opcode", rename_all = "snake_case")]
pub enum RzILInfo {
    // RzILInfoOpPure
    Var {
        value: String,
    },
    Ite {
        condition: Box<RzILInfo>,
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    Let {
        dst: String,
        exp: Box<RzILInfo>,
        body: Box<RzILInfo>,
    },
    // Box<RzILInfo>OpBool
    Bool {
        value: bool,
    },

    #[serde(rename = "!")]
    BoolInv {
        x: Box<RzILInfo>,
    },
    #[serde(rename = "&&")]
    BoolAnd {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    #[serde(rename = "||")]
    BoolOr {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    #[serde(rename = "^^")]
    BoolXor {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    // Box<RzILInfo>OpBitvector
    Bitv {
        bits: String,
        len: usize,
    },
    Msb {
        bv: Box<RzILInfo>,
    },
    Lsb {
        bv: Box<RzILInfo>,
    },
    IsZero {
        bv: Box<RzILInfo>,
    },
    #[serde(rename = "~-")]
    Neg {
        bv: Box<RzILInfo>,
    },
    #[serde(rename = "~")]
    LogNot {
        bv: Box<RzILInfo>,
    },
    #[serde(rename = "+")]
    Add {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    #[serde(rename = "-")]
    Sub {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    #[serde(rename = "*")]
    Mul {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    Div {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    Sdiv {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    Mod {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    Smod {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    #[serde(rename = "&")]
    LogAnd {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    #[serde(rename = "|")]
    LogOr {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    #[serde(rename = "^")]
    LogXor {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    #[serde(rename = ">>")]
    ShiftRight {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
        fill_bit: Box<RzILInfo>,
    },
    #[serde(rename = "<<")]
    ShiftLeft {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
        fill_bit: Box<RzILInfo>,
    },
    #[serde(rename = "==")]
    Equal {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    Sle {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    Ule {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    Cast {
        value: Box<RzILInfo>,
        length: usize,
        fill: Box<RzILInfo>,
    },
    Append {
        high: Box<RzILInfo>,
        low: Box<RzILInfo>,
    },
    // Box<RzILInfo>OpFloat
    Float {
        format: u64,
        bv: Box<RzILInfo>,
    },
    Fbits {
        f: Box<RzILInfo>,
    },
    IsFinite {
        f: Box<RzILInfo>,
    },
    IsNan {
        f: Box<RzILInfo>,
    },
    IsInf {
        f: Box<RzILInfo>,
    },
    IsFzero {
        f: Box<RzILInfo>,
    },
    IsFneg {
        f: Box<RzILInfo>,
    },
    IsFpos {
        f: Box<RzILInfo>,
    },
    Fneg {
        f: Box<RzILInfo>,
    },
    Fpos {
        f: Box<RzILInfo>,
    },
    FcastInt {
        length: u64,
        rmode: String,
        value: Box<RzILInfo>,
    },
    FcastSint {
        length: u64,
        rmode: String,
        value: Box<RzILInfo>,
    },
    FcastFloat {
        format: String,
        rmode: String,
        value: Box<RzILInfo>,
    },
    FcastSfloat {
        format: String,
        rmode: String,
        value: Box<RzILInfo>,
    },
    Fconvert {
        format: String,
        rmode: String,
        value: Box<RzILInfo>,
    },
    Fround {
        rmode: String,
        value: Box<RzILInfo>,
    },
    Frequal {
        rmode_x: String,
        rmode_y: String,
        value: Box<RzILInfo>,
    },
    Fsucc {
        f: Box<RzILInfo>,
    },
    Fpred {
        f: Box<RzILInfo>,
    },
    #[serde(rename = "<")]
    Forder {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    Fsqrt {
        rmode: String,
        f: Box<RzILInfo>,
    },
    Frsqrt {
        rmode: String,
        f: Box<RzILInfo>,
    },
    #[serde(rename = "+.")]
    Fadd {
        rmode: String,
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    #[serde(rename = "-.")]
    Fsub {
        rmode: String,
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    #[serde(rename = "*.")]
    Fmul {
        rmode: String,
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    #[serde(rename = "/.")]
    Fdiv {
        rmode: String,
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    #[serde(rename = "%.")]
    Fmod {
        rmode: String,
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    Hypot {
        rmode: String,
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    Pow {
        rmode: String,
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    Fmad {
        rmode: String,
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
        z: Box<RzILInfo>,
    },
    Fpown {
        rmode: String,
        f: Box<RzILInfo>,
        n: Box<RzILInfo>,
    },
    Frootn {
        rmode: String,
        f: Box<RzILInfo>,
        n: Box<RzILInfo>,
    },
    Fcompound {
        rmode: String,
        f: Box<RzILInfo>,
        n: Box<RzILInfo>,
    },
    // Box<RzILInfo>OpEffect
    Load {
        mem: String,
        key: Box<RzILInfo>,
    },
    Loadw {
        mem: u64,
        key: Box<RzILInfo>,
        bits: u64,
    },
    Store {
        mem: u64,
        key: Box<RzILInfo>,
        value: Box<RzILInfo>,
    },
    Storew {
        mem: u64,
        key: Box<RzILInfo>,
        value: Box<RzILInfo>,
    },
    Nop,
    Set {
        dst: String,
        src: Box<RzILInfo>,
    },
    Jmp {
        dst: Box<RzILInfo>,
    },
    Goto {
        label: String,
    },
    Seq {
        x: Box<RzILInfo>,
        y: Box<RzILInfo>,
    },
    Blk {
        label: String,
        data: Box<RzILInfo>,
        ctrl: Box<RzILInfo>,
    },
    Repeat {
        condition: Box<RzILInfo>,
        data_eff: Box<RzILInfo>,
    },
    Branch {
        condition: Box<RzILInfo>,
        true_eff: Box<RzILInfo>,
        false_eff: Box<RzILInfo>,
    },
    #[default]
    Empty,
}

pub type RzILVMRegValue = serde_json::Value;
pub type RzILVMStatus = std::collections::HashMap<String, RzILVMRegValue>;

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Disassembly {
    pub offset: u64,
    pub esil: String,
    pub refptr: bool,
    pub fcn_addr: u64,
    pub fcn_last: u64,
    pub size: u64,
    pub opcode: String,
    pub disasm: String,
    pub bytes: String,
    pub family: String,
    #[serde(rename = "type")]
    pub disasm_type: String,
    pub reloc: bool,
    pub type_num: u64,
    pub type2_num: u64,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Instruction {
    pub opcode: String,
    pub disasm: String,
    pub pseudo: String,
    pub description: Option<String>,
    pub mnemonic: String,
    pub mask: String,
    pub jump: Option<u64>,
    pub fail: Option<u64>,
    //pub esil: String,
    pub rzil: RzILInfo,
    pub sign: bool,
    pub prefix: u64,
    pub id: u64,
    // pub opex : Value,
    pub addr: u64,
    pub bytes: String,
    pub size: u64,
    #[serde(rename = "type")]
    pub inst_type: String,
    pub esilcost: u64,
    pub scale: u64,
    pub refptr: u64,
    pub cycles: u64,
    pub failcycles: u64,
    pub delay: u64,
    pub stackptr: u64,
    pub family: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CrossRef {
    pub from: u64,
    pub to: u64,
    #[serde(rename = "type")]
    pub ref_type: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Storage {
    #[serde(rename = "type")]
    pub storage_type: String,
    pub reg: String,
    pub stack_off: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Variable {
    pub name: String,
    pub arg: bool,
    #[serde(rename = "type")]
    pub var_type: String,
    pub storage: Storage,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct FunctionInfo {
    pub offset: u64,
    pub name: String,
    pub size: u64,
    #[serde(rename = "is-pure")]
    pub is_pure: bool,
    pub realsz: u64,
    pub noreturn: bool,
    pub stackframe: u64,
    pub calltype: String,
    pub cost: u64,
    pub cc: u64,
    pub bits: u64,
    #[serde(rename = "type")]
    pub func_type: String,
    pub nbbs: u64, // number of basic blocks
    pub edges: u64,
    pub ebbs: u64,
    pub signature: String,
    pub minbound: u64,
    pub maxbound: u64,
    pub callrefs: Vec<CrossRef>,
    pub datarefs: Vec<CrossRef>,
    pub callxrefs: Vec<CrossRef>,
    pub dataxrefs: Vec<CrossRef>,
    pub indegree: u64,
    pub outdegree: u64,
    pub nlocals: u64,
    pub nargs: u64,
    pub stackvars: Vec<Variable>,
    pub regvars: Vec<Variable>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RegisterProfile {
    pub alias_info: Vec<AliasInfo>,
    pub reg_info: Vec<RegisterInfo>,
}

/*
#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum RegisterRole {
    PC, // program counter
    SP, // stack pointer
    SR, // status register
    BP, // base pointer
    LR, // link register

    A0, // arguments
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    A9,

    R0, // return registers
    R1,
    R2,
    R3,

    ZF, // flags
    SF,
    CF,
    OF,

    SN, //syscall number (orig_eax,rax,r0,x0)
    #[default]
    LAST, // not used
}
*/

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum RegisterType {
    Gpr, // General purpose
    Drx,
    Fpu, // Floating point
    Mmx,
    Xmm,
    Ymm,
    Flg,
    Seg,
    Sys, // System
    Sec,
    Vc,  // Vector
    Vcc, // Vector control
    Ctr, // Control
    Last,
    #[default]
    Any,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AliasInfo {
    pub reg: String,
    pub role: u64,
    pub role_str: String,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RegisterInfo {
    pub name: String,
    pub offset: u64,
    pub size: usize,
    #[serde(rename = "type_str")]
    pub reg_type: RegisterType,
    #[serde(rename = "type")]
    pub type_id: u64,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct FlagInfo {
    pub offset: u64,
    pub name: String,
    pub realname: String,
    pub size: u64,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct Information {
    pub core: CoreInfo,
    pub bin: BinInfo,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CoreInfo {
    pub file: String,
    pub size: u64,
    pub mode: String,
    pub format: String,
}

#[derive(Debug, Copy, Default, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename = "endian")]
pub enum Endian {
    #[serde(rename = "BE")]
    Big,
    #[serde(rename = "LE")]
    #[default]
    Little,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct BinInfo {
    pub arch: String,
    pub bits: usize,
    pub endian: Endian,
    pub os: String,
    pub canary: bool,
    #[serde(rename = "PIE")]
    pub pie: bool,
    #[serde(rename = "NX")]
    pub nx: bool,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SectionInfo {
    pub flags: String,
    pub name: String,
    pub perm: String,
    pub paddr: u64,
    pub size: u64,
    pub vaddr: u64,
    pub vsize: u64,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct StringInfo {
    pub length: u64,
    pub ordinal: u64,
    pub paddr: u64,
    pub section: String,
    pub size: u64,
    pub string: String,
    pub vaddr: u64,
    #[serde(rename = "type")]
    pub str_type: String,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct VarInfo {
    pub stack: Vec<Variable>,
    pub reg: Vec<Variable>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct CallingConvention {
    pub name: String,
    pub ret: String,
    pub args: Vec<String>,
    //#[serde(rename = "float_args")]
    //pub fargs: Vec<String>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
// Taken from ELF Spec
pub enum SymbolType {
    #[default]
    Notype,
    Obj,
    Func,
    Section,
    File,
    Common,
    Loos,
    Hios,
    Loproc,
    SparcRegister,
    HiProc,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct SymbolInfo {
    pub demname: String,
    pub flagname: String,
    pub name: String,
    pub paddr: u64,
    pub size: u64,
    #[serde(rename = "type")]
    pub symbol_type: SymbolType,
    pub vaddr: u64,
}

#[derive(Debug, Clone, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
// Taken from ELF Spec
pub enum BindType {
    #[default]
    Global,
    Local,
    Weak,
    Loos,
    Hios,
    Loproc,
    Hiproc,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ImportInfo {
    pub bind: BindType,
    pub name: String,
    pub ordinal: u64,
    pub plt: u64,
    #[serde(rename = "type")]
    pub import_type: SymbolType,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ExportInfo {
    pub demname: String,
    pub flagname: String,
    pub name: String,
    pub paddr: u64,
    pub size: u64,
    #[serde(rename = "type")]
    pub export_type: SymbolType,
    pub vaddr: u64,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RelocInfo {
    pub is_ifunc: bool,
    pub name: String,
    pub paddr: u64,
    #[serde(rename = "type")]
    pub reloc_type: String,
    pub vaddr: u64,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct EntryInfo {
    pub vaddr: u64,
    pub paddr: u64,
    pub baddr: u64,
    pub laddr: u64,
    pub haddr: u64,
    pub etype: String,
}
