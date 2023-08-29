use rzpipe::errors::RzPipeLangError;
use rzpipe::open_pipe;
use rzpipe::rzpipe::RzPipe;
use rzpipe::RzPipeSpawnOptions;
use serde_json::from_str;
use std::boxed::Box;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

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
        len: u32,
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
        length: u32,
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
    pub r#type: String,
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
    pub r#type: String,
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
    pub r#type: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Storage {
    pub r#type: String,
    pub reg: String,
    pub stack_off: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Variable {
    pub name: String,
    pub arg: bool,
    pub r#type: String,
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
    pub r#type: String,
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
    pub reg_info: Vec<RegInfo>,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum RegRole {
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

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AliasInfo {
    pub reg: String,
    pub role: u64,
    pub role_str: RegRole,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum RegType {
    #[default]
    Gpr,
    Drx,
    Fpu,
    Mmx,
    Xmm,
    Ymm,
    Flg,
    Seg,
    Sys,
    Sec,
    Vc,
    Vcc,
    Ctr,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RegInfo {
    pub name: String,
    pub offset: u64,
    pub size: u32,
    pub type_str: RegType,
    pub r#type: u64,
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

#[derive(Debug, Copy, Default, Clone, Serialize, Deserialize)]
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
    pub bits: u32,
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
    pub stype: String,
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

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
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
    pub stype: SymbolType,
    pub vaddr: u64,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
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
    pub itype: SymbolType,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ExportInfo {
    pub demname: String,
    pub flagname: String,
    pub name: String,
    pub paddr: u64,
    pub size: u64,
    #[serde(rename = "type")]
    pub etype: SymbolType,
    pub vaddr: u64,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct RelocInfo {
    pub is_ifunc: bool,
    pub name: String,
    pub paddr: u64,
    #[serde(rename = "type")]
    pub rtype: String,
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

#[derive(Clone)]
pub struct RzApi {
    pub rzp: Arc<Mutex<RzPipe>>,
    //pub instructions: HashMap<u64, Instruction>,
    //pub permissions: HashMap<u64, Permission>,
    pub info: Information,
    do_cache: bool,
    cache: HashMap<String, String>,
}
fn hex_encode(data: &[u8]) -> String {
    data.iter()
        .map(|d| format!("{:02x}", *d))
        .collect::<Vec<_>>()
        .join("")
}

fn hex_decode(data: &str) -> Vec<u8> {
    let mut result = Vec::with_capacity(data.len());
    for i in 0..data.len() / 2 {
        result.push(u8::from_str_radix(&data[2 * i..2 * i + 2], 16).unwrap());
    }
    result
}
pub type RzResult<T> = std::result::Result<T, String>;
fn rz_result<T, E: std::fmt::Display>(result: Result<T, E>) -> RzResult<T> {
    match result {
        Ok(res) => Ok(res),
        Err(e) => Err(e.to_string()),
    }
}

impl RzApi {
    pub fn new<T: AsRef<str>>(
        path: Option<T>,
        opts: Option<RzPipeSpawnOptions>,
    ) -> RzResult<RzApi> {
        if path.is_none() && !RzApi::in_session() {
            let e = "No rizin session open. Please specify path.".to_owned();
            return Err(e);
        }

        let pipe = match open_pipe!(path.as_ref()) {
            Ok(p) => p,
            Err(_) => {
                // This means that path is `Some` or we have an open session.
                return Err(
                    "Path could not be resolved or we do not have an open session!".to_owned(),
                );
            }
        };

        let mut rzapi = RzApi {
            rzp: Arc::new(Mutex::new(pipe)),
            info: Information::default(),
            do_cache: false,
            cache: HashMap::new(),
        };
        rzapi.info = match rzapi.get_info() {
            Ok(info) => info,
            Err(e) => {
                return Err(e.to_string());
            }
        };

        rzapi.set_option("analysis.esil", "false")?;
        rzapi.set_option("scr.color", "0")?;
        Ok(rzapi)
    }

    pub fn in_session() -> bool {
        RzPipe::in_session().is_some()
    }

    pub fn from(pipe: RzPipe) -> RzApi {
        RzApi {
            rzp: Arc::new(Mutex::new(pipe)),
            info: Information::default(),
            do_cache: false,
            cache: HashMap::new(),
        }
    }

    pub fn set_option(&mut self, key: &str, value: &str) -> RzResult<String> {
        self.cmd(format!("e {}={}", key, value).as_str())
    }

    pub fn cmd(&mut self, cmd: &str) -> RzResult<String> {
        rz_result(self.rzp.lock().unwrap().cmd(cmd))
    }

    pub fn ccmd(&mut self, cmd: &str) -> RzResult<String> {
        if self.do_cache {
            if let Some(result) = self.cache.get(cmd) {
                Ok(result.to_owned())
            } else {
                let result = self.cmd(cmd)?;
                self.cache.insert(cmd.to_owned(), result.clone());
                Ok(result)
            }
        } else {
            self.cmd(cmd)
        }
    }
    pub fn close(&mut self) {
        let _r = self.cmd("q!");
    }

    pub fn function<T: AsRef<str>>(&mut self, func: T) -> RzResult<FunctionInfo> {
        let func_name = func.as_ref();
        let cmd = format!("pdfj @ {}", func_name);
        let raw_json = self.cmd(&cmd)?;
        // Handle errors here.
        rz_result(from_str(&raw_json)) //(|e| Box::new(RzPipeLangError::ParsingJson(e.to_string())))
    }

    pub fn disassemble_n_bytes(
        &mut self,
        n: u64,
        offset: Option<u64>,
    ) -> RzResult<Vec<Disassembly>> {
        let raw_json = self.cmd(&format!(
            "pDj {} @ {}",
            n,
            offset
                .map(|x| x.to_string())
                .unwrap_or_else(|| "".to_owned())
        ))?;
        rz_result(from_str(&raw_json)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    pub fn disassemble_n_insts(
        &mut self,
        n: u64,
        offset: Option<u64>,
    ) -> RzResult<Vec<Disassembly>> {
        let raw_json = self.cmd(&format!(
            "pdj {} @ {}",
            n,
            offset
                .map(|x| x.to_string())
                .unwrap_or_else(|| "".to_owned())
        ))?;
        rz_result(from_str(&raw_json)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    pub fn get_address(&mut self, symbol: &str) -> RzResult<u64> {
        for prefix in &["", "sym.", "sym.imp.", "sym.unk."] {
            let cmd = format!("%v {}{}", prefix, symbol);
            let val = self.cmd(&cmd).unwrap_or_default();
            if val != "" && val != "0x0\n" {
                return rz_result(u64::from_str_radix(&val[2..val.len() - 1], 16));
            }
        }
        Err(format!("symbol {} was not found", symbol))
    }

    // get 'n' (or 16) instructions at 'offset' (or current position if offset in
    // `None`)
    pub fn get_n_insts(
        &mut self,
        n: Option<u64>,
        offset: Option<u64>,
    ) -> RzResult<Vec<Instruction>> {
        let n = n.unwrap_or(16);
        let mut cmd = format!("aoj {}", n);
        if let Some(o) = offset {
            cmd = format!("{} @ {}", cmd, o.to_string());
        }
        let raw_json = self.cmd(&cmd)?;
        rz_result(from_str(&raw_json)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    pub fn get_registers(&mut self) -> RzResult<RegisterProfile> {
        let raw_json = self.cmd("drpj")?;
        rz_result(from_str(&raw_json)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    pub fn get_flags(&mut self) -> RzResult<Vec<FlagInfo>> {
        let raw_json = self.cmd("flj")?;
        rz_result(from_str(&raw_json)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    pub fn get_info(&mut self) -> RzResult<Information> {
        let raw_json = self.cmd("ij")?;
        rz_result(from_str(&raw_json)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    pub fn get_shellcode(&mut self, cmd: &str) -> RzResult<Vec<u8>> {
        let result = self.cmd(&format!("gr;gi exec;gc cmd={};g", cmd))?;
        Ok(hex_decode(&result))
    }

    /*
        arch/ABI      arg1  arg2  arg3  arg4  arg5  arg6  arg7  Notes
    ──────────────────────────────────────────────────────────────
    alpha         a0    a1    a2    a3    a4    a5    -
    arc           r0    r1    r2    r3    r4    r5    -
    arm/OABI      a1    a2    a3    a4    v1    v2    v3
    arm/EABI      r0    r1    r2    r3    r4    r5    r6
    arm64         x0    x1    x2    x3    x4    x5    -
    blackfin      R0    R1    R2    R3    R4    R5    -
    i386          ebx   ecx   edx   esi   edi   ebp   -
    ia64          out0  out1  out2  out3  out4  out5  -
    m68k          d1    d2    d3    d4    d5    a0    -
    microblaze    r5    r6    r7    r8    r9    r10   -
    mips/o32      a0    a1    a2    a3    -     -     -     [1]
    mips/n32,64   a0    a1    a2    a3    a4    a5    -
    nios2         r4    r5    r6    r7    r8    r9    -
    parisc        r26   r25   r24   r23   r22   r21   -
    powerpc       r3    r4    r5    r6    r7    r8    r9
    riscv         a0    a1    a2    a3    a4    a5    -
    s390          r2    r3    r4    r5    r6    r7    -
    s390x         r2    r3    r4    r5    r6    r7    -
    superh        r4    r5    r6    r7    r0    r1    r2
    sparc/32      o0    o1    o2    o3    o4    o5    -
    sparc/64      o0    o1    o2    o3    o4    o5    -
    tile          R00   R01   R02   R03   R04   R05   -
    x86-64        rdi   rsi   rdx   r10   r8    r9    -
    x32           rdi   rsi   rdx   r10   r8    r9    -
    xtensa        a6    a3    a4    a5    a8    a9    -
    */

    pub fn get_cc(&mut self, location: u64) -> RzResult<Vec<CallingConvention>> {
        let raw_json = self.cmd(&format!("afcrj {}", location))?;
        rz_result(from_str(&raw_json)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    pub fn get_syscall_cc(&mut self) -> RzResult<CallingConvention> {
        match (self.info.bin.arch.as_str(), self.info.bin.bits) {
            ("x86", 32) => Ok(CallingConvention {
                name: "x86syscall".to_owned(),
                args: vec![
                    "ebx".to_owned(),
                    "ecx".to_owned(),
                    "edx".to_owned(),
                    "esi".to_owned(),
                    "edi".to_owned(),
                    "ebp".to_owned(),
                ],
                ret: "eax".to_owned(),
            }),
            ("x86", 64) => Ok(CallingConvention {
                name: "amd64syscall".to_owned(),
                args: vec![
                    "rdi".to_owned(),
                    "rsi".to_owned(),
                    "rdx".to_owned(),
                    "r10".to_owned(),
                    "r8".to_owned(),
                    "r9".to_owned(),
                ],
                ret: "rax".to_owned(),
            }),
            // 16 is thumb mode, need to handle better
            ("arm", 16) | ("arm", 32) => Ok(CallingConvention {
                name: "arm16syscall".to_owned(),
                args: vec![
                    "r0".to_owned(),
                    "r1".to_owned(),
                    "r2".to_owned(),
                    "r3".to_owned(),
                    "r4".to_owned(),
                    "r5".to_owned(),
                    "r6".to_owned(),
                ],
                ret: "r0".to_owned(),
            }),
            ("arm", 64) => Ok(CallingConvention {
                name: "arm64syscall".to_owned(),
                args: vec![
                    "x0".to_owned(),
                    "x1".to_owned(),
                    "x2".to_owned(),
                    "x3".to_owned(),
                    "x4".to_owned(),
                    "x5".to_owned(),
                    "x6".to_owned(),
                    "x7".to_owned(),
                    "x8".to_owned(), // supposedly xnu/ios can have up 9 args
                ],
                ret: "x0".to_owned(),
            }),
            ("riscv", _) | ("mips", _) => Ok(CallingConvention {
                name: "riscvsyscall".to_owned(),
                args: vec![
                    "a0".to_owned(),
                    "a1".to_owned(),
                    "a2".to_owned(),
                    "a3".to_owned(),
                    "a4".to_owned(),
                    "a5".to_owned(),
                ],
                ret: "a0".to_owned(),
            }),
            ("sparc", _) => Ok(CallingConvention {
                name: "sparcsyscall".to_owned(),
                args: vec![
                    "o0".to_owned(),
                    "o1".to_owned(),
                    "o2".to_owned(),
                    "o3".to_owned(),
                    "o4".to_owned(),
                    "o5".to_owned(),
                ],
                ret: "o0".to_owned(),
            }),
            ("ppc", _) => Ok(CallingConvention {
                name: "ppcsyscall".to_owned(),
                args: vec![
                    "r3".to_owned(),
                    "r4".to_owned(),
                    "r5".to_owned(),
                    "r6".to_owned(),
                    "r7".to_owned(),
                    "r8".to_owned(),
                    "r9".to_owned(),
                ],
                ret: "r3".to_owned(), // TODO errors are in r0
            }),
            ("xtensa", _) => Ok(CallingConvention {
                name: "xtensasyscall".to_owned(),
                args: vec![
                    "a6".to_owned(),
                    "a3".to_owned(),
                    "a4".to_owned(),
                    "a5".to_owned(),
                    "a8".to_owned(),
                    "a9".to_owned(),
                ],
                ret: "a2".to_owned(),
            }),
            _ => Err("calling convention not found".to_owned()),
        }
    }

    pub fn get_variables(&mut self, location: u64) -> RzResult<VarInfo> {
        let raw_json = self.cmd(&format!("afvj @ {}", location))?;
        rz_result(from_str(&raw_json)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    pub fn get_functions(&mut self) -> RzResult<Vec<FunctionInfo>> {
        let raw_json = self.cmd("aflj")?;
        let finfo = from_str::<Vec<FunctionInfo>>(&raw_json)
            .map_err(|e| RzPipeLangError::ParsingJson(e.to_string()));
        /*
        if let Ok(ref mut fns) = finfo {
            for f in fns.iter_mut() {
                let res = self.locals_of(f.offset.unwrap());
                if res.is_ok() {
                    f.locals = res.ok();
                } else {
                    f.locals = Some(Vec::new());
                }
            }
        }*/
        rz_result(finfo)
    }

    pub fn get_sections(&mut self) -> RzResult<Vec<SectionInfo>> {
        rz_result(from_str(&self.cmd("iSj").unwrap())) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    pub fn get_strings(&mut self, data_only: bool) -> RzResult<Vec<StringInfo>> {
        if data_only {
            rz_result(from_str(&self.cmd("izj")?)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
        } else {
            rz_result(from_str(&self.cmd("izzj")?)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
        }
    }

    pub fn get_imports(&mut self) -> RzResult<Vec<ImportInfo>> {
        rz_result(from_str(&self.cmd("iij")?)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    pub fn get_exports(&mut self) -> RzResult<Vec<ExportInfo>> {
        rz_result(from_str(&self.cmd("iej")?)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    pub fn get_symbols(&mut self) -> RzResult<Vec<SymbolInfo>> {
        rz_result(from_str(&self.cmd("isj")?)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    pub fn get_relocs(&mut self) -> RzResult<Vec<RelocInfo>> {
        rz_result(from_str(&self.cmd("irj")?)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    pub fn get_entrypoint(&mut self) -> RzResult<Vec<EntryInfo>> {
        rz_result(from_str(&self.cmd("iej")?)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    pub fn get_libraries(&mut self) -> RzResult<Vec<String>> {
        rz_result(from_str(&self.cmd("ilj")?)) //(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    pub fn seek(&mut self, addr: u64) {
        let _r = self.cmd(format!("s {}", addr).as_str());
    }

    // Send a raw command and recv output
    pub fn raw(&mut self, cmd: String) -> RzResult<String> {
        self.cmd(&cmd)
    }

    pub fn analyze_n_insts(&mut self, n: u64, offset: Option<u64>) -> RzResult<Vec<Instruction>> {
        self.set_option("analysis.esil", "true")?;
        let raw_json = self.cmd(&format!(
            "aoj {} @ {}",
            n,
            offset
                .map(|x| x.to_string())
                .unwrap_or_else(|| "".to_owned())
        ))?;
        self.set_option("analysis.esil", "false")?;
        rz_result(from_str(&raw_json))
    }

    /// All Analysis
    pub fn analyze_all(&mut self) {
        let _r = self.cmd("aa");
    }

    /// Analyze and auto-name functions
    pub fn analyze_and_autoname(&mut self) {
        let _r = self.cmd("aaa");
    }

    /// Analyze function calls
    pub fn analyze_function_calls(&mut self) {
        let _r = self.cmd("aac");
    }

    /// Analyze data references
    pub fn analyze_data_references(&mut self) {
        let _r = self.cmd("aad");
    }

    /// Analyze references esil
    pub fn analyze_references_esil(&mut self) {
        let _r = self.cmd("aae");
    }

    /// Find and analyze function preludes
    pub fn analyze_function_preludes(&mut self) {
        let _r = self.cmd("aap");
    }

    /// Analyze instruction references
    pub fn analyze_function_references(&mut self) {
        let _r = self.cmd("aar");
    }

    /// Analyze symbols
    pub fn analyze_symbols(&mut self) {
        let _r = self.cmd("aas");
    }

    /// Analyze consecutive functions in section
    pub fn analyze_consecutive_functions(&mut self) {
        let _r = self.cmd("aat");
    }

    // Get RzILVM status
    pub fn get_rzil_vm_status(&mut self) -> RzResult<RzILVMStatus> {
        self.cmd("aezi")?;
        let raw_json = self.cmd("aezvj")?;
        rz_result(from_str(&raw_json))
    }
}

#[test]
fn rzil() {
    let mut rzapi = RzApi::new(Some("/bin/ls"), None)
        .map_err(|e| println!("Error:{}", e))
        .unwrap();
    rzapi.analyze_all();
    let rzil = &rzapi
        .analyze_n_insts(1, Some(0x4e19))
        .map_err(|e| println!("Error:{}", e))
        .unwrap()[0]
        .rzil;
    println!("{:?}", rzil);
}
