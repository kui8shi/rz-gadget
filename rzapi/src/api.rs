use rzpipe::rzpipe::RzPipe;
use rzpipe::{open_pipe, RzPipeSpawnOptions};
//use rzpipe::RzPipeSpawnOptions;
use serde_json;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

use crate::structs::*;

#[derive(Clone)]
pub struct RzApi {
    pub rzp: Arc<Mutex<RzPipe>>,
    //pub instructions: HashMap<u64, Instruction>,
    //pub permissions: HashMap<u64, Permission>,
    pub info: Information,
    do_cache: bool,
    cache: HashMap<String, String>,
}

#[allow(dead_code)]
fn hex_encode(data: &[u8]) -> String {
    data.iter()
        .map(|d| format!("{:02x}", *d))
        .collect::<Vec<_>>()
        .join("")
}

fn hex_decode(data: &str) -> Vec<u8> {
    let mut result = Vec::with_capacity(data.len());
    for i in 0..data.len() / 2 {
        // TODO Handle ParseIntError
        result.push(u8::from_str_radix(&data[2 * i..2 * i + 2], 16).unwrap());
    }
    result
}

#[derive(thiserror::Error, Debug)]
pub enum RzError {
    #[error("{0}")]
    Init(#[from] rzpipe::RzInitError),

    #[error("{0}")]
    Pipe(#[from] rzpipe::RzPipeError),

    #[error("Unable to fit a json into type:{0} due to {1}")]
    Json(String, String),

    #[error("{0}")]
    Other(String),
}

pub type Result<T> = std::result::Result<T, RzError>;
/*
fn rz_result<T, E: std::fmt::Display>(result: Result<T, E>) -> Result<T> {
    match result {
        Ok(res) => Ok(res),
        Err(e) => Err(e.to_string()),
    }
}
*/

impl RzApi {
    pub fn new<T: AsRef<str>>(
        path: Option<T>,
        //opts: Option<RzPipeSpawnOptions>,
    ) -> Result<RzApi> {
        if path.is_none() && !RzApi::in_session() {
            return Err(rzpipe::RzInitError::NoSessionOpenError.into());
        }

        // This means that path is `Some` or we have an open session.
        let pipe = RzPipe::spawn(path.unwrap(), None)?;

        let mut rzapi = RzApi {
            rzp: Arc::new(Mutex::new(pipe)),
            info: Information::default(),
            do_cache: false,
            cache: HashMap::new(),
        };
        rzapi.info = rzapi.get_info()?;
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

    pub fn set_option(&mut self, key: &str, value: &str) -> Result<String> {
        self.cmd(format!("e {}={}", key, value).as_str())
    }

    pub fn cmd(&mut self, cmd: &str) -> Result<String> {
        Ok(self.rzp.lock().unwrap().cmd(cmd)?)
    }

    pub fn cmdj<T: serde::de::DeserializeOwned>(&mut self, cmd: &str) -> Result<T> {
        serde_json::from_value(self.rzp.lock().unwrap().cmdj(cmd)?)
            .map_err(|e| RzError::Json(std::any::type_name::<T>().to_string(), e.to_string()))
    }

    pub fn ccmd(&mut self, cmd: &str) -> Result<String> {
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

    pub fn function<T: AsRef<str>>(&mut self, func: T) -> Result<FunctionInfo> {
        let func_name = func.as_ref();
        let cmd = format!("pdfj @ {}", func_name);
        self.cmdj(&cmd)
    }

    pub fn disassemble_n_bytes(&mut self, n: u64, offset: Option<u64>) -> Result<Vec<Disassembly>> {
        self.cmdj(&format!(
            "pDj {} @ {}",
            n,
            offset
                .map(|x| x.to_string())
                .unwrap_or_else(|| "".to_owned())
        ))
    }

    pub fn disassemble_n_insts(&mut self, n: u64, offset: Option<u64>) -> Result<Vec<Disassembly>> {
        self.cmdj(&format!(
            "pdj {} @ {}",
            n,
            offset
                .map(|x| x.to_string())
                .unwrap_or_else(|| "".to_owned())
        ))
    }

    pub fn get_address(&mut self, symbol: &str) -> Result<u64> {
        for prefix in &["", "sym.", "sym.imp.", "sym.unk."] {
            let cmd = format!("%v {}{}", prefix, symbol);
            let val = self.cmd(&cmd).unwrap_or_default();
            if !val.is_empty() && val != "0x0\n" {
                // TODO Handle ParseIntError
                return Ok(u64::from_str_radix(&val[2..val.len() - 1], 16).unwrap());
            }
        }
        Err(RzError::Other(format!("symbol {} was not found", symbol)))
    }

    // get 'n' (or 16) instructions at 'offset' (or current position if offset in
    // `None`)
    pub fn get_n_insts(&mut self, n: Option<u64>, offset: Option<u64>) -> Result<Vec<Instruction>> {
        let n = n.unwrap_or(16);
        let mut cmd = format!("aoj {}", n);
        if let Some(o) = offset {
            cmd = format!("{} @ {}", cmd, o);
        }
        self.cmdj(&cmd)
    }

    // get registers of the architecture to be analyzed
    pub fn get_analysis_registers(&mut self) -> Result<RegisterProfile> {
        self.cmdj("arpj")
    }

    // get registers of the architecture of the cpu running
    pub fn get_cpu_registers(&mut self) -> Result<RegisterProfile> {
        self.cmdj("drpj")
    }

    pub fn get_flags(&mut self) -> Result<Vec<FlagInfo>> {
        self.cmdj("flj")
    }

    pub fn get_info(&mut self) -> Result<Information> {
        self.cmdj("ij")
    }

    pub fn get_shellcode(&mut self, cmd: &str) -> Result<Vec<u8>> {
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

    pub fn get_cc(&mut self, location: u64) -> Result<Vec<CallingConvention>> {
        self.cmdj(&format!("afcrj {}", location))
    }

    pub fn get_syscall_cc(&mut self) -> Result<CallingConvention> {
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
            _ => Err(RzError::Other("calling convention not found".to_owned())),
        }
    }

    pub fn get_variables(&mut self, location: u64) -> Result<VarInfo> {
        self.cmdj(&format!("afvj @ {}", location))
    }

    pub fn get_functions(&mut self) -> Result<Vec<FunctionInfo>> {
        self.cmdj("aflj")
    }

    pub fn get_sections(&mut self) -> Result<Vec<SectionInfo>> {
        self.cmdj("iSj")
    }

    pub fn get_strings(&mut self, data_only: bool) -> Result<Vec<StringInfo>> {
        if data_only {
            self.cmdj("izj")
        } else {
            self.cmdj("izzj")
        }
    }

    pub fn get_imports(&mut self) -> Result<Vec<ImportInfo>> {
        self.cmdj("iij")
    }

    pub fn get_exports(&mut self) -> Result<Vec<ExportInfo>> {
        self.cmdj("iej")
    }

    pub fn get_symbols(&mut self) -> Result<Vec<SymbolInfo>> {
        self.cmdj("isj")
    }

    pub fn get_relocs(&mut self) -> Result<Vec<RelocInfo>> {
        self.cmdj("irj")
    }

    pub fn get_entrypoint(&mut self) -> Result<Vec<EntryInfo>> {
        self.cmdj("iej")
    }

    pub fn get_libraries(&mut self) -> Result<Vec<String>> {
        self.cmdj("ilj")
    }

    pub fn seek(&mut self, addr: u64) {
        let _r = self.cmd(format!("s {}", addr).as_str());
    }

    // Send a raw command and recv output
    pub fn raw(&mut self, cmd: String) -> Result<String> {
        self.cmd(&cmd)
    }

    pub fn analyze_n_insts(&mut self, n: u64, offset: Option<u64>) -> Result<Vec<Instruction>> {
        self.set_option("analysis.esil", "true")?;
        let ret = self.cmdj(&format!(
            "aoj {} @ {}",
            n,
            offset
                .map(|x| x.to_string())
                .unwrap_or_else(|| "".to_owned())
        ))?;
        self.set_option("analysis.esil", "false")?;
        Ok(ret)
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
    pub fn get_rzil_vm_status(&mut self) -> Result<RzILVMStatus> {
        self.cmd("aezi")?;
        self.cmdj("aezvj")
    }
}
