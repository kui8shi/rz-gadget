use rzpipe::rzpipe::RzPipe;
//use rzpipe::{open_pipe, RzPipeSpawnOptions};
//use rzpipe::RzPipeSpawnOptions;
use quick_cache::sync::Cache;
use serde_json;
use std::rc::Rc;
use std::sync::{Arc, Mutex};

use crate::structs::*;

#[derive(Clone)]
pub struct RzApi {
    pub rzp: Arc<Mutex<RzPipe>>,
    //pub instructions: HashMap<u64, Instruction>,
    //pub permissions: HashMap<u64, Permission>,
    pub info: Information,
    do_cache: bool,
    cache: Rc<Cache<String, String>>,
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
            cache: Rc::new(Cache::new(1000)),
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
            cache: Rc::new(Cache::new(1000)),
        }
    }

    pub fn set_option(&self, key: &str, value: &str) -> Result<String> {
        self.cmd(format!("e {}={}", key, value).as_str())
    }

    pub fn cmd(&self, cmd: &str) -> Result<String> {
        Ok(self.rzp.lock().unwrap().cmd(cmd)?)
    }

    pub fn cmdj<T: serde::de::DeserializeOwned>(&self, cmd: &str) -> Result<T> {
        serde_json::from_value(self.rzp.lock().unwrap().cmdj(cmd)?)
            .map_err(|e| RzError::Json(std::any::type_name::<T>().to_string(), e.to_string()))
    }

    pub fn ccmd(&self, cmd: &str) -> Result<String> {
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
    pub fn close(&self) {
        let _r = self.cmd("q!");
    }

    pub fn disassemble_function<T: AsRef<str>>(&self, func: T) -> Result<DisassembledFunction> {
        let func_name = func.as_ref();
        let cmd = format!("pdfj @ {}", func_name);
        self.cmdj(&cmd)
    }

    pub fn disassemble_n_bytes(&self, n: u64, offset: Option<u64>) -> Result<Vec<Option<Disassembly>>> {
        let result: Vec<serde_json::Value> = serde_json::from_str(&self.cmd(&format!(
            "pDj {} @ {}",
            n,
            offset
                .map(|x| x.to_string())
                .unwrap_or_else(|| "".to_owned())
        ))?).map_err(|e| RzError::Json("Vec<Value>".to_string(), e.to_string()))?;
        let mut ret = Vec::new();
        for disas_json in result {
            let disas: Option<Disassembly> = serde_json::from_value(disas_json).ok();
            ret.push(disas);
        }
        Ok(ret)
    }

    pub fn disassemble_n_insts(&self, n: u64, offset: Option<u64>) -> Result<Vec<Disassembly>> {
        self.cmdj(&format!(
            "pdj {} @ {}",
            n,
            offset
                .map(|x| x.to_string())
                .unwrap_or_else(|| "".to_owned())
        ))
    }

    pub fn get_address(&self, symbol: &str) -> Result<u64> {
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

    // get 'n' (or 16) instructions at 'offset' (or current position if offset is
    // `None`)
    pub fn get_n_insts(&self, n: Option<u64>, offset: Option<u64>) -> Result<Vec<Instruction>> {
        let n = n.unwrap_or(16);
        let mut cmd = format!("aoj {}", n);
        if let Some(o) = offset {
            cmd = format!("{} @ {}", cmd, o);
        }
        self.cmdj(&cmd)
    }

    // get registers of the architecture to be analyzed
    pub fn get_analysis_registers(&self) -> Result<RegisterProfile> {
        self.cmdj("arpj")
    }

    // get registers of the architecture of the cpu running
    pub fn get_cpu_registers(&self) -> Result<RegisterProfile> {
        self.cmdj("drpj")
    }

    pub fn get_flags(&self) -> Result<Vec<FlagInfo>> {
        self.cmdj("flj")
    }

    pub fn get_info(&self) -> Result<Information> {
        self.cmdj("ij")
    }

    pub fn get_shellcode(&self, cmd: &str) -> Result<Vec<u8>> {
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

    pub fn get_cc(&self, location: u64) -> Result<CallingConvention> {
        self.cmdj(&format!("afcrj @ {}", location))
    }

    pub fn get_syscall_cc(&self) -> Result<CallingConvention> {
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

    pub fn get_variables(&self, location: u64) -> Result<VarInfo> {
        self.cmdj(&format!("afvlj @ {}", location))
    }

    pub fn get_functions(&self) -> Result<Vec<FunctionInfo>> {
        self.cmdj("aflj")
    }

    pub fn get_sections(&self) -> Result<Vec<SectionInfo>> {
        self.cmdj("iSj")
    }

    pub fn get_strings(&self, data_only: bool) -> Result<Vec<StringInfo>> {
        if data_only {
            self.cmdj("izj")
        } else {
            self.cmdj("izzj")
        }
    }

    pub fn get_imports(&self) -> Result<Vec<ImportInfo>> {
        self.cmdj("iij")
    }

    pub fn get_exports(&self) -> Result<Vec<SymbolInfo>> {
        self.cmdj("iEj")
    }

    pub fn get_symbols(&self) -> Result<Vec<SymbolInfo>> {
        self.cmdj("isj")
    }

    pub fn get_relocs(&self) -> Result<Vec<RelocInfo>> {
        self.cmdj("irj")
    }

    pub fn get_entrypoint(&self) -> Result<Vec<EntryInfo>> {
        self.cmdj("iej")
    }

    pub fn get_libraries(&self) -> Result<Vec<String>> {
        self.cmdj("ilj")
    }

    pub fn seek(&self, addr: u64) {
        let _r = self.cmd(format!("s {}", addr).as_str());
    }

    // Send a raw command and recv output
    pub fn raw(&self, cmd: String) -> Result<String> {
        self.cmd(&cmd)
    }

    /// All Analysis
    pub fn analyze_all(&self) {
        let _r = self.cmd("aa");
    }

    /// Analyze and auto-name functions
    pub fn analyze_and_autoname(&self) {
        let _r = self.cmd("aaa").unwrap();
    }

    /// Analyze function calls
    pub fn analyze_function_calls(&self) {
        let _r = self.cmd("aac").unwrap();
    }

    /// Analyze data references
    pub fn analyze_data_references(&self) {
        let _r = self.cmd("aad").unwrap();
    }

    /// Analyze references esil
    pub fn analyze_references_esil(&self) {
        let _r = self.cmd("aae").unwrap();
    }

    /// Find and analyze function preludes
    pub fn analyze_function_preludes(&self) {
        let _r = self.cmd("aap").unwrap();
    }

    /// Analyze instruction references
    pub fn analyze_function_references(&self) {
        let _r = self.cmd("aar").unwrap();
    }

    /// Analyze symbols
    pub fn analyze_symbols(&self) {
        let _r = self.cmd("aas").unwrap();
    }

    /// Analyze consecutive functions in section
    pub fn analyze_consecutive_functions(&self) {
        let _r = self.cmd("aat").unwrap();
    }

    // Get RzILVM status
    pub fn get_rzil_vm_status(&self) -> Result<RzILVMStatus> {
        self.cmd("aezi")?;
        self.cmdj("aezvj")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Helper function to create a test RzApi instance
    fn create_test_rzapi() -> RzApi {
        RzApi::new(Some("test/dummy")).unwrap()
    }

    #[test]
    fn test_set_option() {
        let api = create_test_rzapi();
        let result = api.set_option("analysis.esil", "false");
        assert!(result.is_ok());
    }

    #[test]
    fn test_cmd() {
        let api = create_test_rzapi();
        let result = api.cmd("i");
        assert!(result.is_ok());
    }

    #[test]
    fn test_cmdj() {
        let api = create_test_rzapi();
        let result: Result<serde_json::Value> = api.cmdj("ij");
        assert!(result.is_ok());
    }

    #[test]
    fn test_ccmd() {
        let api = create_test_rzapi();
        let result = api.ccmd("i");
        assert!(result.is_ok());
    }

    #[test]
    fn test_function() {
        let api = create_test_rzapi();
        api.analyze_all();
        let result = api.disassemble_function("main");
        assert!(result.is_ok());
    }

    #[test]
    fn test_disassemble_n_bytes() {
        let api = create_test_rzapi();
        let result = api.disassemble_n_bytes(16, Some(0x1044));
        assert!(result.is_ok());
    }

    #[test]
    fn test_disassemble_n_insts() {
        let api = create_test_rzapi();
        let result = api.disassemble_n_insts(5, Some(0x1044));
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_address() {
        let api = create_test_rzapi();
        let result = api.get_address("main");
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_n_insts() {
        let api = create_test_rzapi();
        let result = api.get_n_insts(Some(5), Some(0x1044)).unwrap();
        //assert!(result.is_ok());
    }

    #[test]
    fn test_get_analysis_registers() {
        let api = create_test_rzapi();
        let result = api.get_analysis_registers();
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_cpu_registers() {
        let api = create_test_rzapi();
        let result = api.get_cpu_registers();
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_flags() {
        let api = create_test_rzapi();
        let result = api.get_flags();
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_info() {
        let api = create_test_rzapi();
        let result = api.get_info();
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_shellcode() {
        let api = create_test_rzapi();
        let result = api.get_shellcode("nop");
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_cc() {
        let api = create_test_rzapi();
        api.analyze_all();
        let result = api.get_cc(0x1044);
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_syscall_cc() {
        let api = create_test_rzapi();
        let result = api.get_syscall_cc();
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_variables() {
        let api = create_test_rzapi();
        api.analyze_all();
        let result = api.get_variables(0x1044);
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_functions() {
        let api = create_test_rzapi();
        api.analyze_all();
        let result = api.get_functions();
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_sections() {
        let api = create_test_rzapi();
        let result = api.get_sections();
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_strings() {
        let api = create_test_rzapi();
        let result = api.get_strings(true);
        assert!(result.is_ok());
        let result = api.get_strings(false);
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_imports() {
        let api = create_test_rzapi();
        let result = api.get_imports();
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_exports() {
        let api = create_test_rzapi();
        let result = api.get_exports();
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_symbols() {
        let api = create_test_rzapi();
        let result = api.get_symbols();
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_relocs() {
        let api = create_test_rzapi();
        let result = api.get_relocs();
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_entrypoint() {
        let api = create_test_rzapi();
        let result = api.get_entrypoint();
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_libraries() {
        let api = create_test_rzapi();
        let result = api.get_libraries();
        assert!(result.is_ok());
    }

    #[test]
    fn test_seek() {
        let api = create_test_rzapi();
        api.seek(0x1044);
        // Since seek doesn't return a result, we just ensure it doesn't panic
    }

    #[test]
    fn test_raw() {
        let api = create_test_rzapi();
        let result = api.raw("i".to_string());
        assert!(result.is_ok());
    }

    #[test]
    fn test_analyze_all() {
        let api = create_test_rzapi();
        api.analyze_all();
        // Since analyze_all doesn't return a result, we just ensure it doesn't panic
    }

    #[test]
    fn test_analyze_and_autoname() {
        let api = create_test_rzapi();
        api.analyze_and_autoname();
        // Since analyze_and_autoname doesn't return a result, we just ensure it doesn't panic
    }

    #[test]
    fn test_analyze_function_calls() {
        let api = create_test_rzapi();
        api.analyze_function_calls();
        // Since analyze_function_calls doesn't return a result, we just ensure it doesn't panic
    }

    #[test]
    fn test_analyze_data_references() {
        let api = create_test_rzapi();
        api.analyze_data_references();
        // Since analyze_data_references doesn't return a result, we just ensure it doesn't panic
    }

    #[test]
    fn test_analyze_references_esil() {
        let api = create_test_rzapi();
        api.analyze_references_esil();
        // Since analyze_references_esil doesn't return a result, we just ensure it doesn't panic
    }

    #[test]
    fn test_analyze_function_preludes() {
        let api = create_test_rzapi();
        api.analyze_function_preludes();
        // Since analyze_function_preludes doesn't return a result, we just ensure it doesn't panic
    }

    #[test]
    fn test_analyze_function_references() {
        let api = create_test_rzapi();
        api.analyze_function_references();
        // Since analyze_function_references doesn't return a result, we just ensure it doesn't panic
    }

    #[test]
    fn test_analyze_symbols() {
        let api = create_test_rzapi();
        api.analyze_symbols();
        // Since analyze_symbols doesn't return a result, we just ensure it doesn't panic
    }

    #[test]
    fn test_analyze_consecutive_functions() {
        let api = create_test_rzapi();
        api.analyze_all();
        api.analyze_consecutive_functions();
        // Since analyze_consecutive_functions doesn't return a result, we just ensure it doesn't panic
    }

    #[test]
    fn test_get_rzil_vm_status() {
        let api = create_test_rzapi();
        let result = api.get_rzil_vm_status();
        assert!(result.is_ok());
    }
}
