use crate::api_trait::RzApi;
use crate::structs::*;

use rzpipe::errors::RzPipeLangError;
use rzpipe::rz::Rz;
use serde_json::from_str;

impl RzApi for Rz {
    fn analyze(&mut self) {
        self.send("aaa");
        self.flush();
    }

    fn init(&mut self) {
        self.send("e asm.esil = true");
        self.send("e scr.color = false");
        self.analyze()
    }

    fn function<T: AsRef<str>>(&mut self, func: T) -> Result<LFunctionInfo, RzPipeLangError> {
        let func_name = func.as_ref();
        let cmd = format!("pdfj @ {}", func_name);
        self.send(&cmd);
        let raw_json = self.recv();
        // Handle errors here.
        from_str(&raw_json).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    fn disassemble_n_bytes(
        &mut self,
        n: u64,
        offset: Option<u64>,
    ) -> Result<Vec<LOpInfo>, RzPipeLangError> {
        self.send(&format!(
            "pDj {} @ {}",
            n,
            offset
                .map(|x| x.to_string())
                .unwrap_or_else(|| "".to_owned())
        ));
        let s = &self.recv();
        from_str(s).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    fn disassemble_n_insts(
        &mut self,
        n: u64,
        offset: Option<u64>,
    ) -> Result<Vec<LOpInfo>, RzPipeLangError> {
        self.send(&format!(
            "pdj {} @ {}",
            n,
            offset
                .map(|x| x.to_string())
                .unwrap_or_else(|| "".to_owned())
        ));
        from_str(&self.recv()).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    // get 'n' (or 16) instructions at 'offset' (or current position if offset in
    // `None`)
    fn insts<T: AsRef<str>>(
        &mut self,
        n: Option<u64>,
        offset: Option<T>,
    ) -> Result<Vec<LOpInfo>, RzPipeLangError> {
        let n = n.unwrap_or(16);
        let mut cmd = format!("pdj{}", n);
        if let Some(o) = offset {
            cmd = format!("{} @ {}", cmd, o.as_ref());
        }
        self.send(&cmd);
        let raw_json = self.recv();
        from_str(&raw_json).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    fn reg_info(&mut self) -> Result<LRegInfo, RzPipeLangError> {
        self.send("drpj");
        let raw_json = self.recv();
        from_str(&raw_json).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    fn flag_info(&mut self) -> Result<Vec<LFlagInfo>, RzPipeLangError> {
        self.send("fj");
        let raw_json = self.recv();
        from_str(&raw_json).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    fn bin_info(&mut self) -> Result<LBinInfo, RzPipeLangError> {
        self.send("ij");
        let raw_json = self.recv();
        from_str(&raw_json).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    fn cc_info_of(&mut self, location: u64) -> Result<LCCInfo, RzPipeLangError> {
        self.send(&format!("afcrj @ {}", location));
        let raw_json = self.recv();
        from_str(&raw_json).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    fn locals_of(&mut self, location: u64) -> Result<Vec<LVarInfo>, RzPipeLangError> {
        self.send(&format!("afvbj @ {}", location));
        from_str(&self.recv()).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    fn fn_list(&mut self) -> Result<Vec<FunctionInfo>, RzPipeLangError> {
        self.send("aflj");
        let raw_json = self.recv();
        let mut finfo = from_str::<Vec<FunctionInfo>>(&raw_json)
            .map_err(|e| RzPipeLangError::ParsingJson(e.to_string()));
        if let Ok(ref mut fns) = finfo {
            for f in fns.iter_mut() {
                let res = self.locals_of(f.offset.unwrap());
                if res.is_ok() {
                    f.locals = res.ok();
                } else {
                    f.locals = Some(Vec::new());
                }
            }
        }
        finfo
    }

    fn sections(&mut self) -> Result<Vec<LSectionInfo>, RzPipeLangError> {
        self.send("iSj");
        from_str(&self.recv()).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    fn strings(&mut self, data_only: bool) -> Result<Vec<LStringInfo>, RzPipeLangError> {
        if data_only {
            self.send("izj");
            from_str(&self.recv()).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
        } else {
            self.send("izzj");
            from_str(&self.recv()).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
        }
    }

    fn imports(&mut self) -> Result<Vec<LImportInfo>, RzPipeLangError> {
        self.send("iij");
        from_str(&self.recv()).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    fn exports(&mut self) -> Result<Vec<LExportInfo>, RzPipeLangError> {
        self.send("iej");
        from_str(&self.recv()).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    fn symbols(&mut self) -> Result<Vec<LSymbolInfo>, RzPipeLangError> {
        self.send("isj");
        from_str(&self.recv()).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    fn relocs(&mut self) -> Result<Vec<LRelocInfo>, RzPipeLangError> {
        self.send("irj");
        from_str(&self.recv()).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    fn entrypoint(&mut self) -> Result<Vec<LEntryInfo>, RzPipeLangError> {
        self.send("iej");
        from_str(&self.recv()).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    fn libraries(&mut self) -> Result<Vec<String>, RzPipeLangError> {
        self.send("ilj");
        from_str(&self.recv()).map_err(|e| RzPipeLangError::ParsingJson(e.to_string()))
    }

    // Send a raw command and recv output
    fn raw(&mut self, cmd: String) -> Result<String, RzPipeLangError> {
        self.send(&cmd);
        Ok(self.recv())
    }

    /// All Analysis
    fn analyze_all(&mut self) {
        self.send("aa");
        self.recv();
    }

    /// Analyze and auto-name functions
    fn analyze_and_autoname(&mut self) {
        self.send("aaa");
        self.recv();
    }

    /// Analyze function calls
    fn analyze_function_calls(&mut self) {
        self.send("aac");
        self.recv();
    }

    /// Analyze data references
    fn analyze_data_references(&mut self) {
        self.send("aad");
        self.recv();
    }

    /// Analyze references esil
    fn analyze_references_esil(&mut self) {
        self.send("aae");
        self.recv();
    }

    /// Find and analyze function preludes
    fn analyze_function_preludes(&mut self) {
        self.send("aap");
        self.recv();
    }

    /// Analyze instruction references
    fn analyze_function_references(&mut self) {
        self.send("aar");
        self.recv();
    }

    /// Analyze symbols
    fn analyze_symbols(&mut self) {
        self.send("aas");
        self.recv();
    }

    /// Analyze consecutive functions in section
    fn analyze_consecutive_functions(&mut self) {
        self.send("aat");
        self.recv();
    }
}
