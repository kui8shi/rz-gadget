//! Define break;
use std::collections::HashMap;

use std::rc::Rc;
use crate::rzil::RzIL;
use crate::memory::memory::Memory;
use crate::regstore::regstore::{RegStore};
//use regstore::regfile::RzRegFile;
#[derive(Clone, Debug)]
pub struct State<Mem, Reg>
    where Mem: Memory,
          Reg: RegStore
{
    pc: u64,
    regstore: Reg,
    mem: Mem,
    local_vars: HashMap<String, Rc<RzIL>>,
}

impl<Mem, Reg> Context<Mem, Reg>
    where Mem: Memory,
          Reg: RegStore
{
    fn set_pc(&mut self, pc: u64) {
        self.pc = pc
    }
    fn get_pc(&self) -> u64 {
        self.pc
    }

    fn is_symbolic(&self) -> bool {
       true
    }

    fn alias_of(&self, reg: String) -> Option<String> {
        self.regstore.get_reg_entry(&reg).alias.clone()
    }

    fn set_local_var(& self, name: &str, value:Rc<RzIL>) -> Option<Rc<RzIL>> {
        self.local_vars.insert(name.to_owned(), value)
    }
    fn get_local_var(& self, name: &str) -> Option<Rc<RzIL>> {
        match self.local_vars.get(name) {
            Some(var) => Some(var.clone()),
            None => None
        }
    }
    fn clear_local_vars(&mut self) {
        self.local_vars.clear();
    }

    fn reg_read(&self, reg: &str) -> Rc<RzIL> {
        self.regstore.read(reg.as_ref())
    }
    fn reg_write(&mut self, reg: &str, source: Rc<RzIL>) {
        self.regstore.write(reg.as_ref(), source)
    }
    fn mem_read(&self, addr: Rc<RzIL>, read_size: usize) -> Rc<RzIL> {
        // Assert read size is multiple of 8
        assert_eq!(read_size%8, 0, "Read Size is not divisible by 8");
        self.mem.read(addr, read_size)
    }
    fn mem_write(&mut self, addr: Rc<RzIL>, data: Rc<RzIL>, write_size: usize) {
        // Assert write size is multiple of 8
        assert_eq!(write_size%8, 0, "Write Size is not divisible by 8");
        self.mem.write(addr, data, write_size);
    }
}
/*
mod test {
    use super::*;

    use rustproof_libsmt::logics::qf_abv;
    use rustproof_libsmt::backends::smtlib2::SMTLib2;

    use crate::regstore::regfile::RuneRegFile;

    use rzapi::structs::Endian;

    // #[test]
    fn testing_memory_my_dude() {
        let mut lreginfo = Default::default();
        let regstore = RuneRegFile::new(&mut lreginfo);

        let mut mem = SegMem::new(64, Endian::Big);
		let mut smt = SMTLib2::new(Some(qf_abv::QF_ABV));
		mem.init_memory(&mut smt);

        let _ctx = RzSymContext::new(Some(0x9000), mem, regstore, smt);
    }
}
*/
