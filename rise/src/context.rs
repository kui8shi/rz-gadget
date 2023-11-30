//! Define break;
use std::rc::Rc;
use crate::rzil::{RzIL, Pure};
use crate::memory::Memory;
use crate::solver::Solver;
use crate::error::RiseeResult;
use rzapi::structs::Endian;
//use regstore::regfile::RzRegFile;
#[derive(Clone, Debug)]
pub struct Context
{
    pc: u64,
    memory: Memory,
    solver: Solver,
}

impl Context {
    pub fn new() -> Self {
        Context { 
            pc: 0, 
            memory: Memory::new(Endian::Little), 
            solver: Solver::new(),
        }
    }
    
    pub fn get_pc(&self) -> u64 {
        self.pc
    }

    pub fn set_pc(&mut self, pc: u64) -> u64 {
        let old_pc = self.pc;
        self.pc = pc;
        old_pc
    }

    pub fn get_solver(&self) -> &Solver {
        &self.solver
    }

    pub fn get_memory(&self) -> &Memory {
        &self.memory
    }

    pub fn store(&mut self, rzil: &RzIL, addr: Rc<Pure>, val: Rc<Pure>) -> RiseeResult<()> {
        Ok(self.memory.store(&mut self.solver, rzil, addr, val)?)
    }
}
