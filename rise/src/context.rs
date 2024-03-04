//! Define break;
use crate::rzil::{builder::RzILBuilder, PureRef};
use crate::memory::Memory;
use crate::solver::Solver;
use crate::error::RiseResult;
use rzapi::structs::Endian;
//use std::rc::Rc;
//use regstore::regfile::RzRegFile;
#[derive(Clone, Debug)]
pub struct Context<S>
{
    pc: u64,
    memory: Memory,
    solver: S,
}

impl<S: Solver> Context<S> {
    pub fn new(solver: S) -> Self {
        Context { 
            pc: 0, 
            memory: Memory::new(Endian::Little), 
            solver,
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

    pub fn get_solver(&self) -> &S {
        &self.solver
    }

    pub fn get_memory(&self) -> &Memory {
        &self.memory
    }

    //pub fn assert(&self, rzil: &RzILGenerator, )

    pub fn store(&mut self, rzil: &RzILBuilder, addr: PureRef, val: PureRef) -> RiseResult<()>    {
        Ok(self.memory.store(&self.solver, rzil, addr, val)?)
    }
}
