//! Define break;
use std::rc::Rc;
use crate::rzil::{builder::RzILBuilder, PureRef, Effect};
use crate::memory::Memory;
use crate::solver::Solver;
use crate::error::Result;
use rzapi::structs::Endian;
//use std::rc::Rc;
//use regstore::regfile::RzRegFile;
#[derive(Clone, Debug)]
pub enum Status {
    Continue,
    DirectJump(u64),
    SymbolicJump(PureRef),
    Goto(String),
    Branch(PureRef, Rc<Effect>, Rc<Effect>),
}

#[derive(Clone, Debug)]
pub struct Context<S: Solver>
{
    pc: u64,
    memory: Memory,
    solver: S,
    status: Status,
}

impl<S: Solver> Context<S> {
    pub fn new(solver: S) -> Self {
        Context { 
            pc: 0, 
            memory: Memory::new(Endian::Little), 
            solver,
            status: Status::Continue
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

    pub fn get_status(&self) -> Status {
        self.status.clone()
    }
    //pub fn assert(&self, rzil: &RzILGenerator, )

    pub fn store(&mut self, rzil: &RzILBuilder, addr: PureRef, val: PureRef) -> Result<()>    {
        Ok(self.memory.store(&self.solver, rzil, addr, val)?)
    }
}
