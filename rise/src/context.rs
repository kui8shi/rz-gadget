//! Define break;
pub(crate) mod memory;
pub(crate) mod process;
pub(crate) mod solver;
use crate::rzil::{
    ast::{Effect, PureRef},
    builder::RzILCache,
};
use memory::{Memory, MemoryRead, MemoryWrite};
use solver::{Solver, Z3Solver};

use rzapi::structs::Endian;
use std::rc::Rc;

use self::process::Process;
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

pub trait Context: Process {
    fn get_pc(&self) -> u64;
    fn set_pc(&mut self, pc: u64) -> u64;
    fn get_status(&self) -> Status;
}

impl Context for RiseContext {
    fn get_pc(&self) -> u64 {
        self.pc
    }
    fn set_pc(&mut self, pc: u64) -> u64 {
        let old_pc = self.pc;
        self.pc = pc;
        old_pc
    }
    fn get_status(&self) -> Status {
        self.status.clone()
    }
}

#[derive(Clone, Debug)]
pub struct RiseContext {
    pc: u64,
    memory: Memory,
    solver: Z3Solver,
    rzil: RzILCache,
    status: Status,
}

impl RiseContext {
    pub fn new(solver: Z3Solver, rzil: RzILCache) -> Self {
        RiseContext {
            pc: 0,
            memory: Memory::new(Endian::Little),
            solver,
            rzil,
            status: Status::Continue,
        }
    }
}
