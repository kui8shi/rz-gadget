pub(crate) mod memory;
pub(crate) mod process;
pub(crate) mod solver;
use crate::rzil::{builder::RzILCache, Effect, PureRef};
use memory::Memory;
use solver::Z3Solver;

use rzapi::structs::Endian;

#[derive(Clone, Debug)]
pub enum Status {
    LoadOp,
    LoadInst,
    UnconstrainedJump {
        addr: PureRef,
    },
    UnconstrainedBranch {
        condition: PureRef,
        then: Box<Effect>,
        otherwise: Box<Effect>,
        post_dominant: Vec<Effect>,
    },
    Goto {
        label: String,
    },
    Terminated,
}

#[derive(Clone, Debug)]
pub struct StateZ3Backend {
    pc: u64,
    memory: Memory,
    solver: Z3Solver,
    rzil: RzILCache,
    status: Status,
}

pub trait State {
    fn new(rzil: RzILCache, pc: Option<u64>) -> Self;
    fn get_pc(&self) -> u64;
    fn set_pc(&mut self, pc: u64) -> u64;
    fn get_status(&self) -> Status;
    fn set_status(&mut self, status: Status);
}

impl State for StateZ3Backend {
    fn new(rzil: RzILCache, pc: Option<u64>) -> Self {
        StateZ3Backend {
            pc: pc.unwrap_or(0),
            memory: Memory::new(Endian::Little),
            solver: Z3Solver::new(),
            rzil,
            status: Status::LoadInst,
        }
    }

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
    fn set_status(&mut self, status: Status) {
        self.status = status
    }
}
