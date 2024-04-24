pub(crate) mod memory;
pub(crate) mod process;
pub(crate) mod solver;
use crate::rzil::{
    ast::{Effect, PureRef},
    builder::RzILCache,
};
use memory::Memory;
use solver::Z3Solver;

use rzapi::structs::Endian;

#[derive(Clone, Debug)]
pub enum Status {
    Continue,
    LoadInst,
    UnconstrainedJump {
        addr: PureRef,
    },
    UnconstrainedBranch {
        branch: Effect,
        following: Vec<Effect>,
    },
    Goto {
        label: String,
    },
    Terminated,
}

#[derive(Clone, Debug)]
pub struct State {
    pc: u64,
    memory: Memory,
    solver: Z3Solver,
    rzil: RzILCache,
    status: Status,
}

impl State {
    pub fn new(solver: Z3Solver, rzil: RzILCache) -> Self {
        State {
            pc: 0,
            memory: Memory::new(Endian::Little),
            solver,
            rzil,
            status: Status::LoadInst,
        }
    }
}

impl State {
    pub fn get_pc(&self) -> u64 {
        self.pc
    }
    pub fn set_pc(&mut self, pc: u64) -> u64 {
        let old_pc = self.pc;
        self.pc = pc;
        old_pc
    }
    pub fn get_status(&self) -> Status {
        self.status.clone()
    }
}
