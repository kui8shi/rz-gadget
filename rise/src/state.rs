pub(crate) mod memory;
pub(crate) mod process;
pub(crate) mod solver;
use crate::rzil::{
    ast::{Effect, PureRef},
    builder::{RzILBuilder, RzILCache},
};
use memory::Memory;
use solver::Z3Solver;

use rzapi::structs::Endian;

use self::{memory::MemoryOps, solver::Solver};

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
pub struct State_Z3Backend {
    pc: u64,
    memory: Memory,
    solver: Z3Solver,
    rzil: RzILCache,
    status: Status,
}

impl State_Z3Backend {
    pub fn new(solver: Z3Solver, rzil: RzILCache) -> Self {
        State_Z3Backend {
            pc: 0,
            memory: Memory::new(Endian::Little),
            solver,
            rzil,
            status: Status::LoadInst,
        }
    }
}

pub trait State: MemoryOps + Solver + RzILBuilder {
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
impl State for State_Z3Backend {
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
