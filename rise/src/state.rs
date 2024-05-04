pub(crate) mod memory;
pub(crate) mod process;
pub(crate) mod solver;
use crate::{
    convert::ConvertRzILToSymExp,
    rzil::{builder::RzILCache, Effect, PureRef},
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
pub struct StateZ3Backend {
    pc: u64,
    memory: Memory,
    solver: Z3Solver,
    rzil: RzILCache,
    status: Status,
}

pub trait State: MemoryOps + Solver + ConvertRzILToSymExp {
    fn new(rzil: RzILCache) -> Self;
    fn get_pc(&self) -> u64;
    fn set_pc(&mut self, pc: u64) -> u64;
    fn get_status(&self) -> Status;
}

impl State for StateZ3Backend {
    fn new(rzil: RzILCache) -> Self {
        StateZ3Backend {
            pc: 0,
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
}
