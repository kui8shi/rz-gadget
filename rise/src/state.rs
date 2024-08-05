pub(crate) mod memory;
pub(crate) mod process;
pub(crate) mod solver;
use crate::rzil::{builder::RzILBuilder, Effect, PureRef};
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
pub struct StateZ3Backend<S> {
    pc: u64,
    memory: Memory,
    rzil: RzILBuilder,
    status: Status,
    pub(crate) z3: S,
}

pub trait State {
    fn new(rzil: RzILBuilder, pc: Option<u64>) -> Self;
    fn get_pc(&self) -> u64;
    fn set_pc(&mut self, pc: u64) -> u64;
    fn get_status(&self) -> Status;
    fn set_status(&mut self, status: Status);
}

impl<S: Z3Solver> State for StateZ3Backend<S> {
    fn new(rzil: RzILBuilder, pc: Option<u64>) -> Self {
        StateZ3Backend {
            pc: pc.unwrap_or(0),
            memory: Memory::new(Endian::Little),
            z3: Z3Solver::new(),
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
