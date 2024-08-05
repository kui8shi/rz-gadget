use std::collections::VecDeque;

use super::memory::MemoryOps;
use super::solver::{Solver, Z3};
use super::{State, StateZ3Backend};
use crate::convert::ConvertRzILToSymExp;
use crate::error::Result;
use crate::rzil::{error::RzILError, Effect};
use crate::state::Status;

impl<'ctx> Process for StateZ3Backend<Z3<'ctx>> {}

pub trait Process: State + MemoryOps + Solver + ConvertRzILToSymExp {
    fn process(&mut self, root: Effect) -> Result<()> {
        let mut worklist = VecDeque::from([root]);
        let mut status = Status::LoadOp;
        while let Some(op) = worklist.pop_front() {
            match op {
                Effect::Seq { args } => {
                    worklist.append(&mut args.into());
                }
                Effect::Nop | Effect::Empty => {}
                Effect::Set { var } => {
                    let rhs = var.get_arg(0);
                    let lhs = var;
                    self.assign(lhs, rhs)?;
                }
                Effect::Jmp { dst } => {
                    if dst.is_concretized() {
                        self.set_pc(dst.evaluate());
                        status = Status::LoadInst;
                    } else {
                        status = Status::UnconstrainedJump { addr: dst }
                    }
                    break;
                }
                Effect::Goto { label } => {
                    status = Status::Goto { label };
                    break;
                }
                Effect::Blk => {
                    return Err(RzILError::UnimplementedRzILEffect("Blk".to_string()).into())
                }
                Effect::Repeat => {
                    return Err(RzILError::UnimplementedRzILEffect("Repeat".to_string()).into())
                }
                Effect::Branch {
                    condition,
                    then,
                    otherwise,
                } => {
                    if condition.is_concretized() {
                        let next_op = if condition.evaluate_bool() {
                            *then
                        } else {
                            *otherwise
                        };
                        worklist.push_back(next_op);
                    } else {
                        status = Status::UnconstrainedBranch {
                            condition,
                            then,
                            otherwise,
                            post_dominant: worklist.into(),
                        };
                        break;
                    }
                }
                Effect::Store { key, value } => self.store(key, value)?,
            }
        }
        if let Status::LoadOp = status {
            status = Status::LoadInst;
        }
        self.set_status(status);
        Ok(())
    }
}
