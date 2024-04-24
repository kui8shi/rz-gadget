use super::{memory::MemoryOps, solver::Solver, Status};
use super::{State, State_Z3Backend};
use crate::convert::ConvertRzILToSymExp;
use crate::error::Result;
use crate::explorer::PathExplorer;
use crate::rzil::{ast::Effect, error::RzILError};

pub trait Process<S: State>: PathExplorer<S> {
    fn process(&mut self, state: &mut S, op: Effect) -> Result<Status> {
        self.process_op(op, 0)
    }

    fn process_op(&mut self, state: &mut S, op: Effect, depth: u32) -> Result<Status> {
        let default = if depth > 0 {
            Status::Continue
        } else {
            Status::LoadInst
        };
        match op {
            Effect::Seq { mut args } => {
                for _ in 0..args.len() {
                    match self.process_op(args.pop().unwrap(), state, depth + 1)? {
                        Status::Continue => continue,
                        Status::UnconstrainedBranch {
                            branch,
                            mut following,
                        } => {
                            following.extend(args);
                            return Ok(Status::UnconstrainedBranch { branch, following });
                        }
                        other => return Ok(other),
                    }
                }
            }
            Effect::Nop => (),
            Effect::Empty => (),
            Effect::Set { var } => {
                state.convert_set(var.clone())?;
            }
            Effect::Jmp { dst } => {
                if dst.is_concretized() {
                    self.seek(dst.evaluate());
                    return Ok(Status::LoadInst);
                } else {
                    return Ok(Status::UnconstrainedJump { addr: dst.clone() });
                }
            }
            Effect::Goto { label } => {
                return Ok(Status::Goto {
                    label: label.clone(),
                })
            }
            Effect::Blk => return Err(RzILError::UnimplementedRzILEffect("Blk".to_string()).into()),
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
                    match self.process_op(next_op, depth + 1)? {
                        Status::Continue => Ok(default),
                        other => return Ok(other),
                    }
                } else {
                    Status::UnconstrainedBranch {
                        branch: op,
                        following: Vec::new(),
                    }
                }
            }
            Effect::Store { key, value } => {
                state.store(key.clone(), value.clone())?;
                Ok(default)
            }
        }
    }
}
