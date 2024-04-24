use super::State;
use super::{memory::MemoryWrite, solver::Solver, Status};
use crate::convert::ConvertRzILToSymExp;
use crate::error::Result;
use crate::rzil::{ast::Effect, error::RzILError};
use std::rc::Rc;

impl Process for State {
    fn seek(&mut self, addr: u64) {
        self.set_pc(addr);
    }
}

pub trait Process: ConvertRzILToSymExp + Solver + MemoryWrite {
    fn seek(&mut self, addr: u64);

    fn process(&mut self, op: Effect) -> Result<Status> {
        self.process_op(op, 0)
    }

    fn process_op(&mut self, op: Effect, depth: u32) -> Result<Status> {
        match op {
            Effect::Seq { mut args } => {
                for _ in 0..args.len() {
                    match self.process_op(args.pop().unwrap(), depth + 1)? {
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
                self.convert_set(var.clone())?;
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
                        then
                    } else {
                        otherwise
                    };
                    self.process_op(next_op, depth + 1)?;
                }
                return;
            }
            Effect::Store { key, value } => self.store(key.clone(), value.clone())?,
        }
        if depth > 0 {
            Ok(Status::Continue)
        } else {
            Ok(Status::LoadInst)
        }
    }
}
