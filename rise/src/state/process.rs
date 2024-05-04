use crate::engine::Rise;
use crate::error::Result;
use crate::rzil::{error::RzILError, Effect};
use crate::state::{State, Status};

impl<S: State> Process<S> for Rise<S> {}
pub trait Process<S: State> {
    fn process(&mut self, state: &mut S, op: Effect) -> Result<Status> {
        self.process_op(state, op, 0)
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
                    match self.process_op(state, args.pop().unwrap(), depth + 1)? {
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
                Ok(default)
            }
            Effect::Nop | Effect::Empty => Ok(default),
            Effect::Set { var } => {
                state.convert_set(var.clone())?;
                Ok(default)
            }
            Effect::Jmp { dst } => {
                if dst.is_concretized() {
                    state.set_pc(dst.evaluate());
                    Ok(Status::LoadInst)
                } else {
                    Ok(Status::UnconstrainedJump { addr: dst.clone() })
                }
            }
            Effect::Goto { label } => Ok(Status::Goto {
                label: label.clone(),
            }),
            Effect::Blk => Err(RzILError::UnimplementedRzILEffect("Blk".to_string()).into()),
            Effect::Repeat => Err(RzILError::UnimplementedRzILEffect("Repeat".to_string()).into()),
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
                    match self.process_op(state, next_op, depth + 1)? {
                        Status::Continue => Ok(default),
                        other => Ok(other),
                    }
                } else {
                    /*
                    Status::UnconstrainedBranch {
                        branch: op,
                        following: Vec::new(),
                    }
                    */
                    Ok(default)
                }
            }
            Effect::Store { key, value } => {
                state.store(key.clone(), value.clone())?;
                Ok(default)
            }
        }
    }
}
