use super::RiseContext;
use super::{memory::MemoryWrite, solver::Solver, Status};
use crate::convert::ConvertRzIL;
use crate::error::Result;
use crate::rzil::{ast::Effect, error::RzILError};
use std::rc::Rc;

impl Process for RiseContext {}

pub trait Process: ConvertRzIL + Solver + MemoryWrite {
    fn process(&mut self, ops: Vec<Rc<Effect>>) -> Result<()> {
        for op in ops {
            self.process_op(op)?;
        }
        Ok(())
    }

    fn process_op(&mut self, op: Rc<Effect>) -> Result<Status> {
        let op = op.as_ref();
        match op {
            Effect::Nop => Ok(Status::Continue),
            Effect::Set { var } => {
                self.convert_set(var.clone())?;
                Ok(Status::Continue)
            }
            Effect::Jmp { dst } => {
                if dst.is_concretized() {
                    Ok(Status::DirectJump(dst.evaluate()))
                } else {
                    Ok(Status::SymbolicJump(dst.clone()))
                }
            }
            Effect::Goto { label } => Ok(Status::Goto(label.clone())),
            Effect::Seq { args } => {
                for arg in args {
                    let status = self.process_op(arg.clone())?;
                    match status {
                        Status::Continue => continue,
                        _ => {
                            return Ok(status);
                        }
                    }
                }
                Ok(Status::Continue)
            }
            Effect::Blk => Err(RzILError::UnimplementedRzILEffect("Blk".to_string()).into()),
            Effect::Repeat => Err(RzILError::UnimplementedRzILEffect("Repeat".to_string()).into()),
            Effect::Branch {
                condition,
                then,
                otherwise,
            } => Ok(Status::Branch(
                condition.clone(),
                then.clone(),
                otherwise.clone(),
            )),
            Effect::Store { key, value } => {
                self.store(key.clone(), value.clone())?;
                Ok(Status::Continue)
            }
            Effect::Empty => Ok(Status::Continue),
        }
    }
}
