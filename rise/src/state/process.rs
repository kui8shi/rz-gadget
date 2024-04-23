use super::{memory::MemoryWrite, solver::Solver, Status};
use super::State;
use crate::convert::ConvertRzILToSymExp;
use crate::error::Result;
use crate::rzil::{ast::Effect, error::RzILError};
use std::rc::Rc;

impl Process for State {
    fn process(&mut self, ops: Vec<Rc<Effect>>) -> Result<()> {
        for op in ops {
            self.status = self.process_op(op)?;
            if !matches!(self.get_status(), Status::Continue) {
                break;
            }
        }
        Ok(())
    }
}

pub trait Process: ConvertRzILToSymExp + Solver + MemoryWrite {
    fn process(&mut self, ops: Vec<Rc<Effect>>) -> Result<()>;

    fn process_ops(&mut self, ops: Vec<Rc<Effect>>) -> Result<()> {
        for op in ops {
            self.process_op(op)?;
        }
        Ok(())
    }

    fn process_op(&mut self, op: Rc<Effect>) -> Result<Status> {
        dbg!(&op);
        let op = op.as_ref();
        match op {
            Effect::Nop => Ok(Status::Continue),
            Effect::Set { var } => {
                self.convert_set(var.clone())?;
                Ok(Status::Continue)
            }
            Effect::Jmp { dst } => {
                Ok(Status::Jump(dst.clone()))
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
