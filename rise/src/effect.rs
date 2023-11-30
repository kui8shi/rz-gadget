use crate::rzil::{RzIL, Pure, Effect, RzILError};
use crate::memory::Memory;
use crate::context::Context;
use crate::error::RiseeResult;
use std::rc::Rc;
use std::sync::mpsc;
//use crate::engine::Risee;

/*
 *Nop,
    Set {
        dst: Rc<Pure>,
        src: Rc<Pure>,
    },
    Jmp {
        dst: Rc<Pure>,
    },
    Goto {
        label: String,
    },
    Seq {
        args: Vec<Rc<Effect>>,
    },
    Blk,
    Repeat,
    Branch {
        condition: Rc<Pure>,
        then: Rc<Effect>,
        otherwise: Rc<Effect>,
    },
    Store {
        key: Rc<Pure>,
        value: Rc<Pure>,
    },
    StoreW {
        key: Rc<Pure>,
        value: Rc<Pure>,
    },
    Empty,
}
*/
// jump == branch(true, exp, _)
enum Status {
    Continue,
    Jump(u64),
    SymbolicJump(Rc<Pure>),
    Goto(String),
    Branch(Rc<Pure>, Rc<Effect>, Rc<Effect>),
}
fn effect(ctx: &mut Context, rzil: &RzIL, op: Rc<Effect>, sender: &mpsc::Sender<Status>, receiver: &mpsc::Receiver<bool>) -> RiseeResult<Status> {
    let op = op.as_ref();
    match op {
        Effect::Nop => Ok(Status::Continue),
        Effect::Set { dst, src } => Ok(Status::Continue),
        Effect::Jmp { dst } => {
            if dst.is_concretized() {
                Ok(Status::Jump(dst.evaluate()))
            } else {
                Ok(Status::SymbolicJump(dst.clone()))
            }
        },
        Effect::Goto { label } => Ok(Status::Goto(label.clone())),
        Effect::Seq { args } => {
            for arg in args {
                let status = effect(ctx, rzil, arg.clone(), sender)?;
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
        Effect::Branch { condition, then, otherwise } => {
            sender.send(Status::Branch(condition.clone(), then.clone(), otherwise.clone()));
            receiver.recv()?
            Ok(Status::Branch(condition.clone(), then.clone(), otherwise.clone()))
        }
        Effect::Store { key, value } => {
            ctx.store(rzil, key.clone(), value.clone());
            Ok(Status::Continue)
        }
        Effect::StoreW { key, value } => {
            ctx.store(rzil, key.clone(), value.clone());
            Ok(Status::Continue)
        }
        Effect::Empty => Ok(Status::Continue)
    }
}
