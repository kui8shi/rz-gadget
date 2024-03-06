use crate::rzil::{builder::RzILBuilder, PureRef, Effect, error::RzILError};
use crate::memory::Memory;
use crate::solver::Solver;
use crate::context::Context;
use crate::error::RiseResult;
use std::rc::Rc;
use std::sync::mpsc;
//use crate::engine::Rise;

/*
 *Nop,
    Set {
        dst: PureRef,
        src: PureRef,
    },
    Jmp {
        dst: PureRef,
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
        condition: PureRef,
        then: Rc<Effect>,
        otherwise: Rc<Effect>,
    },
    Store {
        key: PureRef,
        value: PureRef,
    },
    StoreW {
        key: PureRef,
        value: PureRef,
    },
    Empty,
}
*/
// jump == branch(true, exp, _)
enum Status {
    Continue,
    Jump(u64),
    SymbolicJump(PureRef),
    Goto(String),
    Branch(PureRef, Rc<Effect>, Rc<Effect>),
}

fn effect<S: Solver>(ctx: &mut Context<S>, rzil: &RzILBuilder, op: Rc<Effect>) -> RiseResult<Status> {
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
                let status = effect(ctx, rzil, arg.clone())?;
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
            Ok(Status::Branch(condition.clone(), then.clone(), otherwise.clone()))
        }
        Effect::Store { key, value } => {
            ctx.store(rzil, key.clone(), value.clone())?;
            Ok(Status::Continue)
        }
        Effect::Empty => Ok(Status::Continue)
    }
}
