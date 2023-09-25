use crate::context::context::Context;
use crate::smt::theories::{Core, BitVec, ArrayEx};
use crate::explorer::explorer::{Control, PathExplorer};

use super::{
    RzIL,
    PureCode,
    EffectCode,
    Scope,
    Sort,
    Code,
    RzILError,
};
use std::rc::Rc;

type RzILResult<T> = std::result::Result<T, RzILError>;

/// convert RzIL to SMTLib format and evaluate it by ctx.eval
/// * 'ctx' - A mutable reference to a struct with Context trait
/// * 'exp' - A mutable reference to a struct with Explorer trait
/// return value: if RzIL is Pure, Some(the evaluated expression's reference), else None.
pub fn to_smt<Ctx, Explorer>(ctx: &mut Ctx, exp: &mut Explorer, op: Rc<RzIL>) -> Option<<Ctx as Core>::Ast>
    where Ctx: Context,
          Explorer: PathExplorer<Ctx = Ctx> {
    
    match &op.code {
        Code::Pure(code) => {
            if op.is_concretized() {
                match op.get_sort() {
                    Sort::Bool => {
                        return Some(ctx.const_bool(op.evaluate_bool()));
                    }
                    Sort::Bv(size) => {
                        return Some(ctx.const_bv(op.evaluate(), size));
                    }
                    Sort::None => unreachable!()
                }
            }
            let ops: Vec<<Ctx as Core>::Ast> 
                = op.args.iter().map(|arg| to_smt(ctx, exp, arg.clone()).unwrap()).collect();
            match code {
                PureCode::Var(scope, ref name) => match scope {
                    Scope::Global => Some(ctx.reg_read(name)),
                    Scope::Local => ctx.get_named_const(name),
                    Scope::LocalPure => unimplemented!(),
                },
                PureCode::Ite => {
                    Some(ctx.ite([&ops[0], &ops[1], &ops[2]]))
                }
                PureCode::Bool => {
                    Some(ctx.const_bool(op.evaluate_bool()))
                }
                PureCode::BoolInv => {
                    Some(ctx.not(&ops[0]))
                }
                PureCode::BoolAnd => {
                    Some(ctx.and([&ops[0], &ops[1]]))
                }
                PureCode::BoolOr => {
                    Some(ctx.or([&ops[0], &ops[1]]))
                }
                PureCode::BoolXor => {
                    Some(ctx.xor([&ops[0], &ops[1]]))
                }
                PureCode::Bitv => {
                    Some(ctx.const_bv(op.evaluate(), op.get_size()))
                }
                PureCode::Msb => {
                    let size = op.get_size();
                    Some(ctx.extract(&ops[0], size - 1, size - 1))
                }
                PureCode::Lsb => {
                    Some(ctx.extract(&ops[0], 0, 0))
                }
                PureCode::IsZero => {
                    let zero = ctx.const_bv(0, op.get_size());
                    Some(ctx.bveq([&ops[0], &zero]))
                }
                PureCode::Neg => {
                    Some(ctx.bvneg(&ops[0]))
                }
                PureCode::LogNot => {
                    Some(ctx.bvnot(&ops[0]))
                }
                PureCode::Add => {
                    Some(ctx.bvadd([&ops[0], &ops[1]]))
                }
                PureCode::Sub => {
                    Some(ctx.bvsub([&ops[0], &ops[1]]))
                }
                PureCode::Mul => {
                    Some(ctx.bvmul([&ops[0], &ops[1]]))
                }
                PureCode::Div => {
                    Some(ctx.bvudiv([&ops[0], &ops[1]]))
                }
                PureCode::Sdiv => {
                    Some(ctx.bvsdiv([&ops[0], &ops[1]]))
                }
                PureCode::Mod => {
                    Some(ctx.bvurem([&ops[0], &ops[1]]))
                }
                PureCode::Smod => {
                    Some(ctx.bvsrem([&ops[0], &ops[1]]))
                }
                PureCode::LogAnd => {
                    Some(ctx.bvand([&ops[0], &ops[1]]))
                }
                PureCode::LogOr => {
                    Some(ctx.bvor([&ops[0], &ops[1]]))
                }
                PureCode::LogXor => {
                    Some(ctx.bvxor([&ops[0], &ops[1]]))
                }
                PureCode::ShiftRight => {
                    //  [RSHIFT] (shiftr s x m) shifts x right by m bits filling with s.
                    let fill_bit = op.args[0].clone();
                    if fill_bit.is_zero() {
                        Some(ctx.bvlshr([&ops[1], &ops[2]]))
                    } else {
                        let expanded = ctx.concat([&ops[0], &ops[1]]); // concat fill_bit x
                        let shifted = ctx.bvashr([&expanded, &ops[2]]);
                        Some(ctx.extract(&shifted, op.get_size() - 1, 0))
                    }
                }
                PureCode::ShiftLeft => {
                    //  [LSHIFT] (shiftl s x m) shifts x left by m bits filling with s.
                    let fill_bit = op.args[0].clone();
                    if fill_bit.is_zero() {
                        Some(ctx.bvshl([&ops[1], &ops[2]]))
                    } else {
                        let size = op.get_size();
                        let fill_bit = &ops[0];
                        let least_bits = ctx.repeat(fill_bit, op.get_size());
                        let expanded = ctx.concat([&ops[1], &least_bits]);
                        let shifted = ctx.bvshl([&expanded, &ops[2]]);
                        // When fill_bit = 1 and the amount of shift is larger than the size, the result
                        // won't be correct (least bits should be 1111..., but 11..00..). Ignore the case for now.
                        Some(ctx.extract(&shifted, 2*size - 1, size))
                    }
                }
                PureCode::Equal => {
                    Some(ctx.bveq([&ops[0], &ops[1]]))
                }
                PureCode::Sle => {
                    Some(ctx.bvsle([&ops[0], &ops[1]]))
                }
                PureCode::Ule => {
                    Some(ctx.bvule([&ops[0], &ops[1]]))
                }
                PureCode::Cast(expand) => {
                    let fill_bit = op.args[0].clone();
                    let value = op.args[1].clone();
                    if *expand {
                        let size_gap = usize::abs_diff(op.get_size(), value.get_size());
                        let most_bits = if fill_bit.is_concretized() {
                            ctx.const_bv(fill_bit.evaluate() << size_gap - 1, size_gap)
                        } else {
                            ctx.repeat(&ops[0], size_gap)
                        };
                        Some(ctx.concat([&most_bits, &ops[1]]))
                    } else { // shrink
                        Some(ctx.extract(&ops[0], op.get_size() - 1, 0))
                    }
                }
                PureCode::Append => {
                    Some(ctx.concat([&ops[0], &ops[1]]))
                }
                PureCode::Load => {
                    Some(ctx.select(&ops[0], 8))
                }
                PureCode::LoadW => {
                    Some(ctx.select(&ops[0], op.get_size()))
                }
                _ => unimplemented!(),
            }
        }
        Code::Effect(code) => {
            match code {
                EffectCode::Nop => None,
                EffectCode::Set => {
                    let children : Vec<<Ctx as Core>::Ast> 
                        = op.args.iter().map(|arg| to_smt(ctx, exp, arg.clone()).unwrap()).collect();
                    let dst = op.args[0].clone();
                    let (scope, name) = dst.get_var_info().unwrap();
                    match scope {
                        Scope::Global => {
                            ctx.reg_write(name, &children[1]);
                            None
                        }
                        Scope::Local => {
                            ctx.set_named_const(name, &children[1]);
                            None
                        }
                        Scope::LocalPure => unreachable!()
                    }
                }
                EffectCode::Jmp => {
                    let children : Vec<<Ctx as Core>::Ast> 
                        = op.args.iter().map(|arg| to_smt(ctx, exp, arg.clone()).unwrap()).collect();
                    let address = op.args[0].clone();
                    if address.is_concretized() {
                        ctx.set_pc(address.evaluate())
                    } else {
                        exp.control_flow(ctx, &children[0])
                    };
                    None
                }
                EffectCode::Goto => {
                    unimplemented!()
                }
                EffectCode::Seq => {
                    for arg in &op.args {
                        to_smt(ctx, exp, arg.clone());
                    }
                    None
                }
                EffectCode::Blk => {
                    unimplemented!()
                }
                EffectCode::Repeat => {
                    unimplemented!()
                }
                EffectCode::Branch => {
                    let condition = to_smt(ctx, exp, op.args[0].clone()).unwrap();
                    if op.is_concretized() {
                        if op.args[0].evaluate_bool() {
                            to_smt(ctx, exp, op.args[1].clone())
                        } else {
                            to_smt(ctx, exp, op.args[2].clone())
                        }
                    } else {
                        match exp.register_branch(ctx, &condition) {
                            Control::Continue => None,
                            Control::TerminatePath => None,
                            Control::ExploreTrue => to_smt(ctx, exp, op.args[1].clone()),
                            Control::ExploreFalse => to_smt(ctx, exp, op.args[2].clone()),
                            Control::Skip => None,
                            Control::Halt => None,
                            Control::Break => None,
                        }
                    }
                    
                }
                EffectCode::Store => {
                    let children : Vec<<Ctx as Core>::Ast> 
                        = op.args.iter().map(|arg| to_smt(ctx, exp, arg.clone()).unwrap()).collect();
                    ctx.store(children[0],&children[1], op.args[1].get_size());
                    None
                }
                EffectCode::StoreW => {
                    let children : Vec<<Ctx as Core>::Ast> 
                        = op.args.iter().map(|arg| to_smt(ctx, exp, arg.clone()).unwrap()).collect();
                    ctx.store(&children[0], &children[1], op.args[1].get_size());
                    None
                }
                EffectCode::Empty => {
                    None
                }
            }
        }
    }
}
