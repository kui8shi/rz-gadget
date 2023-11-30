use crate::solver::Solver;
use crate::memory::Memory;
use crate::rzil::{RzIL, Pure, PureCode, RzILError, Sort};
use crate::error::{RiseeError, RiseeResult};
use std::rc::Rc;
use z3::ast::Ast;
//fn effect_to_z3<'a>
pub fn to_z3<'a>(s: &'a Solver, m: &Memory, r: &RzIL, op: Rc<Pure>) -> RiseeResult<z3::ast::Dynamic<'a>> {
    //if op.is_concretized()
    let size: u32 = op.get_size().try_into().unwrap();
    match op.get_code() {
        PureCode::Var(_s, id) => match op.get_sort() {
            Sort::Bool => {
                if op.is_concretized() {
                    Ok(z3::ast::Bool::from_bool(s.get_z3_ctx(), op.evaluate_bool()).into())
                } else {
                    Ok(z3::ast::Bool::new_const(s.get_z3_ctx(), id.to_string()).into())
                }
            }
            Sort::Bitv(_) => {
                if op.is_concretized() {
                    Ok(z3::ast::BV::from_u64(s.get_z3_ctx(), op.evaluate(), size).into())
                } else {
                    Ok(z3::ast::BV::new_const(s.get_z3_ctx(), id.to_string(), size).into())
                }
            }
        },
        PureCode::Ite => {
            let condition = to_z3_bool(s, m, r, op.get_arg(0))?;
            let then = to_z3(s, m, r, op.get_arg(1))?;
            let else_ = to_z3(s, m, r, op.get_arg(2))?;
            Ok(condition.ite(&then, &else_).into())
        }
        PureCode::Let => {
            let var = to_z3(s, m, r, op.get_arg(0))?;
            let binding = to_z3(s, m, r, op.get_arg(1))?;
            let body = to_z3(s, m, r, op.get_arg(2))?;
            Ok(body.substitute(&[(&var, &binding)]).into())
        }
        PureCode::Bool => Ok(z3::ast::Bool::from_bool(s.get_z3_ctx(), op.evaluate_bool()).into()),
        PureCode::BoolInv => {
            let x = to_z3_bool(s, m, r, op.get_arg(0))?;
            if op.is_concretized() {
                Ok(z3::ast::Bool::from_bool(s.get_z3_ctx(), !op.evaluate_bool()).into())
            } else {
                Ok(x.not().into())
            }
        }
        PureCode::BoolAnd => {
            let x = to_z3_bool(s, m, r, op.get_arg(0))?;
            let y = to_z3_bool(s, m, r, op.get_arg(1))?;
            Ok(z3::ast::Bool::and(s.get_z3_ctx(), &[&x, &y]).into())
        }
        PureCode::BoolOr => {
            let x = to_z3_bool(s, m, r, op.get_arg(0))?;
            let y = to_z3_bool(s, m, r, op.get_arg(1))?;
            Ok(z3::ast::Bool::or(s.get_z3_ctx(), &[&x, &y]).into())
        }
        PureCode::BoolXor => {
            let x = to_z3_bool(s, m, r, op.get_arg(0))?;
            let y = to_z3_bool(s, m, r, op.get_arg(1))?;
            Ok(x.xor(&y).into())
        }
        PureCode::Bitv => Ok(z3::ast::BV::from_u64(s.get_z3_ctx(), op.evaluate(), size).into()),
        PureCode::Msb => {
            let bv = to_z3_bv(s, m, r, op.get_arg(0))?;
            Ok(bv.extract(size - 1, size - 1).into())
        }
        PureCode::Lsb => {
            let bv = to_z3_bv(s, m, r, op.get_arg(0))?;
            Ok(bv.extract(0, 0).into())
        }
        PureCode::IsZero => {
            let bv = to_z3_bv(s, m, r, op.get_arg(0))?;
            let zero = z3::ast::BV::from_u64(s.get_z3_ctx(), 0, size);
            Ok(bv._eq(&zero).into())
        }
        PureCode::Neg => {
            let bv = to_z3_bv(s, m, r, op.get_arg(0))?;
            Ok(bv.bvneg().into())
        }
        PureCode::LogNot => {
            let bv = to_z3_bv(s, m, r, op.get_arg(0))?;
            Ok(z3::ast::Dynamic::from_ast(&!bv))
        }
        PureCode::Add => {
            let x = to_z3_bv(s, m, r, op.get_arg(0))?;
            let y = to_z3_bv(s, m, r, op.get_arg(1))?;
            Ok((x.bvadd(&y)).into())
        }
        PureCode::Sub => {
            let x = to_z3_bv(s, m, r, op.get_arg(0))?;
            let y = to_z3_bv(s, m, r, op.get_arg(1))?;
            Ok((x.bvsub(&y)).into())
        }
        PureCode::Mul => {
            let x = to_z3_bv(s, m, r, op.get_arg(0))?;
            let y = to_z3_bv(s, m, r, op.get_arg(1))?;
            Ok((x.bvmul(&y)).into())
        }
        PureCode::Div => {
            let x = to_z3_bv(s, m, r, op.get_arg(0))?;
            let y = to_z3_bv(s, m, r, op.get_arg(1))?;
            Ok(x.bvudiv(&y).into())
        }
        PureCode::Sdiv => {
            let x = to_z3_bv(s, m, r, op.get_arg(0))?;
            let y = to_z3_bv(s, m, r, op.get_arg(1))?;
            Ok(x.bvsdiv(&y).into())
        }
        PureCode::Mod => {
            let x = to_z3_bv(s, m, r, op.get_arg(0))?;
            let y = to_z3_bv(s, m, r, op.get_arg(1))?;
            Ok(x.bvurem(&y).into())
        }
        PureCode::Smod => {
            let x = to_z3_bv(s, m, r, op.get_arg(0))?;
            let y = to_z3_bv(s, m, r, op.get_arg(1))?;
            Ok(x.bvsmod(&y).into()) // ? This could be bvsrem but bvsmod for now.
        }
        PureCode::LogAnd => {
            let x = to_z3_bv(s, m, r, op.get_arg(0))?;
            let y = to_z3_bv(s, m, r, op.get_arg(1))?;
            Ok(x.bvand(&y).into())
        }
        PureCode::LogOr => {
            let x = to_z3_bv(s, m, r, op.get_arg(0))?;
            let y = to_z3_bv(s, m, r, op.get_arg(1))?;
            Ok(x.bvor(&y).into())
        }
        PureCode::LogXor => {
            let x = to_z3_bv(s, m, r, op.get_arg(0))?;
            let y = to_z3_bv(s, m, r, op.get_arg(1))?;
            Ok(x.bvxor(&y).into())
        }
        PureCode::ShiftRight => {
            let fill_bit = op.get_arg(0);
            let x = to_z3_bv(s, m, r, op.get_arg(1))?;
            let y = to_z3_bv(s, m, r, op.get_arg(2))?;
            if fill_bit.is_zero() {
                Ok(x.bvlshr(&y).into())
            } else {
                let fill_bit = to_z3_bv(s, m, r, fill_bit.clone())?;
                let expanded = z3::ast::BV::concat(&fill_bit, &x);
                let shifted = expanded.bvashr(&y);
                Ok(shifted.extract(size - 1, 0).into())
            }
        }
        PureCode::ShiftLeft => {
            let fill_bit = op.get_arg(0);
            let x = to_z3_bv(s, m, r, op.get_arg(1))?;
            let y = to_z3_bv(s, m, r, op.get_arg(2))?;
            if fill_bit.is_zero() {
                Ok(x.bvshl(&y).into())
            } else {
                let fill_bit = to_z3_bv(s, m, r, fill_bit)?;
                let least_bits = fill_bit.sign_ext(size);
                let expanded = z3::ast::BV::concat(&x, &least_bits);
                let shifted = expanded.bvshl(&y);
                // When fill_bit = 1 and the amount of shift is larger than the size,
                // the result won't be correct (least bits should be 1111..., but 11..00..).                             // But ignored the case for now.
                Ok(shifted.extract(2 * size - 1, size).into())
            }
        }
        PureCode::Equal => {
            let x = to_z3_bv(s, m, r, op.get_arg(0))?;
            let y = to_z3_bv(s, m, r, op.get_arg(1))?;
            Ok(x._eq(&y).into())
        }
        PureCode::Sle => {
            let x = to_z3_bv(s, m, r, op.get_arg(0))?;
            let y = to_z3_bv(s, m, r, op.get_arg(1))?;
            Ok(x.bvsle(&y).into())
        }
        PureCode::Ule => {
            let x = to_z3_bv(s, m, r, op.get_arg(0))?;
            let y = to_z3_bv(s, m, r, op.get_arg(1))?;
            Ok(x.bvule(&y).into())
        }
        PureCode::Cast(expand) => {
            let fill_bit = op.get_arg(0).clone();
            let value = to_z3_bv(s, m, r, op.get_arg(1))?;
            if expand {
                let size_gap = u32::abs_diff(size, value.get_size());
                let most_bits = if fill_bit.is_concretized() {
                    z3::ast::BV::from_u64(
                        s.get_z3_ctx(),
                        (fill_bit.evaluate() << size_gap) - 1,
                        size_gap,
                    )
                } else {
                    let fill_bit = to_z3_bv(s, m, r, op.get_arg(0))?;
                    fill_bit.sign_ext(size_gap)
                };
                Ok(most_bits.concat(&value).into())
            } else {
                // shrink
                Ok(value.extract(size - 1, 0).into())
            }
        }
        PureCode::Append => {
            let high = to_z3_bv(s, m, r, op.get_arg(0))?;
            let low = to_z3_bv(s, m, r, op.get_arg(1))?;
            Ok(z3::ast::BV::concat(&high, &low).into())
        }
        PureCode::Load => {
            let value = m.load(s, r, op.get_arg(0), 8)?;
            Ok(to_z3_bv(s, m, r, value)?.into())
        }
        PureCode::LoadW => {
            let value = m.load(s, r, op.get_arg(1), size)?;
            Ok(to_z3_bv(s, m, r, value)?.into())
        }
        _ => {
            unimplemented!()
        }
    }
}

fn to_z3_bool<'a>(s: &'a Solver, m: &Memory, r: &RzIL, op: Rc<Pure>) -> RiseeResult<z3::ast::Bool<'a>> {
    if !op.is_bool() {
        return Err(RzILError::UnexpectedSort(Sort::Bool, op.get_sort()).into());
    }
    if let Some(ast) = to_z3(s, m, r, op)?.as_bool() {
        Ok(ast)
    } else {
        Err(RiseeError::ToZ3("Bool rzil was somehow converted to non-bool z3 ast".to_string()))
    }
}
fn to_z3_bv<'a>(s: &'a Solver, m: &Memory, r: &RzIL, op: Rc<Pure>) -> RiseeResult<z3::ast::BV<'a>> {
    if !op.is_bv() {
        return Err(RzILError::UnexpectedSort(Sort::Bitv(0), op.get_sort()).into());
    }
    if let Some(ast) = to_z3(s, m, r, op)?.as_bv() {
        Ok(ast)
    } else {
        Err(RiseeError::ToZ3("Bitv rzil was somehow converted to non-bitv z3 ast".to_string()))
    }
}
