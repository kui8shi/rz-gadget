use z3::ast::Ast;
use std::rc::Rc;
use thiserror::Error;
use super::{Pure,Sort, PureCode, RzILError};
use crate::memory::Memory;
type ToZ3Result<T> = std::result::Result<T, ToZ3Error>;

#[derive(Error, Debug)]
pub enum ToZ3Error {
    #[error("{0}")]
    InvalidCode(String),

    #[error("RzIL related error: {0}")]
    RzIL(#[from] RzILError),

    #[error("Z3 ast failed: {0}")]
    Z3(String),
}
//fn effect_to_z3<'a>
impl Pure {
    pub fn to_z3<'a>(&self, ctx: &'a z3::Context, mem: &mut Memory) -> ToZ3Result<z3::ast::Dynamic<'a>>{
        //if op.is_concretized() 
        let size: u32 = self.get_size().try_into().unwrap();
        match self.code {
            PureCode::Var(scope,id) => {
                match self.sort {
                    Sort::Bool => {
                        if self.is_concretized() {
                            Ok(z3::ast::Bool::from_bool(ctx, self.evaluate_bool()).into())
                        } else {
                            Ok(z3::ast::Bool::new_const(ctx, id.to_string()).into())
                        }
                    },
                    Sort::Bitv(_) => {
                        if self.is_concretized() {
                            Ok(z3::ast::BV::from_u64(ctx, self.evaluate(), size).into())
                        } else {
                            Ok(z3::ast::BV::new_const(ctx, id.to_string(), size).into())
                        }
                    },
                }
            }
            PureCode::Ite => {
                let condition = self.args[0].bool_to_z3(ctx, mem)?;
                let then = self.args[1].to_z3(ctx, mem)?;
                let else_ = self.args[2].to_z3(ctx, mem)?;
                Ok(condition.ite(&then, &else_).into())
            }
            PureCode::Let => {
                let var = self.args[0].to_z3(ctx, mem)?;
                let binding = self.args[1].to_z3(ctx, mem)?;
                let body = self.args[2].to_z3(ctx, mem)?;
                Ok(body.substitute(&[(&var, &binding)]).into())
            }
            PureCode::Bool => {
                Ok(z3::ast::Bool::from_bool(ctx, self.evaluate_bool()).into())
            }
            PureCode::BoolInv => {
                let x = self.args[0].bool_to_z3(ctx, mem)?;
                if self.is_concretized() {
                    Ok(z3::ast::Bool::from_bool(ctx, !self.evaluate_bool()).into())
                } else {
                    Ok(x.not().into())
                }
            }
            PureCode::BoolAnd => {
                let x = self.args[0].bool_to_z3(ctx, mem)?;
                let y = self.args[1].bool_to_z3(ctx, mem)?;
                Ok(z3::ast::Bool::and(ctx, &[&x, &y]).into())
            }
            PureCode::BoolOr => {
                let x = self.args[0].bool_to_z3(ctx, mem)?;
                let y = self.args[1].bool_to_z3(ctx, mem)?;
                Ok(z3::ast::Bool::or(ctx, &[&x, &y]).into())
            }
            PureCode::BoolXor => {
                let x = self.args[0].bool_to_z3(ctx, mem)?;
                let y = self.args[1].bool_to_z3(ctx, mem)?;
                Ok(x.xor(&y).into())
            }
            PureCode::Bitv => {
                Ok(z3::ast::BV::from_u64(ctx, self.evaluate(), size).into())
            } 
            PureCode::Msb => {
                let bv = self.args[0].bv_to_z3(ctx, mem)?;
                Ok(bv.extract(size - 1, size - 1).into())
            }
            PureCode::Lsb => {
                let bv = self.args[0].bv_to_z3(ctx, mem)?;
                Ok(bv.extract(0, 0).into())
            }
            PureCode::IsZero => {
                let bv = self.args[0].bv_to_z3(ctx, mem)?;
                let zero = z3::ast::BV::from_u64(ctx, 0, size);
                Ok(bv._eq(&zero).into())
            }
            PureCode::Neg => {
                let bv = self.args[0].bv_to_z3(ctx, mem)?;
                Ok(bv.bvneg().into())
            }
            PureCode::LogNot => {
                let bv = self.args[0].bv_to_z3(ctx, mem)?;
                Ok(z3::ast::Dynamic::from_ast(&!bv))
            }
            PureCode::Add => {
                let x = self.args[0].bv_to_z3(ctx, mem)?;
                let y = self.args[1].bv_to_z3(ctx, mem)?;
                Ok((x.bvadd(&y)).into())
            }
            PureCode::Sub => {
                let x = self.args[0].bv_to_z3(ctx, mem)?;
                let y = self.args[1].bv_to_z3(ctx, mem)?;
                Ok((x.bvsub(&y)).into())
            }
            PureCode::Mul => {
                let x = self.args[0].bv_to_z3(ctx, mem)?;
                let y = self.args[1].bv_to_z3(ctx, mem)?;
                Ok((x.bvmul(&y)).into())
            }
            PureCode::Div => {
                let x = self.args[0].bv_to_z3(ctx, mem)?;
                let y = self.args[1].bv_to_z3(ctx, mem)?;
                Ok(x.bvudiv(&y).into())
            }
            PureCode::Sdiv => {
                let x = self.args[0].bv_to_z3(ctx, mem)?;
                let y = self.args[1].bv_to_z3(ctx, mem)?;
                Ok(x.bvsdiv(&y).into())
            }
            PureCode::Mod => {
                let x = self.args[0].bv_to_z3(ctx, mem)?;
                let y = self.args[1].bv_to_z3(ctx, mem)?;
                Ok(x.bvurem(&y).into())
            }
            PureCode::Smod => {
                let x = self.args[0].bv_to_z3(ctx, mem)?;
                let y = self.args[1].bv_to_z3(ctx, mem)?;
                Ok(x.bvsmod(&y).into()) // ? This could be bvsrem but bvsmod for now.
            }
            PureCode::LogAnd => {
                let x = self.args[0].bv_to_z3(ctx, mem)?;
                let y = self.args[1].bv_to_z3(ctx, mem)?;
                Ok(x.bvand(&y).into())
            }
            PureCode::LogOr => {
                let x = self.args[0].bv_to_z3(ctx, mem)?;
                let y = self.args[1].bv_to_z3(ctx, mem)?;
                Ok(x.bvor(&y).into())
            }
            PureCode::LogXor => {
                let x = self.args[0].bv_to_z3(ctx, mem)?;
                let y = self.args[1].bv_to_z3(ctx, mem)?;
                Ok(x.bvxor(&y).into())
            }
            PureCode::ShiftRight => {
                let fill_bit = &self.args[0];
                let x = self.args[1].bv_to_z3(ctx, mem)?;
                let y = self.args[2].bv_to_z3(ctx, mem)?;
                if fill_bit.is_zero() {
                    Ok(x.bvlshr(&y).into())
                } else {
                    let fill_bit = fill_bit.bv_to_z3(ctx, mem)?;
                    let expanded = z3::ast::BV::concat(&fill_bit, &x);
                    let shifted = expanded.bvashr(&y);
                    Ok(shifted.extract(size - 1, 0).into())
                }
            }
            PureCode::ShiftLeft => {
                let fill_bit = &self.args[0];
                let x = self.args[1].bv_to_z3(ctx, mem)?;
                let y = self.args[2].bv_to_z3(ctx, mem)?;
                if fill_bit.is_zero() {
                    Ok(x.bvshl(&y).into())
                } else {
                    let fill_bit = fill_bit.bv_to_z3(ctx, mem)?;
                    let least_bits = fill_bit.sign_ext(size);
                    let expanded = z3::ast::BV::concat(&x, &least_bits);
                    let shifted = expanded.bvshl(&y);
                    // When fill_bit = 1 and the amount of shift is larger than the size, 
                    // the result won't be correct (least bits should be 1111..., but 11..00..).                             // But ignored the case for now.
                    Ok(shifted.extract(2*size - 1, size).into())
                }
            }
            PureCode::Equal => {
                let x = self.args[0].bv_to_z3(ctx, mem)?;
                let y = self.args[1].bv_to_z3(ctx, mem)?;
                Ok(x._eq(&y).into())
            }
            PureCode::Sle => {
                let x = self.args[0].bv_to_z3(ctx, mem)?;
                let y = self.args[1].bv_to_z3(ctx, mem)?;
                    Ok(x.bvsle(&y).into())
            }
            PureCode::Ule => {
                let x = self.args[0].bv_to_z3(ctx, mem)?;
                let y = self.args[1].bv_to_z3(ctx, mem)?;
                    Ok(x.bvule(&y).into())
            }
            PureCode::Cast(expand) => {
                let fill_bit = self.args[0].clone();
                let value = self.args[1].bv_to_z3(ctx, mem)?;
                if expand {
                    let size_gap = u32::abs_diff(size, value.get_size());
                    let most_bits = if fill_bit.is_concretized() {
                        z3::ast::BV::from_u64(ctx, (fill_bit.evaluate() << size_gap) - 1, size_gap)
                    } else {
                        let fill_bit = self.args[0].bv_to_z3(ctx, mem)?;
                        fill_bit.sign_ext(size_gap)
                    };
                    Ok(most_bits.concat(&value).into())
                } else { // shrink
                    Ok(value.extract(size - 1,0).into())
                }
            }
            PureCode::Append => {
                let high = self.args[0].bv_to_z3(ctx, mem)?;
                let low = self.args[1].bv_to_z3(ctx, mem)?;
                Ok(z3::ast::BV::concat(&high, &low).into())
            }
            PureCode::Load => {
                let value = mem.load(self.args[0], 8);
                value.bv_to_z3(ctx, mem)
            }
            PureCode::LoadW => {
                let value = mem.load(self.args[1], size);
                value.bv_to_z3(ctx, mem)
            }
            _ => {
                unimplemented!()
            }
        }
    }
    
    fn bool_to_z3<'a>(&self, ctx: &'a z3::Context, mem: &mut Memory) -> ToZ3Result<z3::ast::Bool<'a>> { 
        /*
        if !op.is_bool() {
            Err(RzILError::UnexpectedSort(Sort::Bool, op.get_sort()))
        }
        */
        if let Some(ast) = self.to_z3(ctx, mem)?.as_bool() {
            Ok(ast)
        } else {
            Err(ToZ3Error::Z3("failed at the conversion from Dynamic to Bool.".to_owned()))
        }
    }
    fn bv_to_z3<'a>(&self, ctx: &'a z3::Context, mem: &mut Memory) -> ToZ3Result<z3::ast::BV<'a>> { 
        /*
        if !op.is_bv() {
            Err(RzILError::UnexpectedSort(Sort::Bv(0), op.get_sort()))
        }
        */
        if let Some(ast) = self.to_z3(ctx, mem)?.as_bv() {
            Ok(ast)
        } else {
            Err(ToZ3Error::Z3("failed at the conversion from Dynamic to BV.".to_owned()))
        }
    }
}
