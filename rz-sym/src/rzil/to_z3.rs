use z3::{
    ast::{Ast,Bool,BV,Dynamic},
    Config,
    Context
};
use std::rc::Rc;

use crate::rzil::RzIL;
type RzILResult<T> = std::result::Result<T, RzILError>;

pub struct RzILToZ3 {
    ctx: z3::Context,
}

impl RzILToZ3 {
    pub fn new() -> Self {
        let cfg = Config::new();
        RzILToZ3 {
            ctx: Context::new(&cfg),
        }
    }
    
    pub fn convert_pure<'a>(&'a self, op: Rc<RzIL>) -> RzILResult<Dynamic<'a>>{
        if op.is_concretized() 
        match op.code {
            PureCode::Var(scope,name) => {
                match op.sort {
                    Sort::Bool => {
                        if op.is_concretized() {
                            Ok(Bool::from_bool(&self.ctx, op.evaluate_bool()).into())
                        } else {
                            Ok(Bool::new_const(&self.ctx, name).into())
                        }
                    },
                    Sort::Bv(size) => {
                        if op.is_concretized() {
                            Ok(BV::from_u64(&self.ctx, op.evaluate(), size).into())
                        } else {
                            Ok(BV::new_const(&self.ctx,name,size).into())
                        }
                    }
                }
            }
            PureCode::Ite => {
                let condition = self.convert_bool(op.args[0])?;
                let then = self.convert_pure(op.args[1])?;
                let else_ = self.convert_pure(op.args[2])?;
                Ok(condition.ite(&then, &else_).into())
            }
            PureCode::Let => {
                let var = self.convert_pure(op.args[0])?;
                let binding = self.convert_pure(op.args[1])?;
                let body = self.convert_pure(op.args[2])?;
                Ok(body.substitute(&[(&var, &binding)]).into())
            }
            PureCode::Bool => {
                Ok(Bool::from_bool(&self.ctx, op.evaluate_bool()).into())
            }
            PureCode::BoolInv => {
                let x = self.convert_bool(op.args[0])?;
                if op.is_concretized() {
                    Ok(Bool::from_bool(&self.ctx, !op.evaluate_bool()).into())
                } else {
                    Ok(x.not().into())
                }
            }
            PureCode::BoolAnd => {
                let x = self.convert_bool(op.args[0])?;
                let y = self.convert_bool(op.args[1])?;
                Ok(Bool::and(&self.ctx, &[&x,&y]).into())
            }
            PureCode::BoolOr => {
                let x = self.convert_bool(op.args[0])?;
                let y = self.convert_bool(op.args[1])?;
                Ok(Bool::or(&self.ctx, &[&x,&y]).into())
            }
            PureCode::BoolXor => {
                let x = self.convert_bool(op.args[0])?;
                let y = self.convert_bool(op.args[1])?;
                Ok(x.xor(&y).into())
            }
            PureCode::Bitv => {
                Ok(BV::from_u64(&self.ctx, op.evaluate(), op.get_size()).into())
            } 
            PureCode::Msb => {
                let bv = self.convert_bv(op.args[0])?;
                Ok(bv.extract(op.get_size()-1, op.get_size()-1).into())
            }
            PureCode::Lsb => {
                let bv = self.convert_bv(op.args[0])?;
                Ok(bv.extract(0, 0).into())
            }
            PureCode::IsZero => {
                let bv = self.convert_bv(op.args[0])?;
                let zero = BV::from_u64(&self.ctx, 0, op.get_size());
                Ok(bv._eq(&zero).into())
            }
            PureCode::Neg => {
                let bv = self.convert_bv(op.args[0])?;
                Ok(bv.bvneg().into())
            }
            PureCode::LogNot => {
                let bv = self.convert_bv(op.args[0])?;
                Ok(Dynamic::from_ast(&!bv))
            }
            PureCode::Add => {
                let x = self.convert_bv(op.args[0])?;
                let y = self.convert_bv(op.args[1])?;
                Ok((x.bvadd(&y)).into())
            }
            PureCode::Sub => {
                let x = self.convert_bv(op.args[0])?;
                let y = self.convert_bv(op.args[1])?;
                Ok((x.bvsub(&y)).into())
            }
            PureCode::Mul => {
                let x = self.convert_bv(op.args[0])?;
                let y = self.convert_bv(op.args[1])?;
                Ok((x.bvmul(&y)).into())
            }
            PureCode::Div => {
                let x = self.convert_bv(op.args[0])?;
                let y = self.convert_bv(op.args[1])?;
                Ok(x.bvudiv(&y).into())
            }
            PureCode::Sdiv => {
                let x = self.convert_bv(op.args[0])?;
                let y = self.convert_bv(op.args[1])?;
                Ok(x.bvsdiv(&y).into())
            }
            PureCode::Mod => {
                let x = self.convert_bv(op.args[0])?;
                let y = self.convert_bv(op.args[1])?;
                Ok(x.bvurem(&y).into())
            }
            PureCode::Smod => {
                let x = self.convert_bv(op.args[0])?;
                let y = self.convert_bv(op.args[1])?;
                Ok(x.bvsmod(&y).into()) // ? The right one may be bvsrem but bvsmod.
            }
            PureCode::LogAnd => {
                let x = self.convert_bv(op.args[0])?;
                let y = self.convert_bv(op.args[1])?;
                Ok(x.bvand(&y).into())
            }
            PureCode::LogOr => {
                let x = self.convert_bv(op.args[0])?;
                let y = self.convert_bv(op.args[1])?;
                Ok(x.bvor(&y).into())
            }
            PureCode::LogXor => {
                let x = self.convert_bv(op.args[0])?;
                let y = self.convert_bv(op.args[1])?;
                Ok(x.bvxor(&y).into())
            }
            PureCode::ShiftRight => {
                let fill_bit = op.args[0];
                let x = self.convert_bv(op.args[1])?;
                let y = self.convert_bv(op.args[2])?;
                if fill_bit.is_zero() {
                    Ok(x.bvlshr(&y).into())
                } else {
                    let fill_bit = self.convert_bv(fill_bit)?;
                    let expanded = BV::concat(&fill_bit, &x);
                    let shifted = expanded.bvashr(&y);
                    Ok(shifted.xtract(op.get_size() - 1, 0).into())
                }
            }
            PureCode::ShiftLeft(fill_bit, x, y) => {
                let fill_bit = op.args[0];
                let x = self.convert_bv(op.args[1])?;
                let y = self.convert_bv(op.args[2])?;
                if fill_bit.is_zero() {
                    Ok(x.bvshl(&y).into())
                } else {
                    let size = op.get_size();
                    let fill_bit = self.convert_bv(fill_bit)?;
                    let least_bits = BV::repeat(&fill_bit, size);
                    let expanded = BV::concat(&x, &least_bits);
                    let shifted = expanded.bvshl(&y);
                    // When fill_bit = 1 and the amount of shift is larger than the size, 
                    // the result won't be correct (least bits should be 1111..., but 11..00..).                    // But ignored the case for now.
                    Ok(shifted.extract(2*size - 1, size).into())
                }
            }
            PureCode::Equal => {
                let x = self.convert_bv(op.args[0])?;
                let y = self.convert_bv(op.args[1])?;
                Ok(x._eq(&y).into())
            }
            PureCode::Sle => {
                let x = self.convert_bv(op.args[0])?;
                let y = self.convert_bv(op.args[1])?;
                    Ok(x.bvsle(&y).into())
            }
            PureCode::Ule => {
                let x = self.convert_bv(op.args[0])?;
                let y = self.convert_bv(op.args[1])?;
                    Ok(x.bvule(&y).into())
            }
            PureCode::Cast => {
                let size_before = op.args[0].get_size();
                let size_after = op.get_size();
                let value = self.convert_bv(op.args[0])?;
                if size_after >= size_before {
                    let fill = op.args[1];
                    if fill.is_concretized() {
                        if fill.evaluate_bool() {
                            // fill significant bits with 1
                            Ok(BV::concat(
                                    &BV::from_i64(&self.ctx, -1, size_after - size_before),
                                    &value).into())
                        } else {
                            // fill significant bits with 0
                            Ok(value.zero_ext(size_after - size_before).into())
                        }
                    } else {
                        // fill significant bits with "fill"
                        let fill = self.convert_bv(fill)?;
                        Ok(&fill.ite(
                                    &BV::concat(
                                        &BV::from_i64(&self.ctx, -1, size_after - size_before), 
                                        &value),
                                    &value.zero_ext(size_after - size_before)))
                    }
                } else {
                    // extract value
                    Ok(value.extract(size_after-1, 0).into())
                }
            }
            PureCode::Append => {
                let high = self.convert_bv(op.arg[0])?;
                let low = self.convert_bv(op.args[1])?;
                Ok(BV::concat(&high, &low).into())
            }
            /*
            PureCode::Load(key) => {
                
            }
            PureCode::Loadw(key) => {

            }
            */
            _ => {
                Err(RzILError::UnkownRzIL)
            }
        }
    }
    fn convert_bool<'a>(&'a self, op: Rc<Pure>) -> RzILResult<Bool<'a>> { 
        /*
        if !op.is_bool() {
            Err(RzILError::UnexpectedSort(Sort::Bool, op.get_sort()))
        }
        */
        if let Some(ast) = self.convert_pure(op)?.as_bool() {
            Ok(ast)
        }else {
            Err(RzILError::Empty)
        }
    }
    fn convert_bv<'a>(&'a self, op: Rc<Pure>) -> RzILResult<BV<'a>> { 
        /*
        if !op.is_bv() {
            Err(RzILError::UnexpectedSort(Sort::Bv(0), op.get_sort()))
        }
        */
        if let Some(ast) = self.convert_pure(op)?.as_bv() {
            Ok(ast)
        }else {
            Err(RzILError::Empty)
        }
    }
}
