use super::{
    Effect,
    Pure,
    EffectCode,
    PureCode,
    Scope,
    Sort,
    RzILError,
};
use std::rc::Rc;

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
    
    pub fn convert_effect<T: z3::ast::Ast>(&self,op: Rc<Effect>) -> T {

    }
    pub fn convert_pure<'a>(&'a self, op: Rc<Pure>) -> RzILResult<Dynamic<'a>>{
        match op.code {
            PureCode::Var(scope,name) => {
                match op.sort {
                    Sort::Bool => {
                        if op.is_concretized() {
                            Ok(Dynamic::from_ast(&Bool::from_bool(&self.ctx, op.evaluate_bool())))
                        } else {
                            Ok(Dynamic::from_ast(&Bool::new_const(&self.ctx, name)))
                        }
                    },
                    Sort::Bv(size) => {
                        if op.is_concretized() {
                            Ok(Dynamic::from_ast(&BV::from_u64(&self.ctx, op.evaluate(), size)))
                        } else {
                            Ok(Dynamic::from_ast(&BV::new_const(&self.ctx,name,size)))
                        }
                    }
                }
            }
            PureCode::Ite(condition, then, else_) => {
                let condition = self.convert_bool(condition)?;
                let then = self.convert_pure(then)?;
                let else_ = self.convert_pure(else_)?;
                Ok(Dynamic::from_ast(&condition.ite(&then, &else_)))
            }
            PureCode::Let(var, binding, body) => {
                let var = self.convert_pure(var)?;
                let binding = self.convert_pure(binding)?;
                let body = self.convert_pure(body)?;
                Ok(Dynamic::from_ast(&body.substitute(&[(&var, &binding)])))
            }
            PureCode::Bool => {
                Ok(Dynamic::from_ast(&Bool::from_bool(&self.ctx, op.evaluate_bool())))
            }
            PureCode::BoolInv(x) => {
                let x = self.convert_bool(x)?;
                if op.is_concretized() {
                    Ok(Dynamic::from_ast(&Bool::from_bool(&self.ctx, !op.evaluate_bool())))
                } else {
                    Ok(Dynamic::from_ast(&x.not()))
                }
            }
            PureCode::BoolAnd(x, y) => {
                let x = self.convert_bool(x)?;
                let y = self.convert_bool(y)?;
                Ok(Dynamic::from_ast(&Bool::and(&self.ctx, &[&x,&y])))
            }
            PureCode::BoolOr(x, y) => {
                let x = self.convert_bool(x)?;
                let y = self.convert_bool(y)?;
                Ok(Dynamic::from_ast(&Bool::or(&self.ctx, &[&x,&y])))
            }
            PureCode::BoolXor(x, y) => {
                let x = self.convert_bool(x)?;
                let y = self.convert_bool(y)?;
                Ok(Dynamic::from_ast(&x.xor(&y)))
            }
            PureCode::Bitv => {
                Ok(Dynamic::from_ast(&BV::from_u64(&self.ctx, op.evaluate(), op.get_size())))
            } 
            PureCode::Msb(bv) => {
                let bv = self.convert_bv(bv)?;
                Ok(Dynamic::from_ast(&bv.extract(op.get_size()-1, op.get_size()-1)))
            }
            PureCode::Lsb(bv) => {
                let bv = self.convert_bv(bv)?;
                Ok(Dynamic::from_ast(&bv.extract(0, 0)))
            }
            PureCode::IsZero(bv) => {
                let bv = self.convert_bv(bv)?;
                let zero = BV::from_u64(&self.ctx, 0, op.get_size());
                Ok(Dynamic::from_ast(&bv._eq(&zero)))
            }
            PureCode::Neg(bv) => {
                let bv = self.convert_bv(bv)?;
                Ok(Dynamic::from_ast(&bv.bvneg()))
            }
            PureCode::LogNot(bv) => {
                let bv = self.convert_bv(bv)?;
                Ok(Dynamic::from_ast(&!bv))
            }
            PureCode::Add(x, y) => {
                let x = self.convert_bv(x)?;
                let y = self.convert_bv(y)?;
                Ok(Dynamic::from_ast(&(x.bvadd(&y))))
            }
            PureCode::Sub(x, y) => {
                let x = self.convert_bv(x)?;
                let y = self.convert_bv(y)?;
                Ok(Dynamic::from_ast(&(x.bvsub(&y))))
            }
            PureCode::Mul(x, y) => {
                let x = self.convert_bv(x)?;
                let y = self.convert_bv(y)?;
                Ok(Dynamic::from_ast(&(x.bvmul(&y))))
            }
            PureCode::Div(x, y) => {
                let x = self.convert_bv(x)?;
                let y = self.convert_bv(y)?;
                Ok(Dynamic::from_ast(&x.bvudiv(&y)))
            }
            PureCode::Sdiv(x, y) => {
                let x = self.convert_bv(x)?;
                let y = self.convert_bv(y)?;
                Ok(Dynamic::from_ast(&x.bvsdiv(&y)))
            }
            PureCode::Mod(x, y) => {
                let x = self.convert_bv(x)?;
                let y = self.convert_bv(y)?;
                Ok(Dynamic::from_ast(&x.bvurem(&y)))
            }
            PureCode::Smod(x, y) => {
                let x = self.convert_bv(x)?;
                let y = self.convert_bv(y)?;
                Ok(Dynamic::from_ast(&x.bvsmod(&y))) // ? The right one may be bvsrem but bvsmod.
            }
            PureCode::LogAnd(x, y) => {
                let x = self.convert_bv(x)?;
                let y = self.convert_bv(y)?;
                Ok(Dynamic::from_ast(&x.bvand(&y)))
            }
            PureCode::LogOr(x, y) => {
                let x = self.convert_bv(x)?;
                let y = self.convert_bv(y)?;
                Ok(Dynamic::from_ast(&x.bvor(&y)))
            }
            PureCode::LogXor(x, y) => {
                let x = self.convert_bv(x)?;
                let y = self.convert_bv(y)?;
                Ok(Dynamic::from_ast(&x.bvxor(&y)))
            }
            PureCode::ShiftRight(fill_bit, x, y) => {
                let x = self.convert_bv(x)?;
                let y = self.convert_bv(y)?;
                if fill_bit.is_concretized() {
                    if fill_bit.evaluate_bool() {
                        Ok(Dynamic::from_ast(&x.bvashr(&y)))
                    } else {
                        Ok(Dynamic::from_ast(&x.bvlshr(&y)))
                    }
                } else {
                    let fill_bit = self.convert_bool(fill_bit)?;
                    Ok(Dynamic::from_ast(&fill_bit.ite(&x.bvashr(&y), &x.bvlshr(&y))))
                }
            }
            PureCode::ShiftLeft(fill_bit, x, y) => {
                let x = self.convert_bv(x)?;
                let y = self.convert_bv(y)?;
                if fill_bit.is_concretized() {
                    if fill_bit.evaluate_bool() {
                        let one = BV::from_u64(&self.ctx, 1, op.get_size());
                        Ok(Dynamic::from_ast(&x.bvshl(&y).bvor(&one.bvshl(&y).bvsub(&one))))
                    } else {
                        Ok(Dynamic::from_ast(&x.bvshl(&y)))
                    }
                } else {
                    let one = BV::from_u64(&self.ctx, 1, op.get_size());
                    let fill_bit = self.convert_bool(fill_bit)?;
                    Ok(Dynamic::from_ast(&fill_bit.ite(
                                &x.bvshl(&y).bvor(&one.bvshl(&y).bvsub(&one)),
                                &x.bvshl(&y))))
                }
            }
            PureCode::Equal(x, y) => {
                let x = self.convert_bv(x)?;
                let y = self.convert_bv(y)?;
                    Ok(Dynamic::from_ast(&x._eq(&y)))
            }
            PureCode::Sle(x, y) => {
                let x = self.convert_bv(x)?;
                let y = self.convert_bv(y)?;
                    Ok(Dynamic::from_ast(&x.bvsle(&y)))
            }
            PureCode::Ule(x, y) => {
                let x = self.convert_bv(x)?;
                let y = self.convert_bv(y)?;
                    Ok(Dynamic::from_ast(&x.bvule(&y)))
            }
            PureCode::Cast(value, fill) => {
                let size_before = value.get_size();
                let size_after = op.get_size();
                let value = self.convert_bv(value)?;
                if size_after >= size_before {
                    if fill.is_concretized() {
                        if fill.evaluate_bool() {
                            // 1..1value
                            Ok(Dynamic::from_ast(&BV::concat(&BV::from_i64(&self.ctx, -1, size_after - size_before), &value)))
                        } else {
                            // 0..0value
                            Ok(Dynamic::from_ast(&value.zero_ext(size_after - size_before)))
                        }
                    } else {
                        // f..fvalue
                        let fill = self.convert_bool(fill)?;
                        Ok(Dynamic::from_ast(&fill.ite(
                                    &BV::concat(&BV::from_i64(&self.ctx, -1, size_after - size_before), &value),
                                    &value.zero_ext(size_after - size_before))))
                    }
                } else {
                    // extract(value)
                    Ok(Dynamic::from_ast(&value.extract(size_after-1, 0)))
                }
            }
            PureCode::Append(high, low) => {
                let high = self.convert_bv(high)?;
                let low = self.convert_bv(low)?;
                Ok(Dynamic::from_ast(&BV::concat(&high, &low)))
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

impl Effect {

}
