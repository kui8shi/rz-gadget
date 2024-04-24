use crate::error::{Result, RiseError};
use crate::rzil::ast::Scope;
use crate::rzil::{
    ast::{PureCode, PureRef, Sort},
    error::RzILError,
};
use crate::state::memory::MemoryRead;
use crate::state::solver::Z3;
use crate::state::State;
use z3::ast::{Ast, Bool, Dynamic, BV};

pub trait ConvertRzILToSymExp {
    type Any;
    type Bool;
    type Bitv;
    fn convert(&self, op: PureRef) -> Result<Self::Any>;
    fn convert_bool(&self, op: PureRef) -> Result<Self::Bool>;
    fn convert_bitv(&self, op: PureRef) -> Result<Self::Bitv>;
    fn convert_set(&self, var: PureRef) -> Result<Self::Any>;
}

impl ConvertRzILToSymExp for State {
    type Any = Dynamic;
    type Bool = Bool;
    type Bitv = BV;
    // ======== provided methods ========
    fn convert(&self, op: PureRef) -> Result<Dynamic> {
        //if op.is_concretized()
        let size: u32 = op.get_size().try_into().unwrap();
        match op.get_code() {
            PureCode::Var { scope: _, id } => match op.get_sort() {
                Sort::Bool => {
                    if op.is_concretized() {
                        Ok(Bool::from_bool(self.get_z3_ctx(), op.evaluate_bool()).into())
                    } else if let Some(z3_var) = self.get_z3_trasnlation(&op) {
                        Ok(z3_var)
                    } else {
                        let z3_var: Dynamic =
                            Bool::new_const(self.get_z3_ctx(), id.get_uniq_name()).into();
                        self.set_z3_trasnlation(op, z3_var.clone());
                        Ok(z3_var)
                    }
                }
                Sort::Bitv(_) => {
                    if op.is_concretized() {
                        Ok(BV::from_u64(self.get_z3_ctx(), op.evaluate(), size).into())
                    } else if let Some(z3_var) = self.get_z3_trasnlation(&op) {
                        Ok(z3_var)
                    } else {
                        let z3_var: Dynamic =
                            BV::new_const(self.get_z3_ctx(), id.get_uniq_name(), size).into();
                        self.set_z3_trasnlation(op, z3_var.clone());
                        Ok(z3_var)
                    }
                }
            },
            PureCode::Ite => {
                let condition = self.convert_bool(op.get_arg(0))?;
                let then = self.convert(op.get_arg(1))?;
                let else_ = self.convert(op.get_arg(2))?;
                Ok(condition.ite(&then, &else_))
            }
            PureCode::Let => {
                let var = self.convert(op.get_arg(0))?;
                let binding = self.convert(op.get_arg(1))?;
                let body = self.convert(op.get_arg(2))?;
                Ok(body.substitute(&[(&var, &binding)]))
            }
            PureCode::Bool => Ok(Bool::from_bool(self.get_z3_ctx(), op.evaluate_bool()).into()),
            PureCode::BoolInv => {
                let x = self.convert_bool(op.get_arg(0))?;
                if op.is_concretized() {
                    Ok(Bool::from_bool(self.get_z3_ctx(), !op.evaluate_bool()).into())
                } else {
                    Ok(x.not().into())
                }
            }
            PureCode::BoolAnd => {
                let x = self.convert_bool(op.get_arg(0))?;
                let y = self.convert_bool(op.get_arg(1))?;
                Ok(Bool::and(self.get_z3_ctx(), &[&x, &y]).into())
            }
            PureCode::BoolOr => {
                let x = self.convert_bool(op.get_arg(0))?;
                let y = self.convert_bool(op.get_arg(1))?;
                Ok(Bool::or(self.get_z3_ctx(), &[&x, &y]).into())
            }
            PureCode::BoolXor => {
                let x = self.convert_bool(op.get_arg(0))?;
                let y = self.convert_bool(op.get_arg(1))?;
                Ok(x.xor(&y).into())
            }
            PureCode::Bitv => Ok(BV::from_u64(self.get_z3_ctx(), op.evaluate(), size).into()),
            PureCode::Msb => {
                let bv = self.convert_bitv(op.get_arg(0))?;
                Ok(bv.extract(size - 1, size - 1).into())
            }
            PureCode::Lsb => {
                let bv = self.convert_bitv(op.get_arg(0))?;
                Ok(bv.extract(0, 0).into())
            }
            PureCode::IsZero => {
                let bv = self.convert_bitv(op.get_arg(0))?;
                let zero = BV::from_u64(self.get_z3_ctx(), 0, size);
                Ok(bv._eq(&zero).into())
            }
            PureCode::Neg => {
                let bv = self.convert_bitv(op.get_arg(0))?;
                Ok(bv.bvneg().into())
            }
            PureCode::LogNot => {
                let bv = self.convert_bitv(op.get_arg(0))?;
                Ok(Dynamic::from_ast(&!bv))
            }
            PureCode::Add => {
                let x = self.convert_bitv(op.get_arg(0))?;
                let y = self.convert_bitv(op.get_arg(1))?;
                Ok((x.bvadd(&y)).into())
            }
            PureCode::Sub => {
                let x = self.convert_bitv(op.get_arg(0))?;
                let y = self.convert_bitv(op.get_arg(1))?;
                Ok((x.bvsub(&y)).into())
            }
            PureCode::Mul => {
                let x = self.convert_bitv(op.get_arg(0))?;
                let y = self.convert_bitv(op.get_arg(1))?;
                Ok((x.bvmul(&y)).into())
            }
            PureCode::Div => {
                let x = self.convert_bitv(op.get_arg(0))?;
                let y = self.convert_bitv(op.get_arg(1))?;
                Ok(x.bvudiv(&y).into())
            }
            PureCode::Sdiv => {
                let x = self.convert_bitv(op.get_arg(0))?;
                let y = self.convert_bitv(op.get_arg(1))?;
                Ok(x.bvsdiv(&y).into())
            }
            PureCode::Mod => {
                let x = self.convert_bitv(op.get_arg(0))?;
                let y = self.convert_bitv(op.get_arg(1))?;
                Ok(x.bvurem(&y).into())
            }
            PureCode::Smod => {
                let x = self.convert_bitv(op.get_arg(0))?;
                let y = self.convert_bitv(op.get_arg(1))?;
                Ok(x.bvsmod(&y).into()) // ? This could be bvsrem but bvsmod for now.
            }
            PureCode::LogAnd => {
                let x = self.convert_bitv(op.get_arg(0))?;
                let y = self.convert_bitv(op.get_arg(1))?;
                Ok(x.bvand(&y).into())
            }
            PureCode::LogOr => {
                let x = self.convert_bitv(op.get_arg(0))?;
                let y = self.convert_bitv(op.get_arg(1))?;
                Ok(x.bvor(&y).into())
            }
            PureCode::LogXor => {
                let x = self.convert_bitv(op.get_arg(0))?;
                let y = self.convert_bitv(op.get_arg(1))?;
                Ok(x.bvxor(&y).into())
            }
            PureCode::ShiftRight => {
                let fill_bit = op.get_arg(0);
                let x = self.convert_bitv(op.get_arg(1))?;
                let y = self.convert_bitv(op.get_arg(2))?;
                if fill_bit.is_zero() {
                    Ok(x.bvlshr(&y).into())
                } else {
                    let fill_bit = self.convert_bitv(fill_bit)?;
                    let expanded = BV::concat(&fill_bit, &x);
                    let shifted = expanded.bvashr(&y);
                    Ok(shifted.extract(size - 1, 0).into())
                }
            }
            PureCode::ShiftLeft => {
                let fill_bit = op.get_arg(0);
                let x = self.convert_bitv(op.get_arg(1))?;
                let y = self.convert_bitv(op.get_arg(2))?;
                if fill_bit.is_zero() {
                    Ok(x.bvshl(&y).into())
                } else {
                    let fill_bit = self.convert_bitv(fill_bit)?;
                    let least_bits = fill_bit.sign_ext(size);
                    let expanded = BV::concat(&x, &least_bits);
                    let shifted = expanded.bvshl(&y);
                    // When fill_bit = 1 and the amount of shift is larger than the size,
                    // the result won't be correct (least bits should be 1111..., but 11..00..).                             // But ignored the case for now.
                    Ok(shifted.extract(2 * size - 1, size).into())
                }
            }
            PureCode::Equal => {
                let x = self.convert_bitv(op.get_arg(0))?;
                let y = self.convert_bitv(op.get_arg(1))?;
                Ok(x._eq(&y).into())
            }
            PureCode::Sle => {
                let x = self.convert_bitv(op.get_arg(0))?;
                let y = self.convert_bitv(op.get_arg(1))?;
                Ok(x.bvsle(&y).into())
            }
            PureCode::Ule => {
                let x = self.convert_bitv(op.get_arg(0))?;
                let y = self.convert_bitv(op.get_arg(1))?;
                Ok(x.bvule(&y).into())
            }
            PureCode::Cast { expand } => {
                let fill_bit = op.get_arg(0);
                let value = self.convert_bitv(op.get_arg(1))?;
                if expand {
                    let size_gap = u32::abs_diff(size, value.get_size());
                    let most_bits = if fill_bit.is_concretized() {
                        BV::from_u64(
                            self.get_z3_ctx(),
                            (fill_bit.evaluate() << size_gap) - 1,
                            size_gap,
                        )
                    } else {
                        let fill_bit = self.convert_bitv(op.get_arg(0))?;
                        fill_bit.sign_ext(size_gap)
                    };
                    Ok(most_bits.concat(&value).into())
                } else {
                    // shrink
                    Ok(value.extract(size - 1, 0).into())
                }
            }
            PureCode::Append => {
                let high = self.convert_bitv(op.get_arg(0))?;
                let low = self.convert_bitv(op.get_arg(1))?;
                Ok(BV::concat(&high, &low).into())
            }
            PureCode::Load => {
                let value = self.load(op.get_arg(0), 8)?;
                Ok(self.convert_bitv(value)?.into())
            }
            _ => {
                unimplemented!()
            }
        }
    }

    fn convert_bool(&self, op: PureRef) -> Result<Self::Bool> {
        if !op.is_bool() {
            return Err(RzILError::UnexpectedSort(Sort::Bool, op.get_sort()).into());
        }
        if let Some(ast) = self.convert(op)?.as_bool() {
            Ok(ast)
        } else {
            Err(RiseError::RzILToZ3(
                "Bool rzil was somehow converted to non-bool z3 ast".to_string(),
            ))
        }
    }
    fn convert_bitv(&self, op: PureRef) -> Result<Self::Bitv> {
        if !op.is_bitv() {
            return Err(RzILError::UnexpectedSort(Sort::Bitv(0), op.get_sort()).into());
        }
        if let Some(ast) = self.convert(op)?.as_bv() {
            Ok(ast)
        } else {
            Err(RiseError::RzILToZ3(
                "Bitv rzil was somehow converted to non-bitv z3 ast".to_string(),
            ))
        }
    }

    fn convert_set(&self, var: PureRef) -> Result<Self::Any> {
        let (scope, id) = var.expect_var()?;
        debug_assert!(var.num_args() == 1);
        debug_assert!(self.get_z3_trasnlation(&var).is_none());
        if scope == Scope::Let {
            return Err(RzILError::ImmutableVariable(id.get_name().to_string()).into());
        }
        let sort = match var.get_sort() {
            Sort::Bool => z3::Sort::bool(self.get_z3_ctx()),
            Sort::Bitv(size) => z3::Sort::bitvector(self.get_z3_ctx(), size as u32),
        };
        let func_decl = z3::FuncDecl::new(self.get_z3_ctx(), id.get_uniq_name(), &[], &sort);
        let val = self.convert(var.get_arg(0))?;
        let z3_var = func_decl.apply(&[&val]);
        self.set_z3_trasnlation(var, z3_var.clone());
        Ok(z3_var)
    }
}
