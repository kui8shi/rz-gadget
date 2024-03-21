use crate::error::{Result, RiseError};
use crate::memory::Memory;
use crate::rzil::{
    ast::{PureCode, PureRef, Sort},
    builder::RzILBuilder,
    error::RzILError,
};
use crate::solver::{Solver, Z3Solver};
use z3::ast::Ast;
#[derive(Clone, Debug)]
pub struct Z3Var<'ctx> {
    var: z3::ast::Dynamic<'ctx>,
}

#[derive(Clone, Debug)]
pub struct ToZ3<'ctx> {
    // fake life time parameter 'static
    // is (maybe a bad practice)
    // to fit within the same struct with z3::Context
    parent: &'ctx Z3Solver,
    z3_solver: z3::Solver<'ctx>,
    //mappings: HashMap<PureRef, Z3Var<'ctx>>,
}

impl<'ctx> ToZ3<'ctx> {
    pub fn new(solver_api_ref: &'ctx Z3Solver) -> Self {
        //let leaky: &'ctx z3::Context = Box::leak(Box::new(z3_ctx));
        /*
        let z3_solver = Rc::new(OwningHandle::new_with_fn(Box::new(z3_ctx), |ctx| {
            unsafe{ Box::new(z3::Solver::new(ctx.as_ref().unwrap())) }
        }));
        */
        Self {
            parent: solver_api_ref,
            z3_solver: z3::Solver::new(solver_api_ref.get_z3_ctx()),
        }
    }

    pub fn convert(
        &'ctx self,
        m: &Memory,
        r: &RzILBuilder,
        op: PureRef,
    ) -> Result<z3::ast::Dynamic<'ctx>> {
        //if op.is_concretized()
        let size: u32 = op.get_size().try_into().unwrap();
        match op.get_code() {
            PureCode::Var(_, name) => match op.get_sort() {
                Sort::Bool => {
                    if op.is_concretized() {
                        Ok(z3::ast::Bool::from_bool(self.get_z3_ctx(), op.evaluate_bool()).into())
                    } else {
                        Ok(z3::ast::Bool::new_const(self.get_z3_ctx(), name).into())
                    }
                }
                Sort::Bitv(_) => {
                    if op.is_concretized() {
                        Ok(z3::ast::BV::from_u64(self.get_z3_ctx(), op.evaluate(), size).into())
                    } else {
                        Ok(z3::ast::BV::new_const(self.get_z3_ctx(), name, size).into())
                    }
                }
            },
            PureCode::Ite => {
                let condition = self.convert_bool(m, r, op.get_arg(0))?;
                let then = self.convert(m, r, op.get_arg(1))?;
                let else_ = self.convert(m, r, op.get_arg(2))?;
                Ok(condition.ite(&then, &else_))
            }
            PureCode::Let => {
                let var = self.convert(m, r, op.get_arg(0))?;
                let binding = self.convert(m, r, op.get_arg(1))?;
                let body = self.convert(m, r, op.get_arg(2))?;
                Ok(body.substitute(&[(&var, &binding)]))
            }
            PureCode::Bool => {
                Ok(z3::ast::Bool::from_bool(self.get_z3_ctx(), op.evaluate_bool()).into())
            }
            PureCode::BoolInv => {
                let x = self.convert_bool(m, r, op.get_arg(0))?;
                if op.is_concretized() {
                    Ok(z3::ast::Bool::from_bool(self.get_z3_ctx(), !op.evaluate_bool()).into())
                } else {
                    Ok(x.not().into())
                }
            }
            PureCode::BoolAnd => {
                let x = self.convert_bool(m, r, op.get_arg(0))?;
                let y = self.convert_bool(m, r, op.get_arg(1))?;
                Ok(z3::ast::Bool::and(self.get_z3_ctx(), &[&x, &y]).into())
            }
            PureCode::BoolOr => {
                let x = self.convert_bool(m, r, op.get_arg(0))?;
                let y = self.convert_bool(m, r, op.get_arg(1))?;
                Ok(z3::ast::Bool::or(self.get_z3_ctx(), &[&x, &y]).into())
            }
            PureCode::BoolXor => {
                let x = self.convert_bool(m, r, op.get_arg(0))?;
                let y = self.convert_bool(m, r, op.get_arg(1))?;
                Ok(x.xor(&y).into())
            }
            PureCode::Bitv => {
                Ok(z3::ast::BV::from_u64(self.get_z3_ctx(), op.evaluate(), size).into())
            }
            PureCode::Msb => {
                let bv = self.convert_bv(m, r, op.get_arg(0))?;
                Ok(bv.extract(size - 1, size - 1).into())
            }
            PureCode::Lsb => {
                let bv = self.convert_bv(m, r, op.get_arg(0))?;
                Ok(bv.extract(0, 0).into())
            }
            PureCode::IsZero => {
                let bv = self.convert_bv(m, r, op.get_arg(0))?;
                let zero = z3::ast::BV::from_u64(self.get_z3_ctx(), 0, size);
                Ok(bv._eq(&zero).into())
            }
            PureCode::Neg => {
                let bv = self.convert_bv(m, r, op.get_arg(0))?;
                Ok(bv.bvneg().into())
            }
            PureCode::LogNot => {
                let bv = self.convert_bv(m, r, op.get_arg(0))?;
                Ok(z3::ast::Dynamic::from_ast(&!bv))
            }
            PureCode::Add => {
                let x = self.convert_bv(m, r, op.get_arg(0))?;
                let y = self.convert_bv(m, r, op.get_arg(1))?;
                Ok((x.bvadd(&y)).into())
            }
            PureCode::Sub => {
                let x = self.convert_bv(m, r, op.get_arg(0))?;
                let y = self.convert_bv(m, r, op.get_arg(1))?;
                Ok((x.bvsub(&y)).into())
            }
            PureCode::Mul => {
                let x = self.convert_bv(m, r, op.get_arg(0))?;
                let y = self.convert_bv(m, r, op.get_arg(1))?;
                Ok((x.bvmul(&y)).into())
            }
            PureCode::Div => {
                let x = self.convert_bv(m, r, op.get_arg(0))?;
                let y = self.convert_bv(m, r, op.get_arg(1))?;
                Ok(x.bvudiv(&y).into())
            }
            PureCode::Sdiv => {
                let x = self.convert_bv(m, r, op.get_arg(0))?;
                let y = self.convert_bv(m, r, op.get_arg(1))?;
                Ok(x.bvsdiv(&y).into())
            }
            PureCode::Mod => {
                let x = self.convert_bv(m, r, op.get_arg(0))?;
                let y = self.convert_bv(m, r, op.get_arg(1))?;
                Ok(x.bvurem(&y).into())
            }
            PureCode::Smod => {
                let x = self.convert_bv(m, r, op.get_arg(0))?;
                let y = self.convert_bv(m, r, op.get_arg(1))?;
                Ok(x.bvsmod(&y).into()) // ? This could be bvsrem but bvsmod for now.
            }
            PureCode::LogAnd => {
                let x = self.convert_bv(m, r, op.get_arg(0))?;
                let y = self.convert_bv(m, r, op.get_arg(1))?;
                Ok(x.bvand(&y).into())
            }
            PureCode::LogOr => {
                let x = self.convert_bv(m, r, op.get_arg(0))?;
                let y = self.convert_bv(m, r, op.get_arg(1))?;
                Ok(x.bvor(&y).into())
            }
            PureCode::LogXor => {
                let x = self.convert_bv(m, r, op.get_arg(0))?;
                let y = self.convert_bv(m, r, op.get_arg(1))?;
                Ok(x.bvxor(&y).into())
            }
            PureCode::ShiftRight => {
                let fill_bit = op.get_arg(0);
                let x = self.convert_bv(m, r, op.get_arg(1))?;
                let y = self.convert_bv(m, r, op.get_arg(2))?;
                if fill_bit.is_zero() {
                    Ok(x.bvlshr(&y).into())
                } else {
                    let fill_bit = self.convert_bv(m, r, fill_bit.clone())?;
                    let expanded = z3::ast::BV::concat(&fill_bit, &x);
                    let shifted = expanded.bvashr(&y);
                    Ok(shifted.extract(size - 1, 0).into())
                }
            }
            PureCode::ShiftLeft => {
                let fill_bit = op.get_arg(0);
                let x = self.convert_bv(m, r, op.get_arg(1))?;
                let y = self.convert_bv(m, r, op.get_arg(2))?;
                if fill_bit.is_zero() {
                    Ok(x.bvshl(&y).into())
                } else {
                    let fill_bit = self.convert_bv(m, r, fill_bit)?;
                    let least_bits = fill_bit.sign_ext(size);
                    let expanded = z3::ast::BV::concat(&x, &least_bits);
                    let shifted = expanded.bvshl(&y);
                    // When fill_bit = 1 and the amount of shift is larger than the size,
                    // the result won't be correct (least bits should be 1111..., but 11..00..).                             // But ignored the case for now.
                    Ok(shifted.extract(2 * size - 1, size).into())
                }
            }
            PureCode::Equal => {
                let x = self.convert_bv(m, r, op.get_arg(0))?;
                let y = self.convert_bv(m, r, op.get_arg(1))?;
                Ok(x._eq(&y).into())
            }
            PureCode::Sle => {
                let x = self.convert_bv(m, r, op.get_arg(0))?;
                let y = self.convert_bv(m, r, op.get_arg(1))?;
                Ok(x.bvsle(&y).into())
            }
            PureCode::Ule => {
                let x = self.convert_bv(m, r, op.get_arg(0))?;
                let y = self.convert_bv(m, r, op.get_arg(1))?;
                Ok(x.bvule(&y).into())
            }
            PureCode::Cast(expand) => {
                let fill_bit = op.get_arg(0).clone();
                let value = self.convert_bv(m, r, op.get_arg(1))?;
                if expand {
                    let size_gap = u32::abs_diff(size, value.get_size());
                    let most_bits = if fill_bit.is_concretized() {
                        z3::ast::BV::from_u64(
                            self.get_z3_ctx(),
                            (fill_bit.evaluate() << size_gap) - 1,
                            size_gap,
                        )
                    } else {
                        let fill_bit = self.convert_bv(m, r, op.get_arg(0))?;
                        fill_bit.sign_ext(size_gap)
                    };
                    Ok(most_bits.concat(&value).into())
                } else {
                    // shrink
                    Ok(value.extract(size - 1, 0).into())
                }
            }
            PureCode::Append => {
                let high = self.convert_bv(m, r, op.get_arg(0))?;
                let low = self.convert_bv(m, r, op.get_arg(1))?;
                Ok(z3::ast::BV::concat(&high, &low).into())
            }
            PureCode::Load => {
                let value = m.load(self.parent, r, op.get_arg(0), 8)?;
                Ok(self.convert_bv(m, r, value)?.into())
            }
            _ => {
                unimplemented!()
            }
        }
    }

    pub fn convert_bool(
        &'ctx self,
        m: &Memory,
        r: &RzILBuilder,
        op: PureRef,
    ) -> Result<z3::ast::Bool<'ctx>> {
        if !op.is_bool() {
            return Err(RzILError::UnexpectedSort(Sort::Bool, op.get_sort()).into());
        }
        if let Some(ast) = self.convert(m, r, op)?.as_bool() {
            Ok(ast)
        } else {
            Err(RiseError::RzILToZ3(
                "Bool rzil was somehow converted to non-bool z3 ast".to_string(),
            ))
        }
    }
    pub fn convert_bv(
        &'ctx self,
        m: &Memory,
        r: &RzILBuilder,
        op: PureRef,
    ) -> Result<z3::ast::BV<'ctx>> {
        if !op.is_bitv() {
            return Err(RzILError::UnexpectedSort(Sort::Bitv(0), op.get_sort()).into());
        }
        if let Some(ast) = self.convert(m, r, op)?.as_bv() {
            Ok(ast)
        } else {
            Err(RiseError::RzILToZ3(
                "Bitv rzil was somehow converted to non-bitv z3 ast".to_string(),
            ))
        }
    }

    pub fn get_z3_ctx(&self) -> &z3::Context {
        self.parent.get_z3_ctx()
    }

    pub fn get_model(
        &'ctx self,
        extra_constraint: &[z3::ast::Bool],
    ) -> Result<Option<z3::Model<'ctx>>> {
        let solver = self.z3_solver.clone();
        for ast in extra_constraint {
            self.assert(ast);
        }
        Ok(solver.get_model())
    }

    pub fn assert(&'ctx self, ast: &z3::ast::Bool<'ctx>) {
        self.z3_solver.assert(ast);
    }
}
//fn effect_to_z3<'a>
