use crate::memory::Memory;
use crate::rzil::{Pure, RzIL, RzILError, Sort};
use std::mem::transmute;
use std::{collections::BinaryHeap, rc::Rc};
use crate::error::{RiseeResult, RiseeError};
use z3;
use crate::to_z3::to_z3;

#[derive(Clone, Debug)]
pub struct Solver {
    solver: z3::Solver<'static>,
    // fake life time parameter
    // which is (maybe a bad practice)
    // to fit within the same struct with z3::Context
    z3_ctx: Rc<z3::Context>,
    //cache: HashMap<u64, Rc<z3::ast::Dynamic>>,
}

impl Solver {
    pub fn new() -> Self {
        let z3_cfg = z3::Config::new();
        let z3_ctx = Rc::new(z3::Context::new(&z3_cfg));
        let solver: z3::Solver<'static> = unsafe { transmute(z3::Solver::new(&z3_ctx)) };
        Solver {
            solver,
            z3_ctx,
            //cache: HashMap::new(),
        }
    }

    pub fn get_z3_ctx(&self) -> &z3::Context {
        self.z3_ctx.as_ref()
    }

    pub fn assert(&self, mem: &Memory, rzil: &RzIL, op: Rc<Pure>) -> RiseeResult<()> {
        if !op.is_bool() {
            return Err(RiseeError::RzIL(RzILError::UnexpectedSort(
                Sort::Bool,
                op.get_sort(),
            )));
        }
        let ast = to_z3(self, mem, rzil, op)?;
        self.solver.assert(&ast.as_bool().unwrap());
        Ok(())
    }

    // extract n models of op from current context.
    // returned vector has distinct and sorted values.
    pub fn evaluate(&self, 
                    mem: &Memory, 
                    rzil: &RzIL,
                    op: Rc<Pure>,
                    n: usize) -> RiseeResult<Vec<u64>> {
        let ast = to_z3(self, mem, rzil, op.clone())?;
        let mut results = BinaryHeap::new();
        let mut extra_constraint = Vec::new();
        for _ in 0..n {
            if let Some(model) = self._get_model(&extra_constraint)? {
                let val = match model.get_const_interp(&ast) {
                    Some(val) => {
                        if let Some(v) = val.as_bool() {
                            match v.as_bool() {
                                Some(true) => 1,
                                Some(false) => 0,
                                None => {
                                    return Err(RiseeError::Z3(
                                        "returned invalid model (not concretized).".to_owned(),
                                    ))
                                }
                            }
                        } else if let Some(v) = val.as_bv() {
                            match v.as_u64() {
                                Some(val) => val,
                                None => {
                                    return Err(RiseeError::Z3(
                                        "returned invalid model (not concretized).".to_owned(),
                                    ))
                                }
                            }
                        } else {
                            return Err(RiseeError::Z3("returned invalid model (unkown sort).".to_owned()));
                        }
                    }
                    None => {
                        return Err(RiseeError::Z3(
                            "returned no model.".to_owned(),
                        ))
                    }
                };
                results.push(val.clone());
                let val = rzil.new_const(op.get_sort(), val);
                let eq = rzil.new_eq(op.clone(), val);
                let ex_c = rzil.new_boolinv(eq);
                extra_constraint.push({
                    to_z3(self, mem, rzil, ex_c)?.as_bool().unwrap()
                });
            } else {
                // model not found (unsat)
                break;
            }
        }
        Ok(results.into_sorted_vec())
    }
    pub fn get_min(&self, mem: &Memory, rzil: &RzIL, op: Rc<Pure>) -> RiseeResult<u64> {
        match self.evaluate(mem, rzil, op, 10)?.first() {
            Some(val) => Ok(val.clone()),
            None => Err(RiseeError::Unsat),
        }
    }

    pub fn get_max(&self, mem: &Memory, rzil: &RzIL, op: Rc<Pure>) -> RiseeResult<u64> {
        match self.evaluate(mem, rzil, op, 10)?.last() {
            Some(val) => Ok(val.clone()),
            None => Err(RiseeError::Unsat),
        }
    }

    fn _get_model<'a>(
        &'a self,
        extra_constraint: &[z3::ast::Bool],
    ) -> RiseeResult<Option<z3::Model<'a>>> {
        let solver = self.solver.clone();
        for ast in extra_constraint {
            solver.assert(&ast);
        }
        Ok(solver.get_model())
    }

    pub fn is_sat(&self) -> RiseeResult<bool> {
        Ok(self._get_model(&[])?.is_some())
    }
    pub fn is_unsat(&self) -> RiseeResult<bool> {
        Ok(self._get_model(&[])?.is_none())
    }
}
