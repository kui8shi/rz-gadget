use z3;
use std::{rc::Rc, collections::BinaryHeap};
use std::mem;
use std::collections::BTreeSet;
use crate::rzil::{
    {RzILContext, Pure, RzILError, Sort},
   to_z3::ToZ3Error,
};
use thiserror::Error;
use crate::memory::Memory;

type SolverResult<T> = std::result::Result<T, SolverError>;

#[derive(Error, Debug)]
pub enum SolverError {
    #[error("RzIL to Z3 conversion failed: {0}")]
    ToZ3(#[from] ToZ3Error),

    #[error("Z3 Solver's incorrect behavior: {0}")]
    Z3(String),

    #[error("RzIL generation failed: {0}")]
    RzIL(#[from] RzILError),
}

pub struct Solver {
    solver: z3::Solver<'static>, 
        // fake life time parameter
        // which is (maybe a bad practice) 
        // to fit within the same struct with z3::Context
    z3_ctx: z3::Context,
    rzil_ctx: Rc<RzILContext>,
    //cache: HashMap<u64, Rc<z3::ast::Dynamic>>,
}

impl Solver {
    pub fn new(rzil_ctx: Rc<RzILContext>) -> Self {
        let z3_cfg = z3::Config::new();
        let z3_ctx = z3::Context::new(&z3_cfg);
        let solver: z3::Solver<'static> = unsafe {
            mem::transmute(z3::Solver::new(&z3_ctx))
        };
        Solver {
            solver,
            z3_ctx,
            rzil_ctx,
            //cache: HashMap::new(),
        }

    }

    pub fn assert(&self, mem: &mut Memory, op:Rc<Pure>) -> SolverResult<()> {
        if !op.is_bool() {
            return Err(SolverError::RzIL(RzILError::UnexpectedSort(Sort::Bool, op.get_sort())));
        }
        let ast = self._to_z3(mem, op)?;
        self.solver.assert(&ast.as_bool().unwrap());
        Ok(())
    }

    // extract n models of op from current context.
    // returned vector has distinct and sorted valueself.
    pub fn evaluate(&self, mem: &mut Memory, op: Rc<Pure>, n: usize) -> SolverResult<Vec<u64>> {
        let ast = self._to_z3(mem, op.clone())?;
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
                                None => return Err(SolverError::Z3(
                                        "returned symbolized interpretation.".to_owned())),
                            }
                        } else if let Some(v) = val.as_bv() {
                            match v.as_u64() {
                                Some(val) => val,
                                None => return Err(SolverError::Z3(
                                        "returned symbolized interpretation.".to_owned())),
                            }
                        } else {
                            return Err(SolverError::Z3(
                                "unknown sort.".to_owned()));
                        }
                    }
                    None => return Err(SolverError::Z3(
                        "returned the model without an expected interpretation.".to_owned())),
                };
                results.push(val.clone());
                extra_constraint.push({
                    let val = self.rzil_ctx.new_const(op.get_sort(), val);
                    let eq = self.rzil_ctx.new_eq(op.clone(),val);
                    let ex_c = self.rzil_ctx.new_boolinv(eq);
                    self._to_z3(mem, ex_c)?.as_bool().unwrap()
                });
            } else {
                // model not found (unsat)
                break
            }
        }
        Ok(results.into_sorted_vec())
    }

    pub fn get_min(&self, mem: &mut Memory, op: Rc<Pure>) -> SolverResult<Option<u64>> {
         Ok(self.evaluate(mem, op, 10)?.first().cloned())
    }

    pub fn get_max(&self, mem: &mut Memory, op: Rc<Pure>) -> SolverResult<Option<u64>> {
         Ok(self.evaluate(mem, op, 10)?.last().cloned())
    }

    fn _to_z3<'a>(&'a self, mem: &mut Memory, op: Rc<Pure>) -> SolverResult<z3::ast::Dynamic<'a>> {
        /* this code was intended to cache the past 'to_z3' queries as forms of z3::ast::Dynamic. 
         * However, it was impossible since z3::ast::Dynamic has a generic lifetime parameter 
         * which the Solver struct could not hold.
         *
        match self.cache.get(op.hash) {
            Some(ast) => Ok(ast)
            None => match op.to_z3(self.ctx) {
               Ok(ast) => {
                   self.cache.insert(op.hash, ast.clone())
                   Ok(ast)
               }
                   Err(e) => {
                   Err(SolverError::ToZ3Error(e.into()))
               }
            }
        }
         */
        Ok(op.to_z3(&self.z3_ctx, mem)?)
    }

    fn _get_model<'a>(&'a self, extra_constraint: &[z3::ast::Bool]) -> SolverResult<Option<z3::Model<'a>>> {
        let solver = self.solver.clone();
        for ast in extra_constraint {
            solver.assert(&ast);
        }
        Ok(solver.get_model())
    }
}

