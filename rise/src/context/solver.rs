use super::RiseContext;
use crate::error::{Result, RiseError};
use crate::rzil::{ast::PureRef, builder::RzILBuilder};
use crate::to_z3::ConvertRzIL;
use quick_cache::sync::Cache;
use std::collections::{BinaryHeap, HashMap};
use std::rc::Rc;
use std::vec;
use z3::ast::Dynamic;
//use owning_ref::OwningHandle;

#[derive(Clone, Debug)]
pub struct Z3Solver {
    z3_ctx: Rc<z3::Context>,
    z3_solver: z3::Solver,
    translate: Rc<Cache<PureRef, z3::ast::Dynamic>>,
}

impl Z3Solver {
    pub fn new() -> Self {
        const DEFAULT_CACHE_SIZE: usize = 100;
        let z3_cfg = z3::Config::new();
        let z3_ctx = Rc::new(z3::Context::new(&z3_cfg));
        let z3_solver = z3::Solver::new(z3_ctx.clone());
        Z3Solver {
            z3_ctx,
            z3_solver,
            translate: Rc::new(Cache::new(DEFAULT_CACHE_SIZE)),
        }
    }

    pub fn get_z3_ctx(&self) -> Rc<z3::Context> {
        self.z3_ctx.clone()
    }

    pub fn get_z3_solver(&self) -> &z3::Solver {
        &self.z3_solver
    }

    pub fn cache_z3_var(&self, var: PureRef, z3_ast: Dynamic) {
        self.translate.insert(var, z3_ast)
    }

    pub(self) fn get_z3_trasnlation(&self, var: &PureRef) -> Option<Dynamic> {
        self.translate.get(var)
    }

    pub(self) fn set_z3_trasnlation(&self, var: PureRef, z3_var: Dynamic) {
        self.translate.insert(var, z3_var)
    }
}

fn get_interp(val: z3::ast::Dynamic) -> Result<u64> {
    if let Some(v) = val.as_bool() {
        match v.as_bool() {
            Some(true) => Ok(1),
            Some(false) => Ok(0),
            None => Err(RiseError::Z3(
                "returned invalid model (not concretized).".to_owned(),
            )),
        }
    } else if let Some(v) = val.as_bv() {
        match v.as_u64() {
            Some(val) => Ok(val),
            None => Err(RiseError::Z3(
                "returned invalid model (not concretized).".to_owned(),
            )),
        }
    } else {
        return Err(RiseError::Z3(
            "returned invalid model (unkown sort).".to_owned(),
        ));
    }
}

pub trait Z3 {
    fn get_z3_ctx(&self) -> Rc<z3::Context>;

    fn get_z3_solver(&self) -> &z3::Solver;

    fn get_z3_trasnlation(&self, var: &PureRef) -> Option<Dynamic>;

    fn set_z3_trasnlation(&self, var: PureRef, z3_var: Dynamic);

    fn z3_get_model(&self, extra_constraint: &[z3::ast::Bool]) -> Result<Option<z3::Model>> {
        let solver = self.get_z3_solver();
        for ast in extra_constraint {
            self.z3_assert(ast);
        }
        Ok(solver.get_model())
    }

    fn z3_assert(&self, ast: &z3::ast::Bool) {
        self.get_z3_solver().assert(ast);
    }
}

impl Z3 for RiseContext {
    fn get_z3_ctx(&self) -> Rc<z3::Context> {
        self.solver.get_z3_ctx()
    }

    fn get_z3_solver(&self) -> &z3::Solver {
        self.solver.get_z3_solver()
    }

    fn get_z3_trasnlation(&self, var: &PureRef) -> Option<Dynamic> {
        self.solver.get_z3_trasnlation(var)
    }

    fn set_z3_trasnlation(&self, var: PureRef, z3_var: Dynamic) {
        self.solver.set_z3_trasnlation(var, z3_var)
    }
}

pub trait Solver {
    fn solver_name(&self) -> &'static str;
    fn assert(&self, constriant: PureRef) -> Result<()>;
    fn assert_n(&self, constraints: Vec<PureRef>) -> Result<()>;
    fn get_models(
        &mut self,
        extra_constraint: Vec<PureRef>,
        n: usize,
    ) -> Result<Vec<HashMap<String, u64>>>;
    fn evaluate(&self, target: PureRef, n: usize) -> Result<Vec<u64>>;
    fn get_min(&self, target: PureRef) -> Result<u64>;
    fn get_max(&self, target: PureRef) -> Result<u64>;
    fn is_sat(&self) -> Result<bool>;
    fn is_unsat(&self) -> Result<bool>;
}

impl Solver for RiseContext {
    fn solver_name(&self) -> &'static str {
        "z3"
    }

    fn assert(&self, constraint: PureRef) -> Result<()> {
        self.assert_n(vec![constraint])
    }
    fn assert_n(&self, constraints: Vec<PureRef>) -> Result<()> {
        //TODO test
        for op in constraints {
            let ast = self.convert_bool(op)?;
            self.z3_assert(&ast);
        }
        Ok(())
    }

    // extract n models from current context.
    fn get_models(
        &mut self,
        extra_constraints: Vec<PureRef>,
        n: usize,
    ) -> Result<Vec<HashMap<String, u64>>> {
        //TODO
        let mut ex_cons = Vec::new();
        let mut models = Vec::new();
        for op in extra_constraints {
            let ast = self.convert_bool(op)?;
            ex_cons.push(ast);
        }
        for _ in 0..n {
            let mut kv = HashMap::new();
            if let Some(model) = self.z3_get_model(&ex_cons)? {
                for func_decl in model.iter() {
                    if let Some(val) =
                        model.get_func_interp_as_const::<z3::ast::Dynamic>(&func_decl)
                    {
                        kv.insert(func_decl.name(), get_interp(val)?);
                    }
                }
            }
            models.push(kv);
        }
        Ok(models)
    }

    // extract n models of op from current context.
    // returned vector has distinct and sorted values.
    fn evaluate(&self, op: PureRef, n: usize) -> Result<Vec<u64>> {
        let ast = self.convert(op.clone())?;
        let mut results = BinaryHeap::new();
        let mut extra_constraint = Vec::new();
        for _ in 0..n {
            if let Some(model) = self.z3_get_model(&extra_constraint)? {
                let val = match model.get_const_interp(&ast) {
                    Some(val) => get_interp(val)?,
                    None => return Err(RiseError::Z3("returned no model.".to_owned())),
                };
                results.push(val);
                let val = self.rzil.new_const(op.get_sort(), val);
                let eq = self.rzil.new_eq(op.clone(), val)?;
                let ex_c = self.rzil.new_boolinv(eq)?;
                extra_constraint.push(self.convert(ex_c)?.as_bool().unwrap());
            } else {
                // model not found (unsat)
                break;
            }
        }
        Ok(results.into_sorted_vec())
    }

    fn get_min(&self, op: PureRef) -> Result<u64> {
        match self.evaluate(op, 10)?.first() {
            Some(val) => Ok(*val),
            None => Err(RiseError::Unsat),
        }
    }

    fn get_max(&self, op: PureRef) -> Result<u64> {
        match self.evaluate(op, 10)?.last() {
            Some(val) => Ok(*val),
            None => Err(RiseError::Unsat),
        }
    }

    fn is_sat(&self) -> Result<bool> {
        //Ok(z3.get_model(&[])?.is_some())
        Ok(true)
    }
    fn is_unsat(&self) -> Result<bool> {
        //Ok(z3.get_model(&[])?.is_none())
        Ok(true)
    }
}
