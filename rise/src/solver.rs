use crate::memory::Memory;
use crate::rzil::{
    PureRef, 
    builder::RzILBuilder, 
};
use std::collections::{
    BinaryHeap,
    HashMap,
};
use std::rc::Rc;
use std::vec;
use crate::to_z3::ToZ3;
use crate::error::{RiseError, RiseResult};
//use owning_ref::OwningHandle;
     
pub trait Solver {
    fn name(&self) -> &'static str;
    fn assert(&self, mem: &Memory, rzil: &RzILBuilder, constriant: PureRef) -> RiseResult<()>;
    fn assert_n(&self, mem: &Memory, rzil: &RzILBuilder, constraints: Vec<PureRef>) -> RiseResult<()>;
    fn get_models(&self, mem: &Memory, rzil: &RzILBuilder, extra_constraint: Vec<PureRef>, n: usize) -> RiseResult<Vec<HashMap<String, u64>>>;
    fn evaluate(&self, mem: &Memory, rzil: &RzILBuilder, target: PureRef, n: usize) -> RiseResult<Vec<u64>>;
    fn get_min(&self, mem: &Memory, rzil: &RzILBuilder, target: PureRef) -> RiseResult<u64>;
    fn get_max(&self, mem: &Memory, rzil: &RzILBuilder, target: PureRef) -> RiseResult<u64>;
    fn is_sat(&self) -> RiseResult<bool>;
    fn is_unsat(&self) -> RiseResult<bool>;
}

fn get_interp(val: z3::ast::Dynamic) -> RiseResult<u64> {
    if let Some(v) = val.as_bool() {
        match v.as_bool() {
            Some(true) => Ok(1),
            Some(false) => Ok(0),
            None => {
                return Err(RiseError::Z3(
                    "returned invalid model (not concretized).".to_owned(),
                ))
            }
        }
    } else if let Some(v) = val.as_bv() {
        match v.as_u64() {
            Some(val) => Ok(val),
            None => {
                return Err(RiseError::Z3(
                    "returned invalid model (not concretized).".to_owned(),
                ))
            }
        }
    } else {
        return Err(RiseError::Z3("returned invalid model (unkown sort).".to_owned()));
    }
}

#[derive(Clone, Debug)]
pub struct Z3Solver {
    z3_ctx: Rc<z3::Context>,
}

impl Z3Solver {
    pub fn new() -> Self {
        let z3_cfg = z3::Config::new();
        Z3Solver { z3_ctx: Rc::new(z3::Context::new(&z3_cfg)) }
    }

    pub fn get_z3_ctx(&self) -> &z3::Context {
        &self.z3_ctx
    }
}

impl Solver for Z3Solver {
    fn name(&self) -> &'static str {
        "z3"
    }

    fn assert(&self, mem: &Memory, rzil: &RzILBuilder, constraint: PureRef) -> RiseResult<()> {
        self.assert_n(mem, rzil, vec![constraint])
    }
    fn assert_n(&self, mem: &Memory, rzil: &RzILBuilder, constraints: Vec<PureRef>) -> RiseResult<()> {
        //TODO test
        let z3 = ToZ3::new(self);
        for op in constraints {
            let ast = z3.convert_bool(mem, rzil, op.clone())?;
            z3.assert(&ast);
        }
        Ok(())
    }

    // extract n models from current context.
    fn get_models(&self,
                  mem: &Memory,
                  rzil: &RzILBuilder,
                  extra_constraints: Vec<PureRef>,
                  n: usize) -> RiseResult<Vec<HashMap<String, u64>>> {
        //TODO
        let z3 = ToZ3::new(self);
        let mut ex_cons = Vec::new();
        let mut models = Vec::new();
        for op in extra_constraints {
            let ast = z3.convert_bool(mem, rzil, op)?;
            ex_cons.push(ast);
        }
        for i in 0..n {
            let mut kv = HashMap::new();
            if let Some(model) = z3.get_model(&ex_cons)? {
                for func_decl in model.iter() {
                    if let Some(val) = 
                        model.get_func_interp_as_const::<z3::ast::Dynamic>(&func_decl) {
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
    fn evaluate(&self, 
                mem: &Memory, 
                rzil: &RzILBuilder,
                op: PureRef,
                n: usize) -> RiseResult<Vec<u64>> {
        let z3 = ToZ3::new(self);
        let ast = z3.convert(mem, rzil, op.clone())?;
        let mut results = BinaryHeap::new();
        let mut extra_constraint = Vec::new();
        for _ in 0..n {
            if let Some(model) = z3.get_model(&extra_constraint)? {
                let val = match model.get_const_interp(&ast) {
                    Some(val) => {
                        get_interp(val)?
                    }
                    None => {
                        return Err(RiseError::Z3(
                            "returned no model.".to_owned(),
                        ))
                    }
                };
                results.push(val.clone());
                let val = rzil.new_const(op.get_sort(), val);
                let eq = rzil.new_eq(op.clone(), val)?;
                let ex_c = rzil.new_boolinv(eq)?;
                extra_constraint.push({
                    z3.convert(mem, rzil, ex_c)?.as_bool().unwrap()
                });
            } else {
                // model not found (unsat)
                break;
            }
        }
        Ok(results.into_sorted_vec())
    }

    fn get_min(&self, mem: &Memory, rzil: &RzILBuilder, op: PureRef) -> RiseResult<u64> {
        match self.evaluate(mem, rzil, op, 10)?.first() {
            Some(val) => Ok(val.clone()),
            None => Err(RiseError::Unsat),
        }
    }

    fn get_max(&self, mem: &Memory, rzil: &RzILBuilder, op: PureRef) -> RiseResult<u64> {
        match self.evaluate(mem, rzil, op, 10)?.last() {
            Some(val) => Ok(val.clone()),
            None => Err(RiseError::Unsat),
        }
    }

    fn is_sat(&self) -> RiseResult<bool> {
        //Ok(z3.get_model(&[])?.is_some())
        Ok(true)
    }
    fn is_unsat(&self) -> RiseResult<bool> {
        //Ok(z3.get_model(&[])?.is_none())
        Ok(true)
    }
}
