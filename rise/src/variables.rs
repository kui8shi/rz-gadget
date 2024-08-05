use crate::{registers::RegSpec, rzil::error::RzILError};
use std::collections::HashMap;

use crate::rzil::{
    error::Result,
    {PureRef, Scope},
};

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct VarId {
    name: String,
    count: u32, // 0-index id that is unique among a set of variables with the same name
                // it works for registers as a counting number of write accesses
}

impl VarId {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            count: 0,
        }
    }

    pub fn new_with_count(name: &str, count: u32) -> Self {
        Self {
            name: name.to_string(),
            count,
        }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_uniq_name(&self) -> String {
        format!("{}!{}", self.get_name(), self.get_count())
    }

    pub(self) fn get_count(&self) -> u32 {
        self.count
    }

    pub(self) fn set_count(&mut self, id: u32) {
        self.count = id
    }
}

#[derive(Clone, Debug)]
pub struct VarStorage {
    scopes: HashMap<String, Scope>,
    latest_var_ids: HashMap<String, VarId>,
    vars: HashMap<VarId, PureRef>,
    reg_specs: HashMap<String, RegSpec>,
}

impl VarStorage {
    pub fn new() -> Self {
        VarStorage {
            scopes: HashMap::new(),
            latest_var_ids: HashMap::new(),
            vars: HashMap::new(),
            reg_specs: HashMap::new(),
        }
    }
}

pub trait Variables {
    fn clear(&mut self);
    fn clear_local(&mut self);
    fn get_scope(&self, name: &str) -> Option<Scope>;
    fn get_uniq_id(&self, name: &str) -> VarId;
    fn get_var(&self, name: &str) -> Option<PureRef>;
    fn set_var(&mut self, var: PureRef) -> Result<()>;
    fn remove_var(&mut self, name: &str) -> Option<PureRef>;
    fn add_register(&mut self, reg: &RegSpec);
    fn concretize_register(&mut self, name: &str, val: u64);
    fn get_reg_spec(&self, name: &str) -> Option<RegSpec>;
}

impl Variables for VarStorage {
    fn clear(&mut self) {
        self.scopes.clear();
        self.latest_var_ids.clear();
        self.vars.clear();
        self.reg_specs.clear();
    }

    fn clear_local(&mut self) {
        // clear vars with a local or smaller scope
        self.scopes.retain(|_, v| *v == Scope::Global);
        // sync with scopes
        self.latest_var_ids
            .retain(|_, v| self.scopes.contains_key(v.get_name()));
        // sync with latest_var_ids
        self.vars
            .retain(|k, _| self.latest_var_ids.contains_key(k.get_name()));
    }

    fn get_scope(&self, name: &str) -> Option<Scope> {
        self.scopes.get(name).copied()
    }

    fn get_uniq_id(&self, name: &str) -> VarId {
        if let Some(latest) = self.latest_var_ids.get(name) {
            let mut tmp = VarId::new(name);
            tmp.set_count(latest.get_count() + 1);
            tmp
        } else {
            VarId::new(name)
        }
    }

    fn get_reg_spec(&self, name: &str) -> Option<RegSpec> {
        self.reg_specs.get(name).cloned()
    }

    fn get_var(&self, name: &str) -> Option<PureRef> {
        self.latest_var_ids
            .get(name)
            .and_then(|id| self.vars.get(id).cloned())
    }

    fn set_var(&mut self, var: PureRef) -> Result<()> {
        let (scope, id) = var.expect_var()?;
        let name = id.get_name();
        if let Some(old_scope) = self.scopes.insert(name.to_string(), scope) {
            if old_scope != scope {
                return Err(RzILError::InconsistentScope(
                    name.to_string(),
                    old_scope,
                    scope,
                ));
            }
        }
        if let Some(old_id) = self.latest_var_ids.insert(name.to_string(), id.clone()) {
            if old_id == id {
                return Err(RzILError::ImmutableVariable(old_id.get_uniq_name()));
            }
        }
        if self.vars.insert(id.clone(), var).is_some() {
            return Err(RzILError::ImmutableVariable(id.get_uniq_name()));
        }
        Ok(())
    }

    fn add_register(&mut self, reg: &RegSpec) {
        self.scopes
            .insert(reg.get_name().to_string(), Scope::Global);
        self.reg_specs
            .insert(reg.get_name().to_string(), reg.clone());
    }

    fn concretize_register(&mut self, name: &str, val: u64) {
        assert!(self.reg_specs.get(name).is_some());
        let reg = self.get_var(name).unwrap();
        reg.concretize(val)
    }

    fn remove_var(&mut self, name: &str) -> Option<PureRef> {
        self.latest_var_ids
            .remove(name)
            .and_then(|id| self.vars.remove(&id))
    }
}

#[cfg(test)]
mod test {
    use super::{VarId, VarStorage, Variables};
    use crate::rzil::{
        builder::{RzILBuilder, RzILCache},
        error::RzILError,
        {Scope, Sort},
    };

    #[test]
    fn set_var() {
        let builder = RzILBuilder::new();
        let mut vars = VarStorage::new();
        assert_eq!(vars.get_var("a"), None); // no variables set
        let var = builder.new_unconstrained(Sort::Bool, vars.get_uniq_id("a"));
        vars.set_var(var.clone()).unwrap();
        assert_eq!(vars.get_scope("a"), Some(Scope::Global)); // scope is global
        assert_eq!(
            // able to get the variable
            vars.get_var("a"),
            Some(var)
        );
        let false_ = builder.new_const(Sort::Bool, 0);
        let invalid_var = builder.new_let_var(VarId::new("a"), false_);
        assert!(matches!(
            // unable to set vars with the same name and different scopes
            vars.set_var(invalid_var),
            Err(RzILError::InconsistentScope(_, Scope::Global, Scope::Let))
        ));
    }

    #[test]
    fn clear() {
        let builder = RzILBuilder::new();
        let mut vars = VarStorage::new();
        let false_ = builder.new_const(Sort::Bool, 0);
        vars.set_var(builder.new_unconstrained(Sort::Bool, vars.get_uniq_id("a")))
            .unwrap();
        vars.set_var(builder.new_var(Scope::Local, VarId::new("b"), false_))
            .unwrap();
        assert!(vars.get_scope("a").is_some());
        assert!(vars.get_scope("b").is_some());
        assert!(vars.get_var("a").is_some());
        assert!(vars.get_var("b").is_some());
        vars.clear_local();
        assert!(vars.get_scope("a").is_some());
        assert!(vars.get_scope("b").is_none());
        assert!(vars.get_var("a").is_some());
        assert!(vars.get_var("b").is_none());
        vars.clear();
        assert!(vars.get_scope("a").is_none());
        assert!(vars.get_var("a").is_none());
    }

    #[test]
    fn var_id_new() {
        let id = VarId::new("test");
        assert_eq!(id.get_name(), "test");
        assert_eq!(id.get_count(), 0);
        assert_eq!(id.get_uniq_name(), "test!0");
    }

    #[test]
    fn var_id_count() {
        let builder = RzILBuilder::new();
        let mut vars = VarStorage::new();

        vars.set_var(builder.new_unconstrained(Sort::Bool, vars.get_uniq_id("a")))
            .unwrap();
        let (_, id) = vars.get_var("a").unwrap().expect_var().unwrap();
        assert_eq!(id.get_name(), "a");
        assert_eq!(id.get_count(), 0);
        assert_eq!(id.get_uniq_name(), "a!0");

        vars.set_var(builder.new_unconstrained(Sort::Bool, vars.get_uniq_id("a")))
            .unwrap();
        let (_, id) = vars.get_var("a").unwrap().expect_var().unwrap();
        assert_eq!(id.get_name(), "a");
        assert_eq!(id.get_count(), 1);
        assert_eq!(id.get_uniq_name(), "a!1");
    }
}
