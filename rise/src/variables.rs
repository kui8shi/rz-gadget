use crate::registers::RegSpec;
use std::collections::HashMap;

use crate::rzil::{
    ast::{PureRef, Scope},
    error::Result,
};

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct VarId {
    name: String,
    id: u32, // 0-index id that is unique among a set of variables with the same name
             // it works for registers as a counting number of write accesses
}

impl VarId {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            id: 0,
        }
    }

    pub fn get_name(&self) -> &str {
        &self.name
    }

    pub fn get_uniq_name(&self) -> String {
        format!("{}_{}", self.get_name(), self.get_count())
    }

    pub(self) fn get_count(&self) -> u32 {
        self.id
    }

    pub(self) fn set_count(&mut self, id: u32) {
        self.id = id
    }
}

#[derive(Clone, Debug)]
pub struct Variables {
    scopes: HashMap<String, Scope>,
    latest_var_ids: HashMap<String, VarId>,
    vars: HashMap<VarId, PureRef>,
    reg_specs: HashMap<String, RegSpec>,
}

impl Variables {
    pub fn new() -> Self {
        Variables {
            scopes: HashMap::new(),
            latest_var_ids: HashMap::new(),
            vars: HashMap::new(),
            reg_specs: HashMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.scopes.clear();
        self.latest_var_ids.clear();
        self.vars.clear();
        self.reg_specs.clear();
    }

    pub fn partial_clear(&mut self) {
        // clear vars with a local or smaller scope
        self.scopes.retain(|_, v| *v == Scope::Global);
        // sync with scopes
        self.latest_var_ids
            .retain(|_, v| self.scopes.contains_key(v.get_name()));
        // sync with latest_var_ids
        self.vars
            .retain(|k, _| self.latest_var_ids.contains_key(k.get_name()));
    }

    pub fn get_scope(&self, name: &str) -> Option<Scope> {
        self.scopes.get(name).map(|scope| *scope)
    }

    pub fn get_reg_spec(&self, name: &str) -> Option<&RegSpec> {
        self.reg_specs.get(name)
    }

    pub fn get_var(&self, name: &str) -> Option<PureRef> {
        self.latest_var_ids
            .get(name)
            .and_then(|id| self.vars.get(id).map(|v| v.clone()))
    }

    pub fn set_var(&mut self, var: PureRef) -> Result<()> {
        let (scope, mut id) = var.expect_var()?;
        if let Some(latest) = self.latest_var_ids.get(id.get_name()) {
            id.set_count(latest.get_count() + 1);
        }
        self.latest_var_ids
            .insert(id.get_name().to_string(), id.clone());
        self.vars.insert(id, var);
        Ok(())
    }

    pub fn add_register_spec(&mut self, reg: RegSpec) {
        self.reg_specs.insert(reg.get_name(), reg);
    }

    fn remove_var(&mut self, name: &str) -> Option<PureRef> {
        self.latest_var_ids
            .remove(name)
            .and_then(|id| self.vars.remove(&id))
    }
}
