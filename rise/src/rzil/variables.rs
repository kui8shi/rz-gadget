use crate::registers::RegSpec;
use std::cell::Cell;
use std::collections::HashMap;

use super::{
    ast::{PureCode, PureRef, Scope},
    error::{Result, RzILError},
};

#[derive(Clone, Debug)]
pub struct Variables {
    uniq_var_id: Cell<u64>,
    latest_var_ids: HashMap<String, (u64, Scope)>,
    vars: HashMap<u64, PureRef>,
    reg_specs: HashMap<String, RegSpec>,
}

impl Variables {
    pub fn new() -> Self {
        Variables {
            uniq_var_id: Cell::new(0),
            latest_var_ids: HashMap::new(),
            vars: HashMap::new(),
            reg_specs: HashMap::new(),
        }
    }

    pub fn clear(&mut self) {
        self.uniq_var_id.set(0);
        self.latest_var_ids.clear();
        self.vars.clear();
        self.reg_specs.clear();
    }

    pub fn partial_clear(&mut self) {
        let ids_to_be_removed: std::collections::HashSet<u64> = self
            .latest_var_ids
            .iter()
            .filter(|(_, v)| v.1 != Scope::Global)
            .map(|(_, v)| v.0)
            .collect();
        self.latest_var_ids.retain(|_, v| v.1 == Scope::Global);
        self.vars.retain(|k, _| !ids_to_be_removed.contains(k));
    }

    pub fn get_scope(&self, name: &str) -> Option<Scope> {
        if let Some((_, scope)) = self.latest_var_ids.get(name) {
            Some(*scope)
        } else {
            None
        }
    }

    pub fn get_reg_spec(&self, name: &str) -> Option<&RegSpec> {
        self.reg_specs.get(name)
    }

    pub fn get_var(&self, name: &str) -> Option<(Scope, PureRef)> {
        if let Some((id, scope)) = self.latest_var_ids.get(name) {
            if let Some(var) = self.vars.get(id) {
                return Some((*scope, var.clone()));
            }
        }
        None
    }

    pub fn set_var(&mut self, var: PureRef) -> Result<()> {
        if let PureCode::Var(scope, name) = var.get_code() {
            let id = self.get_uniq_var_id();
            self.vars.insert(id, var);
            self.latest_var_ids.insert(name, (id, scope));
            Ok(())
        } else {
            Err(RzILError::UnexpectedCode("Var".to_string(), var.get_code()))
        }
    }

    pub fn add_register(&mut self, reg: RegSpec) {
        self.reg_specs.insert(reg.get_name(), reg);
    }

    fn get_uniq_var_id(&self) -> u64 {
        let id = self.uniq_var_id.get();
        self.uniq_var_id.set(self.uniq_var_id.get() + 1);
        id
    }

    fn remove_var(&mut self, name: &str) -> Option<PureRef> {
        if let Some((id, _)) = self.latest_var_ids.remove(name) {
            return self.vars.remove(&id);
        }
        None
    }
}
