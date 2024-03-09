use crate::error::{Result, RiseError};
use crate::rzil::{variables::Variables, Sort};
use rzapi::{api::RzApi, structs::RegisterType};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct RegSpec {
    name: String,
    sort: Sort,
    upper: u32,
    lower: u32,
    parent: String,
    reg_type: RegisterType,
}

impl RegSpec {
    pub fn get_name(&self) -> String {
        self.name.to_string()
    }
}

fn parse_reg_info(rzapi: &mut RzApi) -> Result<Vec<RegSpec>> {
    let mut min_offsets = HashMap::<u64, (u64, &str)>::new();
    let mut reg_specs = Vec::new();
    let reg_info = match rzapi.get_analysis_registers() {
        Ok(reg_profile) => reg_profile.reg_info,
        Err(e) => return Err(RiseError::RzApi(e)),
    };
    for r in &reg_info {
        match min_offsets.get(&r.type_id) {
            Some((offset, _)) => {
                if r.offset > offset.clone() {
                    ()
                } else {
                    min_offsets.insert(r.type_id, (r.offset, &r.name));
                }
            }
            None => {
                min_offsets.insert(r.type_id, (r.offset, &r.name));
            }
        };
    }
    for r in &reg_info {
        let (min_offset, parent) = min_offsets.get(&r.type_id).unwrap();
        let lower: u32 = (r.offset - min_offset).try_into().unwrap();
        let upper: u32 = (lower as usize + r.size - 1).try_into().unwrap();
        let sort = match r.reg_type {
            RegisterType::Flg => {
                if r.size == 1 {
                    Sort::Bool
                } else {
                    Sort::Bitv(r.size)
                }
            }
            _ => Sort::Bitv(r.size),
        };
        reg_specs.push(RegSpec {
            name: r.name.to_string(),
            sort,
            upper,
            lower,
            parent: parent.to_string(),
            reg_type: r.reg_type.clone(),
        })
    }
    Ok(reg_specs)
}

pub fn bind_registers(rzapi: &mut RzApi, vars: &mut Variables) -> Result<()> {
    let reg_specs = parse_reg_info(rzapi)?;
    for r in reg_specs.into_iter() {
        vars.add_register(r);
    }
    Ok(())
}
