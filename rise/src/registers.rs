use crate::error::{Result, RiseError};
use crate::map::interval_map::IntervalMap;
use crate::rzil::ast::Sort;
use crate::rzil::builder::RzILBuilder;
use crate::variables::Variables;
use rzapi::structs::RegisterInfo;
use rzapi::{api::RzApi, structs::RegisterType};

#[derive(Clone, Debug)]
pub struct RegSpec {
    name: String,
    sort: Sort,
    upper: u32,
    lower: u32,
    parent: String,
    primary: bool,
    reg_type: RegisterType,
}

impl RegSpec {
    pub fn get_name(&self) -> &str {
        &self.name
    }
    pub fn get_sort(&self) -> Sort {
        self.sort
    }
}

fn parse_reg_info(rzapi: &mut RzApi) -> Result<Vec<RegSpec>> {
    let mut largest_regs = IntervalMap::<u64, &RegisterInfo>::new(false);
    let mut reg_specs = Vec::new();
    let reg_info = match rzapi.get_analysis_registers() {
        Ok(reg_profile) => reg_profile.reg_info,
        Err(e) => return Err(RiseError::RzApi(e)),
    };
    for r in &reg_info {
        let range = r.offset..r.offset + r.size as u64;
        let largest = largest_regs.get_key_value(&r.offset);
        if largest.is_none()
            || largest.is_some_and(|(k, v)| {
                (range.start < k.start || k.end < range.start) && v.size < r.size
            })
        {
            largest_regs.insert(range, r);
        }
    }
    for r in &reg_info {
        let largest = largest_regs.get(&r.offset).unwrap();
        let lower: u32 = (r.offset - largest.offset).try_into().unwrap();
        let upper: u32 = (lower as usize + r.size - 1).try_into().unwrap();
        let (sort, primary) = match r.reg_type {
            // primary register is
            // if flg -> smallest register
            // else -> largest regsiter
            RegisterType::Flg => {
                if r.size == 1 {
                    (Sort::Bool, true)
                } else {
                    (Sort::Bitv(r.size), false)
                }
            }
            _ => {
                let is_largest = largest.name == r.name;
                (Sort::Bitv(r.size), is_largest)
            }
        };
        reg_specs.push(RegSpec {
            name: r.name.to_string(),
            sort,
            upper,
            lower,
            parent: largest.name.to_string(),
            primary,
            reg_type: r.reg_type,
        })
    }
    Ok(reg_specs)
}

pub fn bind_registers(
    rzapi: &mut RzApi,
    vars: &mut Variables,
    rzil: &impl RzILBuilder,
) -> Result<()> {
    let reg_specs = parse_reg_info(rzapi)?;
    for r in reg_specs.iter() {
        vars.add_register(r);
        if r.primary {
            let id = vars.get_uniq_id(r.get_name());
            let reg_var = rzil.new_unconstrained(r.get_sort(), id);
            vars.set_var(reg_var).unwrap();
        }
    }
    Ok(())
}
