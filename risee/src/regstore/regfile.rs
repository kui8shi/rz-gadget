use std::collections::HashMap;

use crate::regstore::regstore::{RegStore, RegEntry};

use rzapi::structs::{RegisterProfile, RegisterType};
use rustproof_libsmt::backends::smtlib2::SMTLib2;
use rustproof_libsmt::logics::qf_abv;
use petgraph::graph::NodeIndex;

#[derive(Clone, Debug, Default)]
pub struct RzSymRegfile {
    current_regs: Vec<Option<NodeIndex>>,
    regfile: HashMap<String, RegEntry>,
}

impl RegStore for RzSymRegfile {
    type Ast = NodeIndex;

    fn new(reginfo: &mut RegisterProfile) -> RzSymRegfile {
        let mut current_regs = Vec::new();
        let mut regfile = HashMap::new();
        let mut seen_offsets = Vec::new();
        let mut alias_table = HashMap::new();
        reginfo.reg_info.sort_by(|x, y| (y.offset + y.size as u64).cmp(&(x.offset + x.size as u64)));
        for alias in &reginfo.alias_info {
            alias_table.insert(alias.reg.clone(), alias.role_str.clone());
        }
        for register in &reginfo.reg_info {
            let (idx, s_bit, e_bit, is_whole) = if !seen_offsets.contains(&register.offset) &&
                                                   (register.reg_type == RegisterType::Gpr || register.reg_type == RegisterType::Flg) {
                current_regs.push(None);
                seen_offsets.push(register.offset);
                (current_regs.len() - 1, 0, register.size - 1, true)
            } else {
                let mut found = 0;
                for (i, offset) in seen_offsets.iter().enumerate() {
                    if register.offset == *offset {
                        found = i;
                        break;
                    }
                }
                (found, 0, register.size - 1, false)
            };

            regfile.insert(register.name.clone(),
                           RegEntry::new(register.name.clone(), idx, s_bit, e_bit, is_whole, alias_table.remove(&register.name)));
        }

        RzSymRegfile {
            current_regs,
            regfile,
        }
    }

    fn read(&self, reg_name: &str, solver: &mut SMTLib2<qf_abv::QF_ABV>) -> NodeIndex {
        let rentry = &self.regfile.get(reg_name).expect("Unknown Register");
        let idx = self.current_regs[rentry.idx].expect("Unset register - Undefined Behavior. \
                                                        Consider setting an initial value before use!");
        /*
        if rentry.is_whole {
            idx
        } else {
            solver.assert(bitvec::OpCodes::Extract((rentry.end_bit) as u64, 0), &[idx])
        }
        */
        idx
    }

    // TODO: This is not totally correct as the sizes of registers may not match.
    fn write(&mut self, dest: &str, source: NodeIndex) {
        let rentry = &self.regfile[dest];
        self.current_regs[rentry.idx] = Some(source);
    }

    fn get_reg_entry(&self, r_string: &str) -> RegEntry {
        self.regfile[r_string].clone()
    }

    fn get_reg_ref(&self, r_string: &str) -> Option<NodeIndex> {
        let rentry = &self.regfile[r_string];
        self.current_regs[rentry.idx]
    }

    fn set_reg(&mut self, r_string: &str, cval: NodeIndex) {
        let idx = self.regfile[r_string].idx;
        self.current_regs[idx] = Some(cval);
    }
 
}
