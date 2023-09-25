use petgraph::graph::SymExp;

use rustproof_libsmt::backends::smtlib2::SMTLib2;
use rustproof_libsmt::backends::backend::SMTBackend;
use rustproof_libsmt::logics::qf_abv::{QF_ABV, bv_sort, array_sort, array_const};
use rustproof_libsmt::theories::{array_ex, bitvec, core};
use rzapi::structs::Endian;

use crate::memory::memory::Memory;
use petgraph::graph::SymExp;

// Not using address_width/endianness
#[derive(Clone, Debug)]
pub struct QWordMemory {
    map: Option<SymExp>,
    address_width: usize,
    endian: Endian,
}

impl Memory for QWordMemory {
    fn new(address_width: usize, endian: Endian) -> QWordMemory {
        QWordMemory { 
            map: None,
            address_width,
            endian,
        }
    }

    fn init_memory(&mut self, solver: &mut SMTLib2<QF_ABV>) {
        let bv_array = array_sort(bv_sort(64), 
                                  bv_sort(64));
        let idx_ = solver.new_var(Some("mem"), bv_array);
        // Set memory to all 0s
        let arr_const_ty = array_const(bv_sort(64),
                                       bv_sort(64),
                                       bitvec::OpCodes::Const(0, 64));

        let const_0 = solver.new_const(arr_const_ty);
        solver.assert(core::OpCodes::Cmp, &[idx_, const_0]);
        self.map = Some(idx_);
    }

    fn read(&mut self,
                addr: SymExp,
                read_size: usize,
                solver: &mut SMTLib2<QF_ABV>)
                -> SymExp {
        if self.map.is_none() {
            self.init_memory(solver);
        }
        let mem = self.map.unwrap();
        let idx = solver.assert(array_ex::OpCodes::Select, &[mem, addr]);
        if read_size < 64 {
            solver.assert(bitvec::OpCodes::Extract((read_size - 1) as u64, 1), &[idx])
        } else {
            idx
        }
    }

    fn write(&mut self,
                 addr: SymExp,
                 data: SymExp,
                 _write_size: usize,
                 solver: &mut SMTLib2<QF_ABV>) {
        if self.map.is_none() {
            self.init_memory(solver);
        }

        let mem = self.map.unwrap();
        let new_mem = solver.assert(array_ex::OpCodes::Store, &[mem, addr, data]);
        self.map = Some(new_mem);
    }
}

