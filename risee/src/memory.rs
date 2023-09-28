
use rzapi::structs::Endian;
use std::rc::Rc;
use crate::rzil::{Pure, RzILContext, Sort};
use std::fmt::Debug;
use crate::solver::Solver;
use crate::utils::IntervalTree;


/*
pub trait MemoryOperation {
    fn store(&mut self, ctx: &RzILContext, addr: Rc<Pure>, val: Rc<Pure>);
    fn load(&self, ctx: &RzILContext, addr: Rc<Pure>) -> Rc<Pure>;
}
*/

pub struct MemEntry {
    addr: Rc<Pure>,
    val: Rc<Pure>,
    t: i64,
    conditions: Vec<Rc<Pure>>
}

pub struct Memory {
    mem: IntervalTree<u64, MemEntry>,
    solver: Rc<Solver>,
    rzil: Rc<RzILContext>,
    t_pos: i64,
    t_neg: i64,
}

// ref: http://season-lab.github.io/papers/memsight-ase17.pdf
// ref: https://github.com/season-lab/memsight/blob/master/docs/pseudocode/naive-v4/main.pdf
impl MemSight {
    /// Create a new memory instance
    fn new(addr_width: usize, endian: Endian) -> Self;
    
    /// Write x bytes of memory from a certain location
    fn store(&mut self, ctx: &RzILContext, addr: Rc<Pure>, val: Rc<Pure>) {
        for k in 0..val.get_size() {
            let offset = ctx.new_const(Sort::Bitv(addr.get_size()), k as u64);
            let false_ = ctx.new_const(Sort::Bool, 0);
            let address = ctx.new_add(addr.clone(), offset); 
            let value = ctx.new_cast(val.clone(), false_, k);
            self._store(address, value)
        }
    }
    
    /// Write a byte of memory at a certain location
    fn _store(&mut self, addr: Rc<Pure>, val: Rc<Pure>) {
        let min = self.solver.min(addr);
        let max = self.solver.max(addr);
        self.t_pos += 1;
        self.insert((min, max), (addr, val, true))
    }

    /// Read x bytes of memory at a certain location
    fn load(&self, ctx: &RzILContext, addr: Rc<Pure>) -> Rc<Pure>;
}

