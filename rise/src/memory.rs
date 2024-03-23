use crate::error::Result;
use crate::map::interval_map::IntervalMap;
use crate::paged_map::PagedIntervalMap;
use crate::rzil::{
    ast::{PureRef, Sort},
    builder::RzILBuilder,
};
use crate::solver::Solver;
use rzapi::structs::Endian;
use std::cell::Cell;
use std::fmt::Debug;
use std::ops::Range;
use std::rc::Rc;
//use bitflags::bitflags;

/*
bitflags! {
    #[derive(Clone, Debug, PartialEq, Copy, PartialOrd, Ord, Eq, Hash)]
    pub struct MemoryOptions: u8 {
        const InitialMemoryIsZeroFilled = 0b00000001;
    }
}
*/

// Byte-wise memory entry of its symbolic address and content
#[derive(Clone, Debug, Eq, PartialEq)]
struct MemoryEntry {
    addr: PureRef,
    val: PureRef,
    timestamp: i64,
}

#[derive(Clone, Debug)]
pub struct Memory {
    symbolic: PagedIntervalMap<MemoryEntry>,
    concrete: Rc<IntervalMap<u64, MemoryEntry>>,
    timestamp: Cell<i64>,          // timestamp which increases
    implicit_timestamp: Cell<i64>, // timestamp which decreases
    endian: Endian,
    is_initial_memory_zero_filled: bool,
}

// ref: http://season-lab.github.io/papers/memsight-ase17.pdf
// pesudo code: https://github.com/season-lab/memsight/blob/master/docs/pseudocode/naive-v4/main.pdf
impl Memory {
    /// Create a new memory instance
    pub fn new(endian: Endian) -> Self {
        Memory {
            //solver: Rc::downgrade(&solver),
            symbolic: PagedIntervalMap::<MemoryEntry>::new(),
            concrete: Rc::new(IntervalMap::new()),
            timestamp: Cell::new(0),
            implicit_timestamp: Cell::new(0),
            endian,
            is_initial_memory_zero_filled: true,
        }
    }

    /// Write arbitrary bytes of memory from a certain location
    ///
    /// # Panics if bit width of 'val' is not positive and multiple of 8.
    pub fn store(
        &mut self,
        solver: &dyn Solver,
        rzil: &RzILBuilder,
        addr: PureRef,
        val: PureRef,
    ) -> Result<()> {
        assert!(val.get_size() > 0 && val.get_size() % 8 == 0);
        let min_addr = solver.get_min(self, rzil, addr.clone())?;
        let max_addr = solver.get_max(self, rzil, addr.clone())?;
        assert!(min_addr > max_addr);
        let n = (val.get_size() / 8) as u64;
        for k in 0..n {
            let k_: u32 = k.try_into().unwrap();
            let addr =
                rzil.new_bvadd(addr.clone(), rzil.new_const(Sort::Bitv(addr.get_size()), k))?;
            let range = min_addr + k..max_addr + k + 1;
            let byte = match self.endian {
                //extract 8 bits(a byte) from val
                Endian::Little => rzil.new_extract(val.clone(), k_ + 7, k_)?,
                Endian::Big => rzil.new_extract(val.clone(), u64::BITS - k_, u64::BITS - k_ - 7)?,
            };
            self.timestamp.set(self.timestamp.get() + 1);
            self._store(solver, rzil, addr, byte, range, self.timestamp.get())?;
        }
        Ok(())
    }

    /// Write a byte of memory at ranged location
    fn _store(
        &mut self,
        solver: &dyn Solver,
        rzil: &RzILBuilder,
        addr: PureRef,
        val: PureRef,
        range: Range<u64>,
        timestamp: i64,
    ) -> Result<()> {
        let entry = MemoryEntry {
            addr: addr.clone(),
            val,
            timestamp,
        };
        if addr.is_concretized() {
            Rc::make_mut(&mut self.concrete).insert(addr.evaluate()..addr.evaluate() + 1, entry);
        } else {
            if range.start == range.end + 1 {
                Rc::make_mut(&mut self.concrete).insert(range, entry);
            } else {
                self.symbolic.insert(range, entry);
            }
        }
        Ok(())
    }

    /// Collect all admissible memory entries of given address ranges
    fn search<'a>(&'a self, range: &'a Range<u64>) -> Vec<&MemoryEntry> {
        let mut ret: Vec<&MemoryEntry> = self
            .concrete
            .overlapping(range)
            .filter(|e| e.1.timestamp <= self.timestamp.get())
            .chain(
                self.symbolic
                    .overlapping(range)
                    .filter(|e| e.1.timestamp <= self.timestamp.get()),
            )
            .map(|(_r, e)| e)
            .collect();
        ret.sort_by(|l, r| {
            l.timestamp
                .cmp(&r.timestamp)
                .then(l.addr.is_concretized().cmp(&r.addr.is_concretized()))
                .then(l.addr.evaluate().cmp(&r.addr.evaluate()))
        }); // sort by timestamp, symbolizd or concrete, and address
        ret
    }

    /// Read n bytes of memory at a certain location
    ///
    /// # Panics if n == 0
    pub fn load(
        &self,
        solver: &dyn Solver,
        rzil: &RzILBuilder,
        addr: PureRef,
        n: u64,
    ) -> Result<PureRef> {
        assert!(n > 0);
        let min_addr = solver.get_min(self, rzil, addr.clone())?;
        let max_addr = solver.get_max(self, rzil, addr.clone())?;
        assert!(min_addr > max_addr);
        let offsets: Vec<u64> = match self.endian {
            Endian::Little => (0..n).collect(),
            Endian::Big => (0..n).rev().collect(),
        };
        let mut loaded_value = None;
        for k in &offsets {
            let addr = rzil.new_bvadd(
                addr.clone(),
                rzil.new_const(Sort::Bitv(addr.get_size()), *k),
            )?;
            let range = min_addr + k..max_addr + k + 1;
            let entries = self.search(&range);
            let initial = if self.is_initial_memory_zero_filled {
                rzil.new_const(Sort::Bitv(8), 0)
            } else {
                self.implicit_timestamp
                    .set(self.implicit_timestamp.get() - 1);
                // the format "{:#010x}" treats lower 32 bits of u64 as "0x........"
                // (the length is 10 chars, including "0x")
                let implicit_store = rzil.new_unconstrained(
                    Sort::Bitv(8),
                    format!("mem_{:#010x}", min_addr + k).as_ref(),
                );
                self._store(
                    solver,
                    rzil,
                    addr,
                    implicit_store.clone(),
                    range,
                    self.implicit_timestamp.get(),
                ); // TODO fix this
                implicit_store
            };
            let mut byte = initial;
            for e in &entries {
                // note: entries are placed in nested if-then-else op structure in reverse order.
                let is_target_addr = rzil.new_eq(e.addr.clone(), addr.clone())?;
                byte = rzil.new_ite(is_target_addr, e.val.clone(), byte)?;
            }
            loaded_value = if let Some(v) = loaded_value {
                Some(rzil.new_append(v, byte)?)
            } else {
                Some(byte)
            };
        }
        Ok(loaded_value.unwrap())
    }
    //fn merge()
}
