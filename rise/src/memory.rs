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
    timestamp: Cell<i64>, // timestamp which increases
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
    pub fn store(
        &mut self,
        solver: &dyn Solver,
        rzil: &RzILBuilder,
        addr: PureRef,
        val: PureRef,
    ) -> Result<()> {
        assert!(val.get_size() > 0 && val.get_size() % 8 == 0);
        let n = u32::try_from(val.get_size() / 8).unwrap();
        for k in 0..n {
            let offset = rzil.new_const(Sort::Bitv(addr.get_size()), k as u64);
            let address = rzil.new_bvadd(addr.clone(), offset)?;
            let value = match self.endian {
                Endian::Little => rzil.new_extract(val.clone(), k + 7, k)?,
                Endian::Big => rzil.new_extract(val.clone(), u64::BITS - k, u64::BITS - k - 7)?,
            };
            //extract val k k
            self._store(solver, rzil, address, value)?;
        }
        Ok(())
    }

    /// Write a byte of memory at a certain location
    fn _store(
        &mut self,
        solver: &dyn Solver,
        rzil: &RzILBuilder,
        addr: PureRef,
        val: PureRef,
    ) -> Result<()> {
        self.timestamp.set(self.timestamp.get() + 1);
        let entry = MemoryEntry {
            addr: addr.clone(),
            val,
            timestamp: self.timestamp.get(),
        };
        if addr.is_concretized() {
            Rc::make_mut(&mut self.concrete).insert(addr.evaluate()..addr.evaluate() + 1, entry);
        } else {
            let a = solver.get_min(self, rzil, addr.clone())?;
            let b = solver.get_max(self, rzil, addr.clone())?;
            if a == b {
                Rc::make_mut(&mut self.concrete).insert(a..a + 1, entry);
            } else {
                debug_assert!(a < b);
                self.symbolic.insert(a..b, entry);
            }
        }
        Ok(())
    }

    /// Collect all admissible memory entries of given address ranges
    fn search<'a>(&'a self, range: &'a Range<u64>) -> Vec<&MemoryEntry> {
        self.concrete
            .overlapping(range)
            .filter(|e| e.1.timestamp <= self.timestamp.get())
            .chain(
                self.symbolic
                    .overlapping(range)
                    .filter(|e| e.1.timestamp <= self.timestamp.get()),
            )
            .map(|(_r, e)| e)
            .collect()
    }

    /// Read n bytes of memory at a certain location
    ///
    /// Panics if n == 0
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
        let offsets: Vec<u64> = match self.endian {
            Endian::Little => (0..n).collect(),
            Endian::Big => (0..n).rev().collect(),
        };
        let mut value = None;
        for k in &offsets {
            let initial = if self.is_initial_memory_zero_filled {
                rzil.new_const(Sort::Bitv(8), 0)
            } else {
                // the format "{:#010x}" treats lower 32 bits of u64 as "0x........"
                // (the length is 10 chars, including "0x")
                self.implicit_timestamp.set(self.implicit_timestamp.get() - 1);
                let implicit_store = 
                    rzil.new_unconstrained(Sort::Bitv(8), format!("mem_{:#010x}", min_addr+k).as_ref());
                self._store(solver, rzil, addr, implicit_store.clone()); // TODO fix this
                implicit_store
            };
            let addr = rzil.new_bvadd(addr.clone(), rzil.new_const(Sort::Bitv(addr.get_size()), *k))?;
            let range = min_addr+k..max_addr+k+1;
            let loaded_byte = self._load(solver, rzil, addr, range, initial)?;
            value = if let Some(v) = value {
                Some(rzil.new_append(v, loaded_byte)?)
            } else {
                Some(loaded_byte)
            };
        }
        Ok(value.unwrap())
    }

    /// Read a byte of memory at a certain location
    fn _load(&self, solver: &dyn Solver, rzil: &RzILBuilder, addr: PureRef, range: Range<u64>, initial_value: PureRef) -> Result<PureRef> {
        let mut entries = self.search(&range);
        entries.sort_by(|l, r| l.timestamp.cmp(&r.timestamp)); // sort by timestamp
        // TODO symbolize this
        let mut data = initial_value;
        for e in &entries {
            let is_target_addr = rzil.new_eq(e.addr.clone(), addr.clone())?;
            data = rzil.new_ite(is_target_addr, e.val.clone(), data)?;
        }
        Ok(data)
    }

    //fn merge()
}
