use crate::rzil::{
    Sort,
    PureRef, 
    builder::RzILBuilder
};
use crate::solver::Solver;
use crate::utils::PagedIntervalMap;
use crate::error::RiseResult;
use rangemap::RangeMap;
use rzapi::structs::Endian;
use std::cell::Cell;
use std::fmt::Debug;
use std::ops::Range;
use std::rc::Rc;

// Byte-wise memory entry of its symbolic address and content
#[derive(Clone, Debug, Eq, PartialEq)]
struct MemoryEntry {
    addr: PureRef,
    val: PureRef,
    t: i64,             // timestamp
}

#[derive(Clone, Debug)]
pub struct Memory {
    symbolic: PagedIntervalMap<MemoryEntry>,
    concrete: Rc<RangeMap<u64, MemoryEntry>>,
    t_pos: Cell<i64>, // timestamp which increases
    t_neg: Cell<i64>, // timestamp which decreases
    endian: Endian,
}

// ref: http://season-lab.github.io/papers/memsight-ase17.pdf
// ref: https://github.com/season-lab/memsight/blob/master/docs/pseudocode/naive-v4/main.pdf
impl Memory {
    /// Create a new memory instance
    pub fn new(endian: Endian) -> Self {
        Memory {
            //solver: Rc::downgrade(&solver),
            symbolic: PagedIntervalMap::<MemoryEntry>::new(),
            concrete: Rc::new(RangeMap::new()),
            t_pos: Cell::new(0),
            t_neg: Cell::new(0),
            endian,
        }
    }

    /// Write n bytes of memory from a certain location
    pub fn store(&mut self,
                 solver: &dyn Solver,
                 rzil: &RzILBuilder,
                 addr: PureRef,
                 val: PureRef) -> RiseResult<()> {
        assert!(val.get_size() > 0 && val.get_size() % 8 == 0);
        let n = u32::try_from(val.get_size() / 8).unwrap();
        for k in 0..n {
            let offset = rzil.new_const(Sort::Bitv(addr.get_size()), k as u64);
            let address = rzil.new_bvadd(addr.clone(), offset)?;
            let value = match self.endian {
                Endian::Little => rzil.new_extract(val.clone(), k + 7, k)?,
                Endian::Big => rzil.new_extract(
                    val.clone(), u64::BITS - k, u64::BITS - k - 7)?,
            };
            //extract val k k
            self._store(solver, rzil, address, value)?;
        }
        Ok(())
    }

    /// Write a byte of memory at a certain location
    fn _store(&mut self,
              solver: &dyn Solver,
              rzil: &RzILBuilder,
              addr: PureRef,
              val: PureRef) -> RiseResult<()> {
        self.t_pos.set(self.t_pos.get() + 1);
        let entry = MemoryEntry {
            addr: addr.clone(),
            val,
            t: self.t_pos.get(),
        };
        if addr.is_concretized() {
            Rc::make_mut(&mut self.concrete).insert(addr.evaluate()..addr.evaluate()+1, entry);
        } else {
            let a = solver.get_min(self, rzil, addr.clone())?;
            let b = solver.get_max(self, rzil, addr.clone())?;
            if a == b {
                Rc::make_mut(&mut self.concrete).insert(a..a+1, entry);
            } else {
                self.symbolic.insert(a..b, entry);
            }
        }
        Ok(())
    }

    /// Collect all admissible memory entries of given address ranges
    fn search<'a>(&'a self, range: &'a Range<u64>) -> Vec<&MemoryEntry> {
        self.concrete.overlapping(range).filter(|e| e.1.t <= self.t_pos.get())
            .chain(self.symbolic.overlapping(range).filter(|e| e.1.t <= self.t_pos.get()))
            .map(|(_r, e)| e).collect()
    }

    /// Read n bytes of memory at a certain location
    pub fn load(&self,
                solver: &dyn Solver,
                rzil: &RzILBuilder,
                addr: PureRef, n: u32) -> RiseResult<PureRef> {
        assert!(n > 0);
        let mut bytes = Vec::new();
        let address_range: Vec<u32> = match self.endian {
            Endian::Little => (0..n).collect(),
            Endian::Big => (0..n).rev().collect()
        };
        for k in &address_range {
            let offset = rzil.new_const(Sort::Bitv(addr.get_size()), *k as u64);
            bytes.push(rzil.new_bvadd(addr.clone(), offset)?);
        }
        let mut val = bytes.pop().unwrap();
        while let Some(byte) = bytes.pop() {
            val = rzil.new_append(val, self._load(solver, rzil, byte)?)?;
        }
        Ok(val)
    }

    /// Read a byte of memory at a certain location
    fn _load(&self,
             solver: &dyn Solver,
             rzil: &RzILBuilder,
             addr: PureRef) -> RiseResult<PureRef> {
        let a = solver.get_min(self, rzil, addr.clone())?;
        let b = solver.get_max(self, rzil, addr.clone())?;
        let range = a..b;
        let mut entries = self.search(&range);
        entries.sort_by(|l, r| l.t.cmp(&r.t));
        // TODO symbolize this
        let mut v = rzil.new_const(Sort::Bitv(8), 0);
        self.t_neg.set(self.t_neg.get() - 1);
        for e in &entries {
            let equiv = rzil.new_eq(e.addr.clone(), addr.clone())?;
            v = rzil.new_ite(equiv, e.val.clone(), v)?;
        }
        Ok(v)
    }

    //fn merge()
}
