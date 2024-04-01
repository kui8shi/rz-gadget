use super::solver::Solver;
use super::RiseContext;
use crate::error::Result;
use crate::map::interval_map::IntervalMap;
use crate::paged_map::PagedIntervalMap;
use crate::rzil::ast::{PureRef, Sort};
use crate::rzil::builder::RzILBuilder;
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
pub struct MemoryEntry {
    addr: PureRef,
    val: PureRef,
    timestamp: i64,
}

impl MemoryEntry {
    pub fn addr(&self) -> PureRef {
        self.addr.clone()
    }

    pub fn val(&self) -> PureRef {
        self.val.clone()
    }
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

    pub fn endian(&self) -> Endian {
        self.endian
    }

    pub fn timestamp(&self) -> i64 {
        self.timestamp.set(self.timestamp.get() + 1);
        self.timestamp.get()
    }

    pub fn insert(&mut self, range: Range<u64>, entry: MemoryEntry) {
        if entry.addr.is_concretized() {
            let mut_concrete = Rc::make_mut(&mut self.concrete);
            mut_concrete.insert(entry.addr.evaluate()..entry.addr.evaluate() + 1, entry);
        } else if range.start == range.end + 1 {
            let concrete = Rc::make_mut(&mut self.concrete);
            concrete.insert(range, entry);
        } else {
            self.symbolic.insert(range, entry);
        }
    }

    pub fn implicit_timestamp(&self) -> i64 {
        self.implicit_timestamp
            .set(self.implicit_timestamp.get() - 1);
        self.implicit_timestamp.get()
    }

    pub fn is_initial_memory_zero_filled(&self) -> bool {
        self.is_initial_memory_zero_filled
    }

    /// Collect all admissible memory entries of given address ranges
    pub fn search<'a>(&'a self, range: &'a Range<u64>) -> Vec<&MemoryEntry> {
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

    //fn merge()
}

pub trait MemoryWrite {
    fn store(&mut self, addr: PureRef, val: PureRef) -> Result<()>;
}

pub trait MemoryRead {
    fn load(&self, addr: PureRef, size: usize) -> Result<PureRef>;
}

impl MemoryWrite for RiseContext {
    fn store(&mut self, addr: PureRef, val: PureRef) -> Result<()> {
        assert!(
            val.get_size() > 0,
            "cannot store bitvector with zero width."
        );
        assert_eq!(
            val.get_size() % 8,
            0,
            "cannot store bitvector that is not byte-sized."
        );
        let (min_addr, max_addr) = if addr.is_symbolized() {
            (self.get_min(addr.clone())?, self.get_max(addr.clone())?)
        } else {
            (addr.evaluate(), addr.evaluate())
        };
        assert!(min_addr <= max_addr);
        let n = (val.get_size() / 8) as u64;
        for k in 0..n {
            let k_: u32 = k.try_into().unwrap();
            let addr = self.rzil.new_bvadd(
                addr.clone(),
                self.rzil.new_const(Sort::Bitv(addr.get_size()), k),
            )?;
            let range = min_addr + k..max_addr + k + 1;
            let byte = match self.memory.endian() {
                //extract 8 bits(a byte) from val
                Endian::Little => self.rzil.new_extract(val.clone(), k_ + 7, k_)?,
                Endian::Big => {
                    self.rzil
                        .new_extract(val.clone(), u64::BITS - k_, u64::BITS - k_ - 7)?
                }
            };
            let entry = MemoryEntry {
                addr,
                val: byte,
                timestamp: self.memory.timestamp(),
            };
            self.ranged_store(range, entry)?;
        }
        Ok(())
    }
}

impl RiseContext {
    /// Write a byte of memory at ranged location
    fn ranged_store(&mut self, range: Range<u64>, entry: MemoryEntry) -> Result<()> {
        self.memory.insert(range, entry);
        Ok(())
    }
}

impl MemoryRead for RiseContext {
    /// Read n bytes of memory at a certain location
    ///
    /// # Panics if n == 0
    fn load(&self, addr: PureRef, n: usize) -> Result<PureRef> {
        assert!(n > 0);
        let (min_addr, max_addr) = if addr.is_symbolized() {
            (self.get_min(addr.clone())?, self.get_max(addr.clone())?)
        } else {
            (addr.evaluate(), addr.evaluate())
        };
        assert!(min_addr <= max_addr);
        let offsets: Vec<u64> = match self.memory.endian() {
            Endian::Little => (0..n as u64).collect(),
            Endian::Big => (0..n as u64).rev().collect(),
        };
        let mut loaded_value = None;
        for k in &offsets {
            let addr = self.rzil.new_bvadd(
                addr.clone(),
                self.rzil.new_const(Sort::Bitv(addr.get_size()), *k),
            )?;
            let range = min_addr + k..max_addr + k + 1;
            let entries = self.memory.search(&range);
            let initial = if self.memory.is_initial_memory_zero_filled() {
                self.rzil.new_const(Sort::Bitv(8), 0)
            } else {
                // the format "{:#010x}" treats lower 32 bits of u64 as "0x........"
                // (the length is 10 chars, including "0x")
                let implicit_store = self.rzil.new_unconstrained(
                    Sort::Bitv(8),
                    format!("mem_{:#010x}", min_addr + k).as_ref(),
                );
                /*
                let entry = MemoryEntry {
                    addr: addr.clone(),
                    val: implicit_store.clone(),
                    timestamp: self.memory.implicit_timestamp(),
                };
                self.ranged_store(range.clone(), entry); // TODO fix this
                */
                implicit_store
            };
            let mut byte = initial;
            for e in &entries {
                // note: entries are placed in nested if-then-else op structure in reverse order.
                let is_target_addr = self.rzil.new_eq(e.addr(), addr.clone())?;
                byte = self.rzil.new_ite(is_target_addr, e.val(), byte)?;
            }
            loaded_value = if let Some(v) = loaded_value {
                Some(self.rzil.new_append(v, byte)?)
            } else {
                Some(byte)
            };
        }
        Ok(loaded_value.unwrap())
    }
}

#[cfg(test)]
mod test {
    use crate::{
        context::solver::Z3Solver,
        rzil::{
            ast::Sort,
            builder::{RzILBuilder, RzILCache},
        },
    };

    use super::{MemoryRead, MemoryWrite, RiseContext};

    #[test]
    fn concrete() {
        let solver = Z3Solver::new();
        let rzil = RzILCache::new();
        let mut ctx = RiseContext::new(solver, rzil.clone());
        let addr = rzil.new_const(Sort::Bitv(64), 0x40000);
        let val = rzil.new_const(Sort::Bitv(64), 0xdeadbeaf);
        ctx.store(addr.clone(), val.clone()).unwrap();
        assert_eq!(ctx.load(addr, 8).unwrap(), val);
    }
}
