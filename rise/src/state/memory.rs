use super::{
    solver::{Solver, Z3},
    StateZ3Backend,
};
use crate::{
    error::Result,
    map::{interval_map::IntervalMap, paged_map::PagedIntervalMap},
    rzil::{PureRef, Sort},
    variables::VarId,
};
use rzapi::structs::Endian;
use std::{cell::Cell, fmt::Debug, ops::Range};
//use bitflags::bitflags;

/*
bitflags! {
    #[derive(Clone, Debug, PartialEq, Copy, PartialOrd, Ord, Eq, Hash)]
    pub struct MemoryOptions: u8 {
        const InitialMemoryIsZeroFilled = 0b00000001;
    }
}
*/

const BITS_PER_BYTE: u32 = 8;

// Byte-wise memory entry of its symbolic address and content
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct MemoryEntry {
    addr: PureRef,
    val: PureRef,
    timestamp: i64,
}

/*
impl PartialEq for MemoryEntry {
    fn eq(&self, other: &Self) -> bool {
        self.val.eq(&other.val) && self.timestamp.eq(&other.timestamp)
    }
    fn ne(&self, other: &Self) -> bool {
        !self.eq(&other)
    }
}
*/

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
    concrete: IntervalMap<u64, MemoryEntry>,
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
            concrete: IntervalMap::new(true),
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
        if range.start + 1 == range.end {
            self.concrete.insert(range, entry);
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
    pub fn search(&self, range: &Range<u64>) -> Vec<&MemoryEntry> {
        let mut ret: Vec<&MemoryEntry> = self
            .concrete
            .overlapping(range)
            .filter(|e| e.1.timestamp <= self.timestamp.get())
            .map(|(k, v)| (k.clone(), v))
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

impl<S> StateZ3Backend<S> {
    /// Write a byte of memory at ranged location
    fn ranged_store(&mut self, range: Range<u64>, entry: MemoryEntry) -> Result<()> {
        self.memory.insert(range, entry);
        Ok(())
    }
}

pub trait MemoryOps {
    const MIN_ADDR: u64 = 0x4000;
    const MAX_ADDR: u64 = 0x80000000 - 1;
    fn store(&mut self, addr: PureRef, val: PureRef) -> Result<()>;
    fn load(&self, addr: PureRef, n_bytes: usize) -> Result<PureRef>;
}

impl<'ctx> MemoryOps for StateZ3Backend<Z3<'ctx>> {
    fn store(&mut self, addr: PureRef, val: PureRef) -> Result<()> {
        assert!(
            0 < val.get_size(),
            "cannot store bitvector with zero width."
        );
        assert!(
            val.get_size() <= 64,
            "cannot store bitvector larger than 64 bits."
        );
        assert_eq!(
            val.get_size() % 8,
            0,
            "cannot store bitvector not byte-sized."
        );
        let (min_addr, max_addr) = if addr.is_symbolized() {
            self.get_range(addr.clone())?
        } else {
            (addr.evaluate(), addr.evaluate())
        };
        assert!(min_addr <= max_addr, "min addr exceeds max addr");
        let n_bytes = (val.get_size() / BITS_PER_BYTE as usize) as u64;
        let timestamp = self.memory.timestamp();
        for k in 0..n_bytes {
            let addr = self.rzil.new_bvadd(
                addr.clone(),
                self.rzil.new_const(Sort::Bitv(addr.get_size()), k),
            )?;
            let min_ = min_addr.saturating_add(k).max(Self::MIN_ADDR);
            let max_ = max_addr.saturating_add(k + 1).min(Self::MAX_ADDR);
            let range = min_..max_;
            let byte = {
                let k_bits: u32 = (k * BITS_PER_BYTE as u64).try_into().unwrap();
                let (high, low) = match self.memory.endian() {
                    Endian::Little => (k_bits + 7, k_bits),
                    Endian::Big => (u64::BITS - k_bits - 1, u64::BITS - k_bits - 8),
                };
                //extract 8 bits(a byte) from val
                self.rzil.new_extract(val.clone(), high, low)?
            };
            let entry = MemoryEntry {
                addr,
                val: byte,
                timestamp,
            };
            self.ranged_store(range, entry)?;
        }
        dbg!(&self.memory);
        Ok(())
    }

    /// Read n bytes of memory at a certain location
    ///
    /// # Panics if n == 0
    fn load(&self, addr: PureRef, n_bytes: usize) -> Result<PureRef> {
        assert!(0 < n_bytes, "cannot load bitvector with zero width.");
        let (min_addr, max_addr) = if addr.is_symbolized() {
            self.get_range(addr.clone())?
        } else {
            (addr.evaluate(), addr.evaluate())
        };
        assert!(min_addr <= max_addr, "min addr exceeds max addr");
        let offsets: Vec<u64> = match self.memory.endian() {
            Endian::Little => (0..n_bytes as u64).collect(),
            Endian::Big => (0..n_bytes as u64).rev().collect(),
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
                // TODO enable to use this
                let implicit_store = self.rzil.new_unconstrained(
                    Sort::Bitv(8),
                    // the format "{:#010x}" treats lower 32 bits of u64 as "0x........"
                    // (the length is 10 chars, including "0x")
                    VarId::new(format!("mem_{:#010x}", min_addr + k).as_ref()), // TODO uniq memory id
                );
                /*
                let entry = MemoryEntry {
                    addr: addr.clone(),
                    val: implicit_store.clone(),
                    timestamp: self.memory.implicit_timestamp(),
                };
                self.ranged_store(range.clone(), entry); // TODO make this immutable call
                */
                implicit_store
            };
            let mut byte = initial;
            for e in &entries {
                let is_target_addr = self.rzil.new_eq(e.addr(), addr.clone())?;
                // check whether is the following condition satisfiable.
                byte = match self.check_assumptions(&[is_target_addr.clone()]) {
                    z3::SatResult::Unsat => byte,
                    z3::SatResult::Sat | z3::SatResult::Unknown => {
                        // entries are placed in nested if-then-else op structure in reverse order.
                        self.rzil.new_ite(is_target_addr, e.val(), byte)?
                    }
                };
            }
            loaded_value = if let Some(loaded) = loaded_value {
                Some(self.rzil.new_append(byte, loaded)?)
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
        rzil::{builder::RzILBuilder, Sort},
        state::{solver::Z3, State},
        variables::VarId,
    };

    use super::{MemoryOps, StateZ3Backend};

    #[test]
    fn concrete() {
        // init
        let rzil = RzILBuilder::new();
        let mut ctx = StateZ3Backend::<Z3>::new(rzil.clone(), None);

        // concrete store
        let addr = rzil.new_const(Sort::Bitv(64), 0x40000);
        let val = rzil.new_const(Sort::Bitv(64), 0xdeadbeaf);
        ctx.store(addr.clone(), val.clone()).unwrap();

        // load & assert
        let loaded = ctx.load(addr, 8).unwrap();
        assert_eq!(loaded, val);
    }

    #[test]
    fn symbolic() {
        // init
        let rzil = RzILBuilder::new();
        let mut ctx = StateZ3Backend::<Z3>::new(rzil.clone(), None);

        // symbolic store
        let x = rzil.new_unconstrained(Sort::Bitv(64), VarId::new("x"));
        let four = rzil.new_const(Sort::Bitv(64), 4);
        let addr = rzil.new_bvadd(x, four).unwrap();
        let val = rzil.new_const(Sort::Bitv(64), 0xdeadbeaf);
        ctx.store(addr.clone(), val.clone()).unwrap();

        // load & assert
        let loaded = ctx.load(addr, 8).unwrap();
        assert_eq!(loaded, val);
    }
}
