use rangemap::{RangeMap, map::Gaps};
use std::rc::Rc;
use std::ops::Range;

#[derive(Clone, Debug)]
pub struct PagedRangeMap<V> {
    /*
     * We use rangemap::RangeMap as a representation of the symbolic memory.
     * The symbolic memory holds bit-wise memory entires for feasible address ranges.
     * And it is cloned whenever the symbolic state encounters branches and forks itself.
     * To suppress the cost of the fork operations, we introduce this struct; PagedRangeMap.
     * It nests RangeMaps and able to clone itself without full-copy. Like a unix process,
     * this paged memory only fully copies needed pages as needed for accesses, such as adding,
     * modifying, or deleting memory entires (in other words, copy-on-write).
     */
    pages: Rc<RangeMap<u64,Rc<RangeMap<u64,V>>>>,
}

impl<V> PagedRangeMap<V> {
    pub const fn new() -> Self {
        PagedRangeMap {
            pages: Rc::new(RangeMap::<u64, Rc<RangeMap<u64, V>>::new()),
        }
    }

    /// Gets a page iterator, ordered by page range.
    pub fn iter_page(&self) -> impl Iterator<Item = (&Range<u64>, &Rc<RangeMap<u64, V>>)> {
        self.pages.iter()
    }

    /// Gets an entry iterator, ordered by entry range.
    pub fn iter(&self) -> impl Iterator<Item = (&Range<u64>, &V)> {
        self.pages.iter().flat_map(|(k, v)| v.iter())
    }

    /// Remove all pages, including their entries.
    pub fn clear(&mut self) {
        self.pages.clear()
    }

    /// Returns the number of pages.
    pub fn len(&self) -> usize {
        self.pages.len()
    }

    /// Returns true if the map contains no pages.
    pub fn is_empty(&self) -> bool {
        self.pages.is_empty()
    }
}

impl<V> PagedRangeMap<V> 
where 
    u64: Ord + Clone,
{
    pub fn get_page(&self, key: &u64) -> Option<&Rc<RangeMap<u64, V>>> {
        self.pages.get(key)
    }

    pub fn get(&self, key: &u64) -> Option<&V> {
        match self.pages.get(key) {
            Some(page) => page.get(key),
            None => None
        }
    }

    pub fn get_range_page(&self, key: &u64) -> Option<(&Range<u64>, &Rc<RangeMap<u64, V>>)> {
        self.pages.get_key_value(key)
    }

    pub fn get_range(&self, key: &u64) -> Option<(&Range<u64>, &V)> {
        match self.pages.get(key) {
            Some(page) => page.get_key_value(key),
            None => None
        }
    }
    
    pub fn contains_page(&self, key: &u64) -> bool {
        self.get_page(key).is_some()
    }

    pub fn contains(&self, key: &u64) -> bool {
        self.get(key).is_some()
    }
    /// Gets a page iterator over all the stored ranges that are
    /// either partially or completely overlapped by the given range.
    pub fn overlapping_page<'a>(&'a self, range: &'a Range<u64>) -> impl Iterator<Item = (&Range<u64>, &Rc<RangeMap<u64, V>>)> {
        self.pages.overlapping(range)
    }

    /// Gets a entry iterator over all the stored ranges that are
    /// either partially or completely overlapped by the given range.
    pub fn overlapping<'a>(&'a self, range: &'a Range<u64>) -> impl Iterator<Item = (&Range<u64>, &V)> {
        self.pages.overlapping(range).flat_map(|(k, v)| v.overlapping(range))
    }
    /// Returns `true` if any range in the map completely or partially
    /// overlaps the given range.
    pub fn overlaps_page(&self, range: &Range<u64>) -> bool {
        self.overlapping_page(range).next().is_some()
    }
    /// Returns `true` if any range in the sub-tree map completely or partially
    /// overlaps the given range.
    pub fn overlaps(&self, range: &Range<u64>) -> bool {
        self.overlapping(range).next().is_some()
    }
}


impl<V> PagedRangeMap<V>
where
    V: Eq + Clone,
{
    const PAGE_SIZE: u64 = 128;
    /// Insert a pair of key range and value into the map.
    /// Panics if range `start >= end`.
    pub fn insert(&mut self, range: Range<u64>, value: V) {
        if let Some((range, page)) = self.overlapping_page(&range).next() {
            page.insert(range.clone(), value)
        } else {
            let mut start = range.start.clone() & (Self::PAGE_SIZE - 1);
            let end = (range.end.clone() | (Self::PAGE_SIZE - 1)) + 1;
            while end > start {
                let page_range = start .. start+Self::PAGE_SIZE;
                let entry_range = start .. u64::min(start+Self::PAGE_SIZE, end);
                let mut new_page = Rc::new(RangeMap::new());
                new_page.insert(entry_range, value);
                self.pages.insert(page_range, new_page);
                start += Self::PAGE_SIZE;
            }
        }
    }

    /// Removes a range from the map, if all or any of it was present.
    /// Panics if range `start >= end`.
    pub fn remove(&mut self, range: Range<u64>) {
        
    }
}
