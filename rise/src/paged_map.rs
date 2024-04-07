use crate::map::interval_map::IntervalMap;
use std::fmt::Debug;
use std::ops::Range;
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct PagedIntervalMap<V> {
    /*
     * rise internally uses IntervalMap to represent the symbolic memory.
     * IntervalMap holds byte-wise memory entires for feasible address ranges,
     * and is cloned whenever the symbolic state encounters branches and forks itself.
     * To suppress the cost of the fork operations, we introduce this struct; PagedIntervalMap.
     * It wraps IntervalMaps and enable it to clone itself without full-copy. Like a unix process,
     * this paged memory only full-copies needed pages as for needed accesses, such as adding,
     * replacing, or deleting a memory entry (in other words, copy-on-write).
     */
    pages: Rc<IntervalMap<u64, Rc<IntervalMap<u64, V>>>>,
}

impl<V> PagedIntervalMap<V> {
    pub fn new() -> Self {
        PagedIntervalMap {
            pages: Rc::new(IntervalMap::<u64, Rc<IntervalMap<u64, V>>>::new()),
        }
    }

    /// Get a page iterator, ordered by page range.
    pub fn iter_page(&self) -> impl Iterator<Item = (&Range<u64>, Rc<IntervalMap<u64, V>>)> {
        self.pages.iter().map(|(k, v)| (k, v.clone()))
    }

    /// Get an entry iterator, ordered by entry range.
    pub fn iter(&self) -> impl Iterator<Item = (&Range<u64>, &V)> {
        self.pages.iter().flat_map(|(_, v)| v.iter())
    }

    /// Remove all pages, including their entries.
    pub fn clear(&mut self) {
        Rc::<IntervalMap<u64, Rc<IntervalMap<u64, V>>>>::make_mut(&mut self.pages).clear()
    }

    /// Return the number of pages.
    pub fn len(&self) -> usize {
        self.pages.len()
    }

    /// Return true if the map contains no pages.
    pub fn is_empty(&self) -> bool {
        self.pages.is_empty()
    }
}

impl<V> PagedIntervalMap<V> {
    pub fn get_page(&self, key: &u64) -> Option<Rc<IntervalMap<u64, V>>> {
        self.pages.get(key).cloned()
    }

    pub fn get(&self, key: &u64) -> Option<&V> {
        match self.pages.get(key) {
            Some(page) => page.get(key),
            None => None,
        }
    }

    pub fn get_range_page(&self, key: &u64) -> Option<(&Range<u64>, &IntervalMap<u64, V>)> {
        self.pages
            .get_key_value(key)
            .map(|(page_range, page)| (page_range, page.as_ref()))
    }

    pub fn get_range(&self, key: &u64) -> Option<(&Range<u64>, &V)> {
        match self.pages.get(key) {
            Some(page) => page.get_key_value(key),
            None => None,
        }
    }

    pub fn contains_page(&self, key: &u64) -> bool {
        self.pages.get(key).is_some()
    }

    pub fn contains(&self, key: &u64) -> bool {
        match self.pages.get(key) {
            Some(page) => page.get(key).is_some(),
            None => false,
        }
    }
    /// Get a page iterator over all the stored ranges that are
    /// either partially or completely overlapped by the given range.
    pub fn overlapping_page<'a>(
        &'a self,
        range: &'a Range<u64>,
    ) -> impl Iterator<Item = (&Range<u64>, Rc<IntervalMap<u64, V>>)> {
        self.pages.overlapping(range).map(|(k, v)| (k, v.clone()))
    }

    /// Get an entry iterator over all the stored ranges that are
    /// either partially or completely overlapped by the given range.
    pub fn overlapping<'a>(
        &'a self,
        range: &'a Range<u64>,
    ) -> impl Iterator<Item = (&Range<u64>, &V)> {
        self.pages
            .overlapping(range)
            .flat_map(|(_, v)| v.overlapping(range))
    }

    /// Return `true` if any range in the map completely or partially
    /// overlaps the given range.
    pub fn overlaps_page(&self, range: &Range<u64>) -> bool {
        self.overlapping_page(range).next().is_some()
    }
    /// Return `true` if any range in the sub-tree map completely or partially
    /// overlaps the given range.
    pub fn overlaps(&self, range: &Range<u64>) -> bool {
        self.overlapping(range).next().is_some()
    }
}

impl<V> PagedIntervalMap<V>
where
    V: Eq + Clone + Debug,
{
    const PAGE_SIZE: u64 = 128; // should be power of 2.

    /// Insert a pair of key range and value into the map.
    ///
    /// Panics if range `start >= end`.
    pub fn insert(&mut self, range: Range<u64>, value: V) {
        assert!(range.start < range.end);
        let mut start = range.start & !(Self::PAGE_SIZE - 1);
        while range.end > start {
            let page_range = start..start + Self::PAGE_SIZE;
            let entry_range = start..u64::min(start + Self::PAGE_SIZE, range.end);

            if let Some(page) = self.get_page(&start) {
                Rc::<IntervalMap<u64, V>>::make_mut(&mut page.clone())
                    .insert(entry_range, value.clone());
            } else {
                let mut new_page = IntervalMap::<u64, V>::new();
                new_page.insert(entry_range, value.clone());
                Rc::<IntervalMap<u64, Rc<IntervalMap<u64, V>>>>::make_mut(&mut self.pages)
                    .insert(page_range, Rc::new(new_page));
            }

            start += Self::PAGE_SIZE;
        }
    }

    /// Removes a range from the map, if all or any of it was present.
    ///
    /// Panics if range `start >= end`.
    pub fn remove(&mut self, range: Range<u64>) {
        assert!(range.start < range.end);
        for (page_range, page) in self.overlapping_page(&range) {
            let start = u64::max(range.start, page_range.start);
            let end = u64::min(range.end, page_range.end);
            Rc::<IntervalMap<u64, V>>::make_mut(&mut page.clone()).remove(start..end)
        }
    }
}
