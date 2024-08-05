use super::interval_map::IntervalMap;
use std::collections::HashMap;
use std::fmt::Debug;
use std::ops::Range;

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
    pages: HashMap<u64, IntervalMap<u64, V>>, // key is the starting address of a page
}

impl<V> PagedIntervalMap<V> {
    const PAGE_SIZE: u64 = 1024; // should be power of 2.

    pub fn new() -> Self {
        PagedIntervalMap {
            pages: HashMap::new(),
        }
    }

    /// Get a page iterator, ordered by page range.
    pub fn iter_page(&self) -> impl Iterator<Item = (Range<u64>, &IntervalMap<u64, V>)> {
        self.pages.iter().map(|(k, v)| (Self::to_page_range(k), v))
    }

    /// Get an entry iterator, ordered by entry range.
    pub fn iter(&self) -> impl Iterator<Item = (Range<u64>, &V)> {
        self.pages
            .iter()
            .flat_map(|(_, v)| v.iter().map(|(k, v)| (k.clone(), v)))
    }

    /// Remove all pages, including their entries.
    pub fn clear(&mut self) {
        self.pages.clear()
    }

    /// Return the number of pages.
    pub fn len(&self) -> usize {
        self.pages.len()
    }

    /// Return true if the map contains no pages.
    pub fn is_empty(&self) -> bool {
        self.pages.is_empty()
    }

    fn to_page_range(key: &u64) -> Range<u64> {
        let page_start = key & !(Self::PAGE_SIZE - 1);
        let page_end = key + Self::PAGE_SIZE;
        page_start..page_end
    }

    fn page_overlaps(page_key: &u64, range: &Range<u64>) -> bool {
        let page = Self::to_page_range(page_key);
        u64::max(page.start, range.start) < u64::min(page.end, range.end)
    }
}

impl<V> PagedIntervalMap<V> {
    fn get_page(&self, key: &u64) -> Option<&IntervalMap<u64, V>> {
        self.pages.get(key)
    }

    fn get_value(&self, key: &u64) -> Option<&V> {
        match self.pages.get(key) {
            Some(page) => page.get(key),
            None => None,
        }
    }

    fn get_page_range_and_page(&self, key: &u64) -> Option<(Range<u64>, &IntervalMap<u64, V>)> {
        self.pages
            .get_key_value(key)
            .map(|(page_range, page)| (Self::to_page_range(page_range), page))
    }

    fn get_range_and_value(&self, key: &u64) -> Option<(Range<u64>, &V)> {
        match self.pages.get(key) {
            Some(page) => page.get_key_value(key).map(|(k, v)| (k.clone(), v)),
            None => None,
        }
    }

    fn contains_page(&self, key: &u64) -> bool {
        self.pages.contains_key(key)
    }

    fn contains(&self, key: &u64) -> bool {
        match self.pages.get(key) {
            Some(page) => page.get(key).is_some(),
            None => false,
        }
    }
    /// Get a page iterator over all the stored ranges that are
    /// either partially or completely overlapped by the given range.
    fn overlapping_page<'a, 'b>(
        &'a self,
        range: &'b Range<u64>,
    ) -> impl Iterator<Item = (Range<u64>, &'a IntervalMap<u64, V>)> + 'b
    where
        'a: 'b,
    {
        self.pages.iter().filter_map(|(k, v)| {
            Self::page_overlaps(k, range).then_some((Self::to_page_range(k), v))
        })
    }

    /// Get an entry iterator over all the stored ranges that are
    /// either partially or completely overlapped by the given range.
    pub fn overlapping<'a, 'b>(
        &'a self,
        range: &'b Range<u64>,
    ) -> impl Iterator<Item = (Range<u64>, &'a V)> + 'b
    where
        'a: 'b,
    {
        self.pages
            .iter()
            .filter(|(k, _)| Self::page_overlaps(k, range))
            .flat_map(|(_, v)| v.overlapping(range).map(|(k, v)| (k.clone(), v)))
    }

    /// Return `true` if any range in the map completely or partially
    /// overlaps the given range.
    fn overlaps_page(&self, range: &Range<u64>) -> bool {
        self.overlapping_page(range).next().is_some()
    }
    /// Return `true` if any range in the sub-tree map completely or partially
    /// overlaps the given range.
    fn overlaps(&self, range: &Range<u64>) -> bool {
        self.overlapping(range).next().is_some()
    }
}

impl<V> PagedIntervalMap<V>
where
    V: Eq + Clone + Debug,
{
    const MAX_RANGE_SIZE: u64 = Self::PAGE_SIZE / 2;

    /// Insert a pair of key range and value into the map.
    ///
    /// Panics if range `start >= end`.
    pub fn insert(&mut self, range: Range<u64>, value: V) {
        let range = range.start..u64::min(range.end, range.start + Self::MAX_RANGE_SIZE);
        let mut page_start = range.start & !(Self::PAGE_SIZE - 1);
        while range.end > page_start {
            let page_end = page_start + Self::PAGE_SIZE;
            let entry_range = u64::max(page_start, range.start)..u64::min(page_end, range.end);
            if let Some(page) = self.pages.get_mut(&page_start) {
                page.insert(entry_range, value.clone());
            } else {
                let mut new_page = IntervalMap::<u64, V>::new(true);
                new_page.insert(entry_range, value.clone());
                self.pages.insert(page_start, new_page);
            }
            page_start += Self::PAGE_SIZE;
        }
    }

    /// Removes a range from the map, if all or any of it was present.
    ///
    /// Panics if range `start >= end`.
    pub fn remove(&mut self, range: Range<u64>) {
        assert!(range.start < range.end);
        let overlapping_pages = self.pages.iter_mut().filter_map(|(k, v)| {
            Self::page_overlaps(k, &range).then_some((Self::to_page_range(k), v))
        });

        for (page_range, page) in overlapping_pages {
            let start = u64::max(range.start, page_range.start);
            let end = u64::min(range.end, page_range.end);
            page.remove(start..end)
        }
    }
}
