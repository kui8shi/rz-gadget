use crate::map::splay_tree::detect_cycle;

use super::interval::Interval;
use super::splay_tree;
use std::cmp::Ordering;
use std::fmt::{self, Debug};
use std::ops::{Bound, Range};
#[derive(Clone)]
pub struct IntervalMap<K, V> {
    binary_tree: splay_tree::SplayTree<Interval<K>, V>,
}
impl<K, V> Default for IntervalMap<K, V> {
    fn default() -> Self {
        Self::new(false)
    }
}

impl<K, V> PartialEq for IntervalMap<K, V>
where
    K: PartialEq,
    V: PartialEq,
{
    fn eq(&self, other: &IntervalMap<K, V>) -> bool {
        self.expanded_iter().eq(other.expanded_iter())
    }
}

impl<K, V> PartialOrd for IntervalMap<K, V>
where
    K: PartialOrd,
    V: PartialOrd,
{
    #[inline]
    fn partial_cmp(&self, other: &IntervalMap<K, V>) -> Option<Ordering> {
        self.expanded_iter().partial_cmp(other.expanded_iter())
    }
}

impl<K, V> Ord for IntervalMap<K, V>
where
    K: Ord,
    V: Ord,
{
    #[inline]
    fn cmp(&self, other: &IntervalMap<K, V>) -> Ordering {
        self.expanded_iter().cmp(other.expanded_iter())
    }
}

impl<K, V> Eq for IntervalMap<K, V>
where
    K: Eq,
    V: Eq,
{
}

impl<K, V> IntervalMap<K, V> {
    /// Makes a new empty `IntervalMap`.
    #[cfg(not(feature = "const_fn"))]
    pub fn new(allow_key_dup: bool) -> Self {
        IntervalMap {
            binary_tree: splay_tree::SplayTree::new(allow_key_dup),
        }
    }

    /// Gets an iterator over all pairs of key range and value,
    /// ordered by key range.
    ///
    /// The iterator element type is `(&'a Range<K>, &'a V)`.
    pub fn iter(&self) -> Iter<'_, K, V> {
        Iter {
            inner: self.binary_tree.iter(),
        }
    }

    /// Clears the map, removing all elements.
    pub fn clear(&mut self) {
        self.binary_tree.clear();
    }

    /// Returns the number of elements in the map.
    pub fn len(&self) -> usize {
        self.binary_tree.len()
    }

    /// Returns true if the map contains no elements.
    pub fn is_empty(&self) -> bool {
        self.binary_tree.is_empty()
    }

    /// Returns an iterator that includes both ends of the key range.
    ///
    /// Mainly used for comparisons.
    fn expanded_iter(&self) -> impl Iterator<Item = (&K, &K, &V)> {
        self.binary_tree.iter().map(|(k, v)| (&k.start, &k.end, v))
    }
}

impl<K, V> IntervalMap<K, V>
where
    K: Ord + Clone + Debug,
{
    /// Returns a reference to the value corresponding to the given key,
    /// if the key is covered by any range in the map.
    pub fn get(&self, key: &K) -> Option<&V> {
        self.get_key_value(key).map(|(_range, value)| value)
    }

    /// Returns the range-value pair (as a pair of references) corresponding
    /// to the given key, if the key is covered by any range in the map.
    pub fn get_key_value(&self, key: &K) -> Option<(&Range<K>, &V)> {
        // The only stored range that could contain the given key is the
        // last stored range whose start is less than or equal to this key.
        self.binary_tree
            .range(..=key.clone())
            .next_back()
            .filter(|(interval, _value)| {
                // Does the only candidate range contain
                // the requested key?
                interval.contains(key)
            })
            .map(|(interval, value)| (interval.as_ref(), value))
    }

    /// Returns `true` if any range in the map covers the specified key.
    pub fn contains_key(&self, key: &K) -> bool {
        self.get(key).is_some()
    }

    /// Gets an iterator over all the maximally-sized ranges
    /// contained in `outer_range` that are not covered by
    /// any range stored in the map.
    ///
    /// If the start and end of the outer range are the same
    /// and it does not overlap any stored range, then a single
    /// empty gap will be returned.
    ///
    /// The iterator element type is `Range<K>`.
    pub fn gaps<'a>(&'a self, outer_range: &'a Range<K>) -> Gaps<'a, K, V> {
        Gaps {
            outer_range,
            keys: self.binary_tree.keys(),
            // We'll start the candidate range at the start of the outer range
            // without checking what's there. Each time we yield an item,
            // we'll skip any ranges we find before the next gap.
            candidate_start: &outer_range.start,
        }
    }

    /// Gets an iterator over all the stored ranges that are
    /// either partially or completely overlapped by the given range.
    pub fn overlapping<'a>(&'a self, range: &'a Range<K>) -> Overlapping<K, V> {
        // Find the first matching stored range by its start,
        // After that, another overlapping filter is imposed on the end of ranges
        // in Overlapping::next(). (See definition of 'next' in Iterator impl of Overlapping)
        let iter = self.binary_tree.range(range.clone());
        Overlapping {
            interval: Interval::new(range.clone()),
            iter,
        }
    }

    /// Returns `true` if any range in the map completely or partially
    /// overlaps the given range.
    pub fn overlaps(&self, range: &Range<K>) -> bool {
        self.overlapping(range).next().is_some()
    }
}

impl<K, V> IntervalMap<K, V>
where
    K: Ord + Clone + Debug,
    V: Eq + Clone + Debug,
{
    /// Insert a pair of key range and value into the map.
    ///
    /// # Panics
    ///
    /// Panics if range `start >= end`.
    pub fn insert(&mut self, range: Range<K>, new_val: V) {
        assert!(range.start < range.end);

        // Wrap up the given range so that we can "borrow"
        // it as a wrapper reference to either its start or end.
        // See `interval.rs` for explanation of these hacks.
        let mut new_intvl: Interval<K> = Interval::new(range);

        if !self.binary_tree.is_key_duplication_allowed() {
            // Is there a stored range either overlapping the start of
            // the range to insert or immediately preceding it?
            //
            // If there is any such stored range, it will be the last
            // whose start is less than or equal to the start of the range to insert,
            // or the one before that if both of the above cases exist.
            let mut candidates = self
                .binary_tree
                .range(..=new_intvl.start.clone())
                .rev()
                .take(2)
                .filter(|(stored_intvl, _stored_val)| {
                    // Does the candidate range either overlap
                    // or immediately precede the range to insert?
                    // (Remember that it might actually cover the whole
                    // range to insert and then some.)
                    stored_intvl.touches(&new_intvl)
                });
            if let Some(mut candidate) = candidates.next() {
                // Or the one before it if both cases described above exist.
                if let Some(another_candidate) = candidates.next() {
                    candidate = another_candidate;
                }
                let (stored_intvl, stored_val) = (candidate.0.clone(), candidate.1.clone());
                self.adjust_touching_ranges_for_insert(
                    stored_intvl,
                    stored_val,
                    &mut new_intvl,
                    &new_val,
                );
            }
            debug_assert!(!detect_cycle(&self.binary_tree));

            // Are there any stored ranges whose heads overlap or immediately
            // follow the range to insert?
            //
            // If there are any such stored ranges (that weren't already caught above),
            // their starts will fall somewhere after the start of the range to insert,
            // and on or before its end.
            //
            // This time around, if the latter holds, it also implies
            // the former so we don't need to check here if they touch.
            while let Some((stored_intvl, stored_val)) = self
                .binary_tree
                .range((
                    Bound::Included(new_intvl.start.clone()),
                    Bound::Included(new_intvl.end.clone()),
                ))
                .next()
            {
                // One extra exception: if we have different values,
                // and the stored range starts at the end of the range to insert,
                // then we don't want to keep looping forever trying to find more!
                //#[allow(clippy::suspicious_operation_groupings)]
                if stored_intvl.start == new_intvl.end && *stored_val != new_val {
                    // `adjust_touching_ranges_for_insert` below assumes that the given range
                    // is relevant, and behaves very poorly if it is handed a range which
                    // shouldn't be touching.
                    break;
                }

                self.adjust_touching_ranges_for_insert(
                    stored_intvl.clone(),
                    stored_val.clone(),
                    &mut new_intvl,
                    &new_val,
                );
            }
        }

        self.binary_tree.insert(new_intvl, new_val);
    }

    /// Removes a range from the map, if all or any of it was present.
    ///
    /// If the range to be removed _partially_ overlaps any ranges
    /// in the map, then those ranges will be contracted to no
    /// longer cover the removed range.
    ///
    /// # Panics
    ///
    /// Panics if range `start >= end`.
    pub fn remove(&mut self, range: Range<K>) {
        assert!(range.start < range.end);

        let intvl: Interval<K> = Interval::new(range);

        // Is there a stored range overlapping the start of
        // the range to remove?
        //
        // If there is any such stored range, it will be the last
        // whose start is less than or equal to the start of the range to remove.
        if let Some((stored_intvl, stored_val)) = self
            .binary_tree
            .range(..=intvl.start.clone())
            .next_back()
            .filter(|(stored_intvl, _stored_val)| {
                // Does the only candidate range overlap
                // the range to insert?
                stored_intvl.overlaps(&intvl)
            })
            .map(|(stored_intvl, stored_val)| (stored_intvl.clone(), stored_val.clone()))
        {
            self.adjust_overlapping_intervals_for_remove(stored_intvl, stored_val, &intvl);
        }

        // Are there any stored ranges whose heads overlap the range to remove?
        //
        // If there are any such stored ranges (that weren't already caught above),
        // their starts will fall somewhere after the start of the range to remove,
        // and before its end.
        //
        // REVISIT: Possible micro-optimisation: `impl Borrow<T> for Interval<T>`
        // and use that to search here, to avoid constructing another `Interval`.
        while let Some((stored_intvl, stored_val)) = self
            .binary_tree
            .range((
                Bound::Excluded(intvl.start.clone()),
                Bound::Excluded(intvl.end.clone()),
            ))
            .next()
            .map(|(stored_intvl, stored_val)| (stored_intvl.clone(), stored_val.clone()))
        {
            self.adjust_overlapping_intervals_for_remove(stored_intvl, stored_val, &intvl);
        }
    }

    fn adjust_touching_ranges_for_insert(
        &mut self,
        stored_intvl: Interval<K>,
        stored_val: V,
        new_intvl: &mut Interval<K>,
        new_val: &V,
    ) {
        use std::cmp::{max, min};
        if stored_val == *new_val {
            // The ranges have the same value, so we can "adopt"
            // the stored range.
            //
            // This means that no matter how big or where the stored range is,
            // we will expand the new range's bounds to subsume it,
            // and then delete the stored range.
            new_intvl.start = min(&new_intvl.start, &stored_intvl.start).clone();
            new_intvl.end = max(&new_intvl.end, &stored_intvl.end).clone();
            self.binary_tree.remove(&stored_intvl);
        } else {
            // The ranges have different values.
            if new_intvl.overlaps(&stored_intvl) {
                // Delete the stored range, and then add back between
                // 0 and 2 subranges at the ends of the range to insert.
                self.binary_tree.remove(&stored_intvl);
                if stored_intvl.start < new_intvl.start {
                    // Insert the piece left of the range to insert.
                    self.binary_tree.insert(
                        Interval::new(stored_intvl.start.clone()..new_intvl.start.clone()),
                        stored_val.clone(),
                    );
                }
                if stored_intvl.end > new_intvl.end {
                    // Insert the piece right of the range to insert.
                    self.binary_tree.insert(
                        Interval::new(new_intvl.end.clone()..stored_intvl.end.clone()),
                        stored_val,
                    );
                }
            } else {
                // No-op; they're not overlapping,
            }
        }
    }

    fn adjust_overlapping_intervals_for_remove(
        &mut self,
        stored: Interval<K>,
        stored_val: V,
        intvl_to_remove: &Interval<K>,
    ) {
        // Delete the stored range, and then add back between
        // 0 and 2 subranges at the ends of the range to insert.
        self.binary_tree.remove(&stored);
        if stored.start < intvl_to_remove.start {
            // Insert the piece left of the range to insert.
            self.binary_tree.insert(
                Interval::new(stored.range.start.clone()..intvl_to_remove.start.clone()),
                stored_val.clone(),
            );
        }
        if stored.range.end > intvl_to_remove.end {
            // Insert the piece right of the range to insert.
            self.binary_tree.insert(
                Interval::new(intvl_to_remove.end.clone()..stored.range.end.clone()),
                stored_val,
            );
        }
    }
}

pub struct Iter<'a, K, V> {
    inner: splay_tree::Iter<'a, Interval<K>, V>,
}

impl<'a, K, V> Iterator for Iter<'a, K, V>
where
    K: 'a,
    V: 'a,
{
    type Item = (&'a Range<K>, &'a V);

    fn next(&mut self) -> Option<(&'a Range<K>, &'a V)> {
        self.inner.next().map(|(k, v)| (&k.end_sorted.range, v))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

/// An owning iterator over the entries of a `IntervalMap`, ordered by key range.
///
/// The iterator element type is `(Range<K>, V)`.
///
/// This `struct` is created by the [`into_iter`] method on [`IntervalMap`]
/// (provided by the `IntoIterator` trait). See its documentation for more.
///
/// [`into_iter`]: IntoIterator::into_iter
pub struct IntoIter<K, V> {
    inner: splay_tree::IntoIter<Interval<K>, V>,
}

impl<K: Clone, V> IntoIterator for IntervalMap<K, V> {
    type Item = (Range<K>, V);
    type IntoIter = IntoIter<K, V>;
    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            inner: self.binary_tree.into_iter(),
        }
    }
}

impl<K: Clone, V> Iterator for IntoIter<K, V> {
    type Item = (Range<K>, V);
    fn next(&mut self) -> Option<(Range<K>, V)> {
        self.inner.next().map(|(k, v)| (k.end_sorted.range, v))
    }
    fn size_hint(&self) -> (usize, Option<usize>) {
        self.inner.size_hint()
    }
}

// We can't just derive this automatically, because that would
// expose irrelevant (and private) implementation details.
// Instead implement it in the same way that the underlying BTreeMap does.
impl<K: Debug, V: Debug> Debug for IntervalMap<K, V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries::<&std::ops::Range<K>, &V, self::Iter<'_, K, V>>(self.iter())
            .finish()
    }
}

impl<K, V> FromIterator<(Range<K>, V)> for IntervalMap<K, V>
where
    K: Ord + Clone + Debug,
    V: Eq + Clone + Debug,
{
    fn from_iter<T: IntoIterator<Item = (Range<K>, V)>>(iter: T) -> Self {
        let mut interval_map = IntervalMap::<K, V>::new(true);
        interval_map.extend(iter);
        interval_map
    }
}

impl<K, V> Extend<(Range<K>, V)> for IntervalMap<K, V>
where
    K: Ord + Clone + Debug,
    V: Eq + Clone + Debug,
{
    fn extend<T: IntoIterator<Item = (Range<K>, V)>>(&mut self, iter: T) {
        iter.into_iter().for_each(move |(k, v)| {
            self.insert(k, v);
        })
    }
}

/// An iterator over all ranges not covered by a `IntervalMap`.
///
/// The iterator element type is `Range<K>`.
///
/// This `struct` is created by the [`gaps`] method on [`IntervalMap`]. See its
/// documentation for more.
///
/// [`gaps`]: IntervalMap::gaps
pub struct Gaps<'a, K, V> {
    outer_range: &'a Range<K>,
    keys: splay_tree::Keys<'a, Interval<K>, V>,
    candidate_start: &'a K,
}

// `Gaps` is always fused. (See definition of `next` below.)
impl<'a, K, V> std::iter::FusedIterator for Gaps<'a, K, V> where K: Ord + Clone {}

impl<'a, K, V> Iterator for Gaps<'a, K, V>
where
    K: Ord + Clone,
{
    type Item = Range<K>;

    fn next(&mut self) -> Option<Self::Item> {
        for item in &mut self.keys {
            let range = &item.range;
            if range.end <= *self.candidate_start {
                // We're already completely past it; ignore it.
            } else if range.start <= *self.candidate_start {
                // We're inside it; move past it.
                self.candidate_start = &range.end;
            } else if range.start < self.outer_range.end {
                // It starts before the end of the outer range,
                // so move past it and then yield a gap.
                let gap = self.candidate_start.clone()..range.start.clone();
                self.candidate_start = &range.end;
                return Some(gap);
            }
        }

        // Now that we've run out of items, the only other possible
        // gap is at the end of the outer range.
        if *self.candidate_start < self.outer_range.end {
            // There's a gap at the end!
            let gap = self.candidate_start.clone()..self.outer_range.end.clone();
            // We're done; skip to the end so we don't try to find any more.
            self.candidate_start = &self.outer_range.end;
            Some(gap)
        } else {
            // We got to the end; there can't be any more gaps.
            None
        }
    }
}

/// An iterator over all stored ranges partially or completely
/// overlapped by a given range.
///
/// The iterator element type is `(&'a Range<K>, &'a V)`.
///
/// This `struct` is created by the [`overlapping`] method on [`IntervalMap`]. See its
/// documentation for more.
///
/// [`overlapping`]: IntervalMap::overlapping
pub struct Overlapping<'a, K, V> {
    interval: Interval<K>,
    iter: splay_tree::Iter<'a, Interval<K>, V>,
}

// `Overlapping` is always fused. (See definition of `next` below.)
impl<'a, K, V> core::iter::FusedIterator for Overlapping<'a, K, V> where K: Ord {}

impl<'a, K, V> Iterator for Overlapping<'a, K, V>
where
    K: Ord,
{
    type Item = (&'a Range<K>, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        self.iter
            .next()
            .filter(|(k, _)| self.interval.overlaps(&k))
            .map(|(k, v)| (&k.range, v))
    }
}

#[cfg(test)]
mod test {
    use super::IntervalMap;

    #[test]
    fn insertion() {
        let mut imap = IntervalMap::new(false);
        imap.insert(2..9, "a");
        imap.insert(4..8, "b");
        imap.insert(5..7, "c");
        assert!(imap.get(&1).is_none());
        assert!(imap.get(&2) == Some(&"a"));
        assert!(imap.get(&3) == Some(&"a"));
        assert!(imap.get(&4) == Some(&"b"));
        assert!(imap.get(&5) == Some(&"c"));
        assert!(imap.get(&6) == Some(&"c"));
        assert!(imap.get(&7) == Some(&"b"));
        assert!(imap.get(&8) == Some(&"a"));
        assert!(imap.get(&9).is_none());
    }

    #[test]
    fn insertion_overlap() {
        let mut imap = IntervalMap::new(false);
        imap.insert(640..704, "a");
        assert!(imap.get(&650) == Some(&"a"));
        imap.insert(320..384, "b");
        assert!(imap.get(&650) == Some(&"a"));
        imap.insert(704..768, "c");
        assert!(imap.get(&650) == Some(&"a"));
        assert!(imap.get(&700) == Some(&"a"));
        assert!(imap.get(&750) == Some(&"c"));
        assert!(imap.get(&600).is_none());
        assert!(imap.get(&350) == Some(&"b"));
    }
}
