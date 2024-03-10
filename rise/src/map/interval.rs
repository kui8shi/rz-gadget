use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::ops::{Deref, Range};

//
// Range wrapper start sorted
//

#[derive(Debug, Clone)]
pub struct Interval<T> {
    /*
     *  Wrapper of std::ops::Range
     *  Determines the inequality and the size relationships by start.
     */
    pub end_sorted: IntervalEndSorted<T>,
}

impl<T> Interval<T> {
    pub fn new(range: Range<T>) -> Self {
        Interval {
            end_sorted: IntervalEndSorted { range },
        }
    }
}

impl<T> Interval<T>
where
    T: Ord,
{
    pub fn overlaps(&self, other: &Self) -> bool {
        use core::cmp::{max, min};
        // Strictly less than, because ends are excluded.
        max(&self.start, &other.start) < min(&self.end, &other.end)
    }

    pub fn touches(&self, other: &Self) -> bool {
        use core::cmp::{max, min};
        // Less-than-or-equal-to because if one end is excluded, the other is included.
        // I.e. the two could be joined into a single range, because they're overlapping
        // or immediately adjacent.
        max(&self.start, &other.start) <= min(&self.end, &other.end)
    }
}

impl<T> PartialEq for Interval<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Interval<T>) -> bool {
        self.start == other.start
    }
}

impl<T> Eq for Interval<T> where T: Eq {}

impl<T> Ord for Interval<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Interval<T>) -> Ordering {
        match self.start.cmp(&other.start) {
            Ordering::Equal => self.end.cmp(&other.end),
            Ordering::Less => Ordering::Less,
            Ordering::Greater => Ordering::Greater,
        }
    }
}

impl<T> PartialOrd for Interval<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Interval<T>) -> Option<Ordering> {
        let start_comparision = self.start.partial_cmp(&other.start);
        if let Some(Ordering::Less) = start_comparision {
            start_comparision
        } else if let Some(Ordering::Greater) = start_comparision {
            start_comparision
        } else {
            self.end.partial_cmp(&other.end)
        }
    }
}

impl<T> Borrow<IntervalEndSorted<T>> for Interval<T> {
    fn borrow(&self) -> &IntervalEndSorted<T> {
        &self.end_sorted
    }
}

// Avoid the need to tediously plumb through the layers of wrapper structs
// when you're just trying to access members of the inner range itself.
impl<T> Deref for Interval<T> {
    type Target = IntervalEndSorted<T>;

    fn deref(&self) -> &Self::Target {
        &self.end_sorted
    }
}

impl<T> From<Range<T>> for Interval<T> {
    fn from(value: Range<T>) -> Self {
        Interval::new(value)
    }
}

//
// Range wrapper end sorted
//

#[derive(Debug, Clone)]
pub struct IntervalEndSorted<T> {
    pub range: Range<T>,
}

impl<T> IntervalEndSorted<T> {
    pub fn new(range: Range<T>) -> IntervalEndSorted<T> {
        IntervalEndSorted { range }
    }
}

impl<T> PartialEq for IntervalEndSorted<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &IntervalEndSorted<T>) -> bool {
        self.end == other.end
    }
}

impl<T> Eq for IntervalEndSorted<T> where T: Eq {}

impl<T> Ord for IntervalEndSorted<T>
where
    T: Ord,
{
    fn cmp(&self, other: &IntervalEndSorted<T>) -> Ordering {
        self.end.cmp(&other.end)
    }
}

impl<T> PartialOrd for IntervalEndSorted<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &IntervalEndSorted<T>) -> Option<Ordering> {
        self.end.partial_cmp(&other.end)
    }
}

// Avoid the need to tediously plumb through the layers of wrapper structs
// when you're just trying to access members of the inner range itself.
impl<T> Deref for IntervalEndSorted<T> {
    type Target = Range<T>;

    fn deref(&self) -> &Self::Target {
        &self.range
    }
}

#[cfg(test)]
mod test {
    use super::Interval;

    #[test]
    fn compare() {
        let a = Interval::new(2..19);
        let b = Interval::new(6..18);
        let c = Interval::new(2..18);
        assert!(a < b);
        assert!(b > c);
        assert!(a == c);
        assert!(a.end_sorted > b.end_sorted);
        assert!(b.end_sorted == c.end_sorted);
        assert!(a.end_sorted > c.end_sorted);
    }

    #[test]
    fn overlap_and_touch() {
        let a = Interval::new(2..19);
        let b = Interval::new(18..20);
        let c = Interval::new(19..20);
        assert!(a.overlaps(&b));
        assert!(!a.overlaps(&c));
        assert!(a.touches(&b));
        assert!(a.touches(&c));
    }
}
