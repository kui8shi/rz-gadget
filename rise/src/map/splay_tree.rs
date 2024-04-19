use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::ops::{Bound, RangeBounds};

#[derive(Clone, Debug)]
struct Node<K, V> {
    key: K,
    values: Vec<V>, // allow duplicated key
    id: usize,
    parent: Option<usize>,
    left: Option<usize>,
    right: Option<usize>,
}

impl<K: Ord, V> Node<K, V> {
    fn add_value(&mut self, value: V) {
        self.values.push(value);
    }
}

pub struct SplayTree<K, V> {
    array: Vec<Node<K, V>>,
    root: Option<usize>,
    allow_key_dup: bool,
}

impl<K: Clone, V: Clone> Clone for SplayTree<K, V> {
    fn clone(&self) -> Self {
        Self {
            array: self.array.to_vec(),
            root: self.root,
            allow_key_dup: self.allow_key_dup,
        }
    }
}
impl<K: Debug, V: Debug> Debug for SplayTree<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("{\n")?;
        self.pretty_print(self.root, 2, f)?; // called recursively
        f.write_str("}")
    }
}

impl<K: Debug, V: Debug> SplayTree<K, V> {
    fn pretty_print(
        &self,
        id: Option<usize>,
        indent: usize,
        f: &mut std::fmt::Formatter<'_>,
    ) -> std::fmt::Result {
        if let Some(node) = self.at(id) {
            self.pretty_print(self.right_of(id), indent + 2, f)?;
            f.write_fmt(format_args!(
                "{}{:?}: {:?} -> {:?}\n",
                " ".repeat(indent * 2),
                id.unwrap(),
                node.key,
                node.values
            ))?;
            self.pretty_print(self.left_of(id), indent + 2, f)?;
        }
        Ok(())
    }
}
impl<K, V> SplayTree<K, V> {
    pub fn new(allow_key_dup: bool) -> Self {
        Self {
            array: Vec::new(),
            root: None,
            allow_key_dup,
        }
    }
    pub fn new_ok_dup() -> Self {
        Self::new(true)
    }

    pub fn len(&self) -> usize {
        self.array.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_key_duplication_allowed(&self) -> bool {
        self.allow_key_dup
    }

    pub fn new_node(
        &mut self,
        key: K,
        value: V,
        left: Option<usize>,
        right: Option<usize>,
    ) -> Option<usize> {
        let new_id = self.array.len();
        self.array.push(Node {
            key,
            values: [value].into(),
            id: new_id,
            parent: None,
            left,
            right,
        });
        self.link_left(Some(new_id), left);
        self.link_right(Some(new_id), right);
        Some(new_id)
    }

    pub fn clear(&mut self) {
        self.array.clear();
        self.root = None;
    }

    pub fn iter(&self) -> Iter<'_, K, V> {
        Iter::new(
            self,
            self.left_most_of(self.root),
            self.right_most_of(self.root),
        )
    }

    pub fn into_iter(self) -> IntoIter<K, V> {
        let front = self.left_most_of(self.root);
        let back = self.right_most_of(self.root);
        IntoIter::new(self, front, back)
    }

    pub fn keys(&self) -> Keys<'_, K, V> {
        Keys {
            inner: Iter::new(
                self,
                self.left_most_of(self.root),
                self.right_most_of(self.root),
            ),
        }
    }

    pub fn values(&self) -> Values<'_, K, V> {
        Values {
            inner: Iter::new(
                self,
                self.left_most_of(self.root),
                self.right_most_of(self.root),
            ),
        }
    }

    pub(self) fn at(&self, id: Option<usize>) -> Option<&Node<K, V>> {
        id.map(|id| {
            debug_assert!(id < self.array.len());
            &self.array[id]
        })
    }

    pub(self) fn mut_at(&mut self, id: Option<usize>) -> Option<&mut Node<K, V>> {
        id.map(|id| {
            debug_assert!(id < self.array.len());
            &mut self.array[id]
        })
    }

    pub(self) fn parent_of(&self, id: Option<usize>) -> Option<usize> {
        self.at(id).and_then(|node| node.parent)
    }

    pub(self) fn left_of(&self, id: Option<usize>) -> Option<usize> {
        self.at(id).and_then(|node| node.left)
    }

    pub(self) fn right_of(&self, id: Option<usize>) -> Option<usize> {
        self.at(id).and_then(|node| node.right)
    }

    pub(self) fn ascend(&self, id: Option<usize>) -> Option<usize> {
        let right_leftmost = self.left_most_of(self.right_of(id));
        if right_leftmost.is_some() {
            right_leftmost
        } else {
            let mut traverse = id;
            while self.is_right_child(traverse) {
                // current node is a right child without any valid right nodes
                // the node with a larger key is the first parent
                // of which 'traverse' is a left child
                traverse = self.parent_of(traverse);
            }
            self.parent_of(traverse)
        }
    }

    pub(self) fn descend(&self, id: Option<usize>) -> Option<usize> {
        let left_rightmost = self.right_most_of(self.left_of(id));
        if left_rightmost.is_some() {
            left_rightmost
        } else if self.is_right_child(id) {
            // current node is a right child without any left nodes
            // the next node with a smaller key is the parent
            self.parent_of(id)
        } else {
            // current node is a left child without any left nodes
            // the next node with a smaller key is the grandparent
            self.parent_of(self.parent_of(id))
        }
    }

    fn is_left_child(&self, id: Option<usize>) -> bool {
        id.is_some() && self.left_of(self.parent_of(id)) == id
    }

    fn is_right_child(&self, id: Option<usize>) -> bool {
        id.is_some() && self.right_of(self.parent_of(id)) == id
    }

    fn left_most_of(&self, id: Option<usize>) -> Option<usize> {
        let left = self.left_of(id);
        if left.is_some() {
            self.left_most_of(left)
        } else {
            id
        }
    }

    fn right_most_of(&self, id: Option<usize>) -> Option<usize> {
        let right = self.right_of(id);
        if right.is_some() {
            self.right_most_of(right)
        } else {
            id
        }
    }

    fn link_left(&mut self, parent: Option<usize>, child: Option<usize>) {
        if let Some(n) = self.mut_at(parent) {
            n.left = child;
        }
        if let Some(n) = self.mut_at(child) {
            n.parent = parent;
        }
    }
    fn link_right(&mut self, parent: Option<usize>, child: Option<usize>) {
        if let Some(n) = self.mut_at(parent) {
            n.right = child;
        }
        if let Some(n) = self.mut_at(child) {
            n.parent = parent;
        }
    }

    // rotate the tree right to make the left node of 'root' the new root.
    //
    // #Panics
    // if the left child node of 'root' is None.
    fn rotate_right(&mut self, root: Option<usize>) -> Option<usize> {
        let left = self.left_of(root);
        self.link_left(root, self.right_of(left));
        self.link_right(left, root);
        if let Some(n) = self.mut_at(left) {
            n.parent = None;
        }
        left
    }

    // rotate the tree left to make the right child node of 'root' the new root.
    //
    // #Panics
    // if root node or right child node of 'root' is None.
    fn rotate_left(&mut self, root: Option<usize>) -> Option<usize> {
        let right = self.right_of(root);
        self.link_right(root, self.left_of(right));
        self.link_left(right, root);
        if let Some(n) = self.mut_at(right) {
            n.parent = None;
        }
        right
    }

    fn add_value(&mut self, id: Option<usize>, value: V) {
        if self.is_key_duplication_allowed() {
            self.mut_at(id).unwrap().values.push(value);
        } else {
            let _ = std::mem::replace(&mut self.mut_at(id).unwrap().values[0], value);
        }
    }
}

impl<K: Ord, V> SplayTree<K, V> {
    pub fn insert(&mut self, key: K, value: V) -> bool {
        self.root = self.splay(&key, self.root);
        if self.is_empty() {
            debug_assert!(self.root.is_none());
            self.root = self.new_node(key, value, None, None);
            return true;
        }
        debug_assert!(self.root.is_some());
        match key.cmp(&self.at(self.root).unwrap().key) {
            Ordering::Less => {
                // split at the left edge of root and
                // insert a new node as the new root
                let new_root = self.new_node(key, value, self.left_of(self.root), self.root);
                self.link_left(self.root, None);
                self.root = new_root;
                true
            }
            Ordering::Greater => {
                // split at the right edge of root and
                // insert a new node as the new root
                let new_root = self.new_node(key, value, self.root, self.right_of(self.root));
                self.link_right(self.root, None);
                self.root = new_root;
                true
            }
            Ordering::Equal => {
                // complete the insertion as the addition of 'value' to root
                self.add_value(self.root, value);
                true
            }
        }
    }

    pub fn get(&mut self, key: &K) -> Option<&[V]> {
        self.root = self.splay(key, self.root);
        if &self.at(self.root)?.key == key {
            let node = self.at(self.root)?;
            Some(&node.values)
        } else {
            None
        }
    }

    #[rustfmt::skip]
    // returns node iterator which fit in 'range'
    // this is meaningful only if the tree is ordered by 'T'
    pub fn range<T, R>(&self, range: R) -> Iter<'_, K, V>
    where
        T: Sized + Ord,
        K: Borrow<T> + Ord,
        R: RangeBounds<T>,
    {
        let (start, end) = (range.start_bound(), range.end_bound());
        match (start, end) {
            (Bound::Excluded(s), Bound::Excluded(e)) if s == e => {
                panic!("range start and end are equal and excluded in SplayTree");
            }
            (Bound::Included(s) | Bound::Excluded(s), Bound::Included(e) | Bound::Excluded(e))
                if s > e =>
            {
                panic!("range start is greater than range end in SplayTree");
            }
            _ => {}
        }
        let mut lower_bound = self.lower_bound(self.root, &start);
        let mut upper_bound = self.upper_bound(self.root, &end);
        if !self.at(lower_bound).is_some_and(|lower|
            self.at(upper_bound).is_some_and(|upper| lower.key <= upper.key)) {
            // if either bound is none or lower exceeds upper,
            // the tree has no keys in the range.
            lower_bound = None;
            upper_bound = None;
        }
        Iter::new(self, lower_bound, upper_bound)
    }

    fn find(&mut self, key: &K) -> Option<usize> {
        if &self.at(self.root)?.key == key {
            self.root
        } else {
            None
        }
    }

    // perform splay operation on the subtree of 'root' and return the new root.
    // if the subtree has a node with 'key', it becomes the new root.
    // else, the node last accessed becomes the new root.
    fn splay(&mut self, key: &K, root: Option<usize>) -> Option<usize> {
        let root_node = self.at(root)?;

        match key.cmp(&root_node.key) {
            Ordering::Less => self.splay_left(key, root),
            Ordering::Greater => self.splay_right(key, root),
            Ordering::Equal => root,
        }
    }

    // perform splay operation assuming 'key' is in the left subtree of 'root'.
    // return the new root.
    fn splay_left(&mut self, key: &K, root: Option<usize>) -> Option<usize> {
        let left = self.left_of(root);
        if left.is_none() {
            return root;
        }
        let left_node = self.at(left).unwrap();
        match key.cmp(&left_node.key) {
            Ordering::Less => {
                // zig-zig

                // make the left-left subtree have 'key' at its root by recursive splay operations
                let left_left = self.splay(key, self.left_of(left));
                self.link_left(left, left_left);

                // make the root of left-left subtree the new root by double right rotations
                let new_root = self.rotate_right(root);
                if self.left_of(new_root).is_some() {
                    self.rotate_right(new_root)
                } else {
                    new_root
                }
            }
            Ordering::Greater => {
                // zig-zag

                // make the left-right subtree have 'key' at its root by recursive splay operations
                let left_right = self.splay(key, self.right_of(left));
                self.link_right(left, left_right);

                // make the root of left-right subtree the new root by left and right rotations
                let left = if self.right_of(left).is_some() {
                    self.rotate_left(left)
                } else {
                    left
                };
                self.link_left(root, left);
                self.rotate_right(root)
            }
            Ordering::Equal => {
                // zig
                self.rotate_right(root)
            }
        }
    }

    // perform splay operation assuming 'key' is in the right subtree of 'root'.
    // return the new root.
    fn splay_right(&mut self, key: &K, root: Option<usize>) -> Option<usize> {
        let right = self.right_of(root);
        if right.is_none() {
            return root;
        }
        let right_node = self.at(right).unwrap();
        match key.cmp(&right_node.key) {
            Ordering::Greater => {
                // zig-zig

                // make the right-right subtree have 'key' at its root by recursive splay operations
                let right_right = self.splay(key, self.right_of(right));
                self.link_right(right, right_right);

                // make the root of right-right subtree the new root by double left rotations
                let new_root = self.rotate_left(root);
                if self.right_of(new_root).is_some() {
                    self.rotate_left(new_root)
                } else {
                    new_root
                }
            }
            Ordering::Less => {
                // zig-zag

                // make the right-left subtree have 'key' at its root by recursive splay operations
                let right_left = self.splay(key, self.left_of(right));
                self.link_left(right, right_left);

                // make the root of right-left subtree the new root by left and right rotations
                let right = if self.left_of(right).is_some() {
                    self.rotate_right(right)
                } else {
                    right
                };
                self.link_right(root, right);
                self.rotate_left(root)
            }
            Ordering::Equal => {
                // zig
                self.rotate_left(root)
            }
        }
    }

    fn lower_bound<T>(&self, id: Option<usize>, bound: &Bound<&T>) -> Option<usize>
    where
        T: Sized + Ord,
        K: Borrow<T>,
    {
        let node = self.at(id)?;
        match bound {
            // find lower bound
            Bound::Included(s) => match (*s).cmp(node.key.borrow()) {
                Ordering::Less => {
                    // [s < key : in the range. if left is none, it is the lower bound
                    self.lower_bound(self.left_of(id), bound).or(id)
                }
                Ordering::Greater => {
                    // key < [s : not in the range. go right
                    self.lower_bound(self.right_of(id), bound)
                }
                Ordering::Equal => {
                    // [s == key : in the range. the node is the lower bound of spanning range
                    id
                }
            },
            Bound::Excluded(s) => match (*s).cmp(node.key.borrow()) {
                Ordering::Less => {
                    // (s < key : in the range. if left is none, it is the lower bound
                    self.lower_bound(self.left_of(id), bound).or(id)
                }
                Ordering::Greater | Ordering::Equal => {
                    // key <= (s : not in the range. go right
                    self.lower_bound(self.right_of(id), bound)
                }
            },
            Bound::Unbounded => {
                // the left-most node is the lower bound
                self.lower_bound(self.left_of(id), bound).or(id)
            }
        }
    }

    fn upper_bound<T>(&self, id: Option<usize>, bound: &Bound<&T>) -> Option<usize>
    where
        T: Sized + Ord,
        K: Borrow<T>,
    {
        let node = self.at(id)?;
        match bound {
            // find upper bound
            Bound::Included(e) => match (*e).cmp(node.key.borrow()) {
                Ordering::Greater => {
                    // key < e] : in the range. if right is none, it is the upper bound
                    self.upper_bound(self.right_of(id), bound).or(id)
                }
                Ordering::Less => {
                    // e] < key : not in the range. go left
                    self.upper_bound(self.left_of(id), bound)
                }
                Ordering::Equal => {
                    // e] == key : in the range. the node is the upper bound of spanning range
                    id
                }
            },
            Bound::Excluded(e) => match (*e).cmp(node.key.borrow()) {
                Ordering::Greater => {
                    // key < e) : in the range. if right is none, it is the upper bound
                    self.upper_bound(self.right_of(id), bound).or(id)
                }
                Ordering::Less | Ordering::Equal => {
                    // e) <= key : not in the range. go left
                    self.upper_bound(self.left_of(id), bound)
                }
            },
            Bound::Unbounded => {
                // the right-most node is the upper bound
                self.upper_bound(self.right_of(id), bound).or(id)
            }
        }
    }
}

impl<K: Ord + Clone, V> SplayTree<K, V> {
    // remove the node with the exact key recieved if contained.
    pub fn remove(&mut self, key: &K) -> Option<Vec<V>> {
        self.root = Some(self.splay(key, self.root)?);
        if &self.at(self.root).unwrap().key != key {
            return None;
        }
        // save left and right children ids of root
        let mut left_root = self.left_of(self.root);
        let mut right_root = self.right_of(self.root);

        let removed_node = {
            // remove root node from splay tree's internal node array.
            // in the process, the last element of the array will be swapped
            // into the removed node's array position(see docs of 'Vec::swap_remove').
            let last = Some(self.array.last().unwrap().id);
            let root = self.root;
            {
                // rename the last node's id to removing(root) node's id,
                // because we are going to swap the array positions.
                if self.is_left_child(last) {
                    self.link_left(self.parent_of(last), root);
                } else if self.is_right_child(last) {
                    self.link_right(self.parent_of(last), root);
                }
                self.link_left(root, self.left_of(last));
                self.link_right(root, self.right_of(last));
                self.mut_at(last).unwrap().id = root.unwrap();
                if left_root == last {
                    left_root = root;
                }
                if right_root == last {
                    right_root = root;
                }
            }
            // remove root node
            self.root = None;
            self.array.swap_remove(root.unwrap())
        };

        if let Some(key) = self
            .at(self.right_most_of(left_root))
            .map(|left_max| &left_max.key)
        {
            // splay the max node of left subtree
            left_root = self.splay(&key.clone(), left_root);
        }

        self.root = if let Some(left_root_node) = self.mut_at(left_root) {
            // make the root of left subtree as the new root
            left_root_node.right = right_root;
            left_root
        } else {
            // make the root of right subtree as the new root
            right_root
        };

        // return owned values of the removed node
        Some(removed_node.values)
    }
}

// node iterator. front/back is a tuple
// that the first member is a reference to the node whose key is the target range's lower/upper bound,
// and the second member is the current index of the value vector of the nodes.
pub struct Iter<'a, K, V> {
    tree: &'a SplayTree<K, V>,
    front: (Option<usize>, usize), // (front node ref, node.value index)
    back: (Option<usize>, usize),  // (back node ref, node.value index)
}

impl<'a, K: 'a, V: 'a> Iter<'a, K, V> {
    pub(self) fn new(tree: &'a SplayTree<K, V>, front: Option<usize>, back: Option<usize>) -> Self {
        let initial_val_idx_front = 0;
        let initial_val_idx_back = tree.at(back).map_or(0, |n| n.values.len() - 1);
        Self {
            tree,
            front: (front, initial_val_idx_front),
            back: (back, initial_val_idx_back),
        }
    }
}

impl<'a, K: 'a, V: 'a> Iterator for Iter<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        let (front, val_idx) = self.front;
        debug_assert!(front.is_none() == self.tree.at(front).is_none());
        if val_idx < self.tree.at(front)?.values.len() - 1 {
            self.front = (front, val_idx + 1);
        } else if self.front == self.back {
            self.front = (None, 0);
            self.back = (None, 0);
        } else {
            let next = self.tree.ascend(front);
            let next_val_idx = 0;
            self.front = (next, next_val_idx);
        }
        let node = self.tree.at(front)?;
        Some((&node.key, &node.values[val_idx]))
    }
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for Iter<'a, K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let (back, val_idx) = self.back;
        if val_idx > 0 {
            self.back = (back, val_idx - 1);
        } else if self.front == self.back {
            self.front = (None, 0);
            self.back = (None, 0);
        } else {
            let next = self.tree.descend(back);
            let next_val_idx = self.tree.at(back).map_or(0, |n| n.values.len() - 1);
            self.back = (next, next_val_idx);
        }
        let node = self.tree.at(back)?;
        Some((&node.key, &node.values[val_idx]))
    }
}

pub struct IntoIter<K, V> {
    tree: SplayTree<K, V>,
    front: Option<usize>,
    back: Option<usize>,
}

impl<K, V> IntoIter<K, V> {
    pub(self) fn new(tree: SplayTree<K, V>, front: Option<usize>, back: Option<usize>) -> Self {
        Self { tree, front, back }
    }
}

impl<K: Clone, V> Iterator for IntoIter<K, V> {
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        if self.tree.at(self.front)?.values.is_empty() {
            if self.front == self.back {
                self.front = None;
                self.back = None;
            } else {
                self.front = self.tree.ascend(self.front);
            }
        }
        let node = self.tree.mut_at(self.front)?;
        Some((node.key.clone(), node.values.pop()?))
    }
}

impl<K: Clone, V> DoubleEndedIterator for IntoIter<K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.tree.at(self.back)?.values.is_empty() {
            if self.front == self.back {
                self.front = None;
                self.back = None;
            } else {
                self.back = self.tree.descend(self.back);
            }
        }
        let node = self.tree.mut_at(self.back)?;
        Some((node.key.clone(), node.values.pop()?))
    }
}

pub struct Keys<'a, K, V> {
    inner: Iter<'a, K, V>,
}

impl<'a, K: 'a, V: 'a> Iterator for Keys<'a, K, V> {
    type Item = &'a K;

    fn next(&mut self) -> Option<Self::Item> {
        let (front, _) = self.inner.front;
        let node = self.inner.tree.at(front)?;
        if self.inner.front == self.inner.back {
            self.inner.front = (None, 0);
            self.inner.back = (None, 0);
        } else {
            let next = self.inner.tree.ascend(front);
            let next_val_idx = 0;
            self.inner.front = (next, next_val_idx);
        }
        Some(&node.key)
    }
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for Keys<'a, K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let (back, _) = self.inner.back;
        let node = self.inner.tree.at(back)?;
        if self.inner.front == self.inner.back {
            self.inner.front = (None, 0);
            self.inner.back = (None, 0);
        } else {
            let next = self.inner.tree.descend(back);
            let next_val_idx = 0;
            self.inner.back = (next, next_val_idx);
        }
        Some(&node.key)
    }
}

pub struct Values<'a, K, V> {
    inner: Iter<'a, K, V>,
}

impl<'a, K: 'a, V: 'a> Iterator for Values<'a, K, V> {
    type Item = &'a V;

    fn next(&mut self) -> Option<Self::Item> {
        self.inner.next().map(|(_, v)| v)
    }
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for Values<'a, K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        self.inner.next_back().map(|(_, v)| v)
    }
}

pub fn detect_cycle<K, V>(tree: &SplayTree<K, V>) -> bool {
    debug_assert!(tree.root.is_none() == tree.is_empty());
    tree.root.is_some_and(|root| {
        let mut visited = vec![false; tree.len()];
        let mut finished = vec![false; tree.len()];
        dfs(tree, root, &mut visited, &mut finished)
    })
}

// return true if a cycle in the subtree of 'root' is detected.
fn dfs<K, V>(
    tree: &SplayTree<K, V>,
    root: usize,
    visited: &mut [bool],
    finished: &mut [bool],
) -> bool {
    visited[root] = true;
    for child in [tree.left_of(Some(root)), tree.right_of(Some(root))]
        .into_iter()
        .flatten()
    {
        if finished[child] {
            continue;
        }
        if visited[child] && !finished[child] {
            return true;
        }
        if dfs(tree, child, visited, finished) {
            return true;
        }
    }
    finished[root] = true;
    false
}

#[cfg(test)]
mod test {
    use crate::map::splay_tree::detect_cycle;

    use super::SplayTree;
    #[test]
    fn rotation() {
        let mut tree = SplayTree::new_ok_dup();
        tree.insert(100, "foo");
        tree.insert(200, "bar");
        let root = tree.root;
        tree.root = tree.rotate_right(tree.root);
        assert!(root != tree.root);
        tree.root = tree.rotate_left(tree.root);
        assert!(root == tree.root);
        assert!(!detect_cycle(&tree));
    }

    #[test]
    fn insertion() {
        let mut tree = SplayTree::new_ok_dup();
        tree.insert(100, "foo");
        tree.insert(100, "foo_foo");
        tree.insert(200, "bar");
        tree.insert(0, "zero");
        tree.insert(9200, "big");
        assert!(tree.get(&100) == Some(&["foo", "foo_foo"]));
        assert!(tree.get(&200) == Some(&["bar"]));
        assert!(tree.get(&0) == Some(&["zero"]));
        assert!(tree.get(&9200) == Some(&["big"]));
        assert!(!detect_cycle(&tree));
    }

    #[test]
    fn removal() {
        let mut tree = SplayTree::new_ok_dup();
        tree.insert(100, "foo");
        tree.insert(200, "bar");
        assert!(tree.remove(&100) == Some(Vec::from_iter(["foo"])));
        assert!(tree.remove(&200) == Some(Vec::from_iter(["bar"])));
        assert!(tree.is_empty());
        assert!(tree.root.is_none());
        assert!(!detect_cycle(&tree));
    }

    #[test]
    fn iter() {
        let mut tree = SplayTree::new_ok_dup();
        tree.insert(60, "foo");
        tree.insert(20, "bar");
        tree.insert(10, "a");
        tree.insert(20, "b");
        tree.insert(30, "c");
        tree.insert(40, "d");
        tree.insert(50, "e");
        tree.insert(60, "f");
        let mut iter = tree.iter();
        assert!(iter.next() == Some((&10, &"a")));
        assert!(iter.next() == Some((&20, &"bar")));
        assert!(iter.next() == Some((&20, &"b")));
        assert!(iter.next() == Some((&30, &"c")));
        assert!(iter.next() == Some((&40, &"d")));
        assert!(iter.next() == Some((&50, &"e")));
        assert!(iter.next() == Some((&60, &"foo")));
        assert!(iter.next() == Some((&60, &"f")));
        assert!(iter.next().is_none());
        assert!(iter.next_back().is_none());
        assert!(!detect_cycle(&tree));
    }

    #[test]
    fn into_iter() {
        let mut tree = SplayTree::new_ok_dup();
        tree.insert(60, "foo");
        tree.insert(20, "bar");
        tree.insert(10, "a");
        tree.insert(20, "b");
        tree.insert(30, "c");
        tree.insert(40, "d");
        tree.insert(50, "e");
        tree.insert(60, "f");
        assert!(!detect_cycle(&tree));
        let mut iter = tree.into_iter();
        assert!(iter.next_back() == Some((60, "f")));
        assert!(iter.next() == Some((10, "a")));
        assert!(iter.next_back() == Some((60, "foo")));
        assert!(iter.next() == Some((20, "b")));
        assert!(iter.next_back() == Some((50, "e")));
        assert!(iter.next() == Some((20, "bar")));
        assert!(iter.next_back() == Some((40, "d")));
        assert!(iter.next() == Some((30, "c")));
        assert!(iter.next_back().is_none());
        assert!(iter.next().is_none());
    }

    #[test]
    fn range() {
        let mut tree = SplayTree::new_ok_dup();
        tree.insert(61, "foo");
        tree.insert(21, "bar");
        tree.insert(10, "a");
        tree.insert(20, "b");
        tree.insert(30, "c");
        tree.insert(40, "d");
        tree.insert(50, "e");
        tree.insert(60, "f");
        assert!(!detect_cycle(&tree));

        let mut range = tree.range(20..60);
        assert!(range.next() == Some((&20, &"b")));
        assert!(range.next() == Some((&21, &"bar")));
        assert!(range.next() == Some((&30, &"c")));
        assert!(range.next() == Some((&40, &"d")));
        assert!(range.next() == Some((&50, &"e")));
        assert!(range.next().is_none());

        let mut range = tree.range(..30);
        assert!(range.next_back() == Some((&21, &"bar")));
        assert!(range.next_back() == Some((&20, &"b")));
        assert!(range.next_back() == Some((&10, &"a")));
        assert!(range.next().is_none());
    }
}
