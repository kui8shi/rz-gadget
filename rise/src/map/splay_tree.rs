use std::borrow::Borrow;
use std::cmp::Ordering;
use std::fmt::Debug;
use std::ops::{Bound, RangeBounds};

#[derive(Clone, Debug)]
struct Node<K, V> {
    key: K,
    value: Vec<V>, // allow duplicated key
    id: usize,
    parent: Option<usize>,
    left: Option<usize>,
    right: Option<usize>,
}

impl<K: Ord, V> Node<K, V> {
    fn add_value(&mut self, value: V) {
        self.value.push(value);
    }
}

pub struct SplayTree<K, V> {
    array: Vec<Node<K, V>>,
    root: Option<usize>,
}

impl<K: Clone, V: Clone> Clone for SplayTree<K, V> {
    fn clone(&self) -> Self {
        Self {
            array: self.array.to_vec(),
            root: self.root.clone(),
        }
    }
}
impl<K: Debug, V: Debug> Debug for SplayTree<K, V> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_print(self.root, 0, f)
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
            self.pretty_print(self.left_of(id), indent + 2, f)?;
            f.write_fmt(format_args!(
                "{}{:?} : {:?}",
                " ".repeat(indent * 2),
                node.key,
                node.value
            ))?;
            self.pretty_print(self.right_of(id), indent + 2, f)?;
        }
        Ok(())
    }
}
impl<K, V> SplayTree<K, V> {
    pub fn new() -> Self {
        Self {
            array: Vec::new(),
            root: None,
        }
    }

    pub fn len(&self) -> usize {
        self.array.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn new_node(
        &mut self,
        key: K,
        value: V,
        left: Option<usize>,
        right: Option<usize>,
    ) -> usize {
        let new_id = self.array.len();
        self.array.push(Node {
            key,
            value: [value].into(),
            id: new_id,
            parent: None,
            left,
            right,
        });
        self.relink(Some(new_id), Some(new_id));
        new_id
    }

    // delete node with 'id' from splay tree's internal node array.
    // in the deletion, the last element of the array will be swapped
    // into the deleted node's position(see 'Vec::swap_remove' document).
    // since the call is disruptive for the relationships between nodes and their ids,
    // please take care of your vars of node ids before calling this function.
    pub(self) fn delete_node(&mut self, id: Option<usize>) -> Option<Node<K, V>> {
        if self.is_empty() || !id.is_some_and(|id| id < self.len()) {
            return None;
        }
        let last = self.array.last().unwrap().id;
        debug_assert!(last == self.array.len() - 1);
        {
            // rename the last node's id to 'id',
            // because we are going to swap the array positions
            // of the deleting node and the last node by 'swap_remove'.
            self.relink(Some(last), id);
            self.array[last].id = id.unwrap();
            if self.root == Some(last) {
                self.root = id;
            }
        }
        Some(self.array.swap_remove(id.unwrap()))
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

    /*
    pub fn into_iter(self) -> IntoIter<K, V> {
        let front = self.left_most_of(self.root);
        let back = self.right_most_of(self.root);
        IntoIter::new(self, front, back)
    }
    */

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
        id.and_then(|id| {
            debug_assert!(id < self.array.len());
            debug_assert!(self.array[id].value.len() > 0);
            Some(&self.array[id])
        })
    }

    pub(self) fn mut_at(&mut self, id: Option<usize>) -> Option<&mut Node<K, V>> {
        id.and_then(|id| {
            debug_assert!(id < self.array.len());
            debug_assert!(self.array[id].value.len() > 0);
            Some(&mut self.array[id])
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
        } else if self.is_left_child(id) {
            // current node is a left child without any right nodes
            // the next node with a larger key is the parent
            self.parent_of(id)
        } else {
            // current node is a right child without any right nodes
            // the next node with a larger key is the grandparent
            self.parent_of(self.parent_of(id))
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

    fn relink(&mut self, old: Option<usize>, new: Option<usize>) {
        if self.is_left_child(old) {
            self.mut_at(self.parent_of(old)).map(|n| n.left = new);
        } else if self.is_right_child(old) {
            self.mut_at(self.parent_of(old)).map(|n| n.right = new);
        }
        self.mut_at(self.left_of(old)).map(|n| n.parent = new);
        self.mut_at(self.right_of(old)).map(|n| n.parent = new);
    }

    // rotate the tree right to make the left node of 'root' the new root.
    //
    // #Panics
    // if the left child node of 'root' is None.
    fn rotate_right(&mut self, root: Option<usize>) -> Option<usize> {
        let new_root = self.left_of(root);
        self.array[root?].left = self.at(new_root)?.right;
        self.array[new_root?].right = root;
        new_root
    }

    // rotate the tree left to make the right child node of 'root' the new root.
    //
    // #Panics
    // if root node or right child node of 'root' is None.
    fn rotate_left(&mut self, root: Option<usize>) -> Option<usize> {
        let new_root = self.right_of(root);
        self.array[root?].left = self.at(new_root)?.left;
        self.array[new_root?].left = root;
        new_root
    }
}

impl<K: Ord, V> SplayTree<K, V> {
    pub fn insert(&mut self, key: K, value: V) -> bool {
        let root = std::mem::replace(&mut self.root, None);
        self.root = self.splay(&key, self.root);
        if self.at(self.root).is_none() {
            self.root = Some(self.new_node(key, value, None, None));
            return true;
        }
        let root_node = self.mut_at(self.root).unwrap();
        match key.cmp(&root_node.key) {
            Ordering::Less => {
                // split at the left edge of root and
                // insert a new node as the new root
                self.new_node(key, value, self.left_of(root), root);
                true
            }
            Ordering::Greater => {
                // split at the right edge of root and
                // insert a new node as the new root
                self.new_node(key, value, root, self.right_of(root));
                true
            }
            Ordering::Equal => {
                // complete the insertion as the addition of 'value' to root
                root_node.add_value(value);
                true
            }
        }
    }

    pub fn get(&mut self, key: &K) -> Option<&[V]> {
        self.root = self.splay(key, self.root);
        if &self.at(self.root)?.key == key {
            let node = self.at(self.root)?;
            Some(&node.value)
        } else {
            None
        }
    }

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
        let lower_bound = self.lower_bound(self.root, &start);
        let upper_bound = self.upper_bound(self.root, &end);
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

        let new_root = match key.cmp(&root_node.key) {
            Ordering::Less => self.splay_left(key, root),
            Ordering::Greater => self.splay_right(key, root),
            Ordering::Equal => root,
        };
        new_root
    }

    // perform splay operation assuming 'key' is in the left subtree of 'root'.
    // return the new root.
    fn splay_left(&mut self, key: &K, root: Option<usize>) -> Option<usize> {
        let left = self.left_of(root);
        if left.is_none() {
            return root;
        }
        match key.cmp(&self.at(left)?.key) {
            Ordering::Less => {
                // zig-zig

                // make the left-left subtree have 'key' at its root by recursive splay operations
                let tmp = self.splay(key, self.at(left)?.left);
                self.mut_at(left)?.left = tmp;

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
                let tmp = self.splay(key, self.at(left)?.right);
                self.mut_at(left)?.right = tmp;

                // make the root of left-right subtree the new root by left and right rotations
                let tmp = if self.right_of(left).is_some() {
                    self.rotate_left(left)
                } else {
                    left
                };
                self.mut_at(root)?.left = tmp;
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
        match key.cmp(&self.at(right)?.key) {
            Ordering::Greater => {
                // zig-zig

                // make the right-right subtree have 'key' at its root by recursive splay operations
                let tmp = self.splay(key, self.at(right)?.right);
                self.mut_at(right)?.right = tmp;

                // make the root of right-right subtree the new root by double left rotations
                let new_root = self.rotate_right(root);
                if self.left_of(new_root).is_some() {
                    self.rotate_right(new_root)
                } else {
                    new_root
                }
            }
            Ordering::Less => {
                // zig-zag

                // make the right-left subtree have 'key' at its root by recursive splay operations
                let tmp = self.splay(key, self.at(right)?.left);
                self.mut_at(right)?.left = tmp;

                // make the root of right-left subtree the new root by left and right rotations
                let tmp = if self.right_of(right).is_some() {
                    self.rotate_right(right)
                } else {
                    right
                };
                self.mut_at(root)?.right = tmp;
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
            Bound::Included(v) => match (*v).cmp(node.key.borrow()) {
                Ordering::Less => {
                    // v < [key : not in the range. go right
                    self.lower_bound(self.right_of(id), bound)
                }
                Ordering::Greater => {
                    // [key < v : in the range. if left is none, it is the lower bound
                    self.lower_bound(self.left_of(id), bound).or(id)
                }
                Ordering::Equal => {
                    // v == [key : in the range. the node is the lower bound of spanning range
                    id
                }
            },
            Bound::Excluded(v) => match (*v).cmp(node.key.borrow()) {
                Ordering::Less | Ordering::Equal => {
                    // v <= (key : not in the range. go right
                    self.lower_bound(self.right_of(id), bound)
                }
                Ordering::Greater => {
                    // (key < v : in the range. if left is none, it is the lower bound
                    self.lower_bound(self.left_of(id), bound).or(id)
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
            Bound::Included(v) => match (*v).cmp(node.key.borrow()) {
                Ordering::Less => {
                    // v < key] : in the range. if right is none, it is the upper bound
                    self.lower_bound(self.right_of(id), bound).or(id)
                }
                Ordering::Greater => {
                    // key] < v : not in the range. go left
                    self.lower_bound(self.left_of(id), bound)
                }
                Ordering::Equal => {
                    // v == key] : in the range. the node is the upper bound of spanning range
                    id
                }
            },
            Bound::Excluded(v) => match (*v).cmp(node.key.borrow()) {
                Ordering::Less => {
                    // v < key) : in the range. if right is none, it is the upper bound
                    self.lower_bound(self.right_of(id), bound).or(id)
                }
                Ordering::Greater | Ordering::Equal => {
                    // key) <= v : not in the range. go left
                    self.lower_bound(self.left_of(id), bound)
                }
            },
            Bound::Unbounded => {
                // the right-most node is the upper bound
                self.lower_bound(self.right_of(id), bound).or(id)
            }
        }
    }
}

impl<K: Ord + Clone, V> SplayTree<K, V> {
    pub fn remove(&mut self, key: &K) -> Option<Vec<V>> {
        self.root = self.splay(key, self.root);
        if &self.at(self.root)?.key != key {
            return None;
        }
        let mut left = self.left_of(self.root);
        let right = self.right_of(self.root);
        let removed_node = self.delete_node(self.root);
        self.relink(self.root, None);
        if let Some(key) = self
            .at(self.right_most_of(left))
            .map(|left_max| &left_max.key)
        {
            left = self.splay(&key.clone(), left);
        }
        self.root = self.mut_at(left).map_or(right, |left_node| {
            left_node.right = right;
            left
        });
        removed_node.map(|n| n.value)
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
        let initial_val_idx_back = tree.at(back).map_or(0, |n| n.value.len() - 1);
        Self {
            tree,
            front: (front, initial_val_idx_front),
            back: (front, initial_val_idx_back),
        }
    }
}

impl<'a, K: 'a, V: 'a> Iterator for Iter<'a, K, V> {
    type Item = (&'a K, &'a V);

    fn next(&mut self) -> Option<Self::Item> {
        let (front, val_idx) = self.front;
        let node = self.tree.at(front)?;
        if self.front == self.back {
            self.front = (None, 0);
            self.back = (None, 0);
        } else if val_idx < node.value.len() - 1 {
            self.front = (front, val_idx + 1);
        } else {
            let next = self.tree.ascend(front);
            let next_val_idx = 0;
            self.front = (next, next_val_idx);
        }
        Some((&node.key, &node.value[val_idx]))
    }
}

impl<'a, K: 'a, V: 'a> DoubleEndedIterator for Iter<'a, K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let (back, val_idx) = self.back;
        let node = self.tree.at(back)?;
        if self.front == self.back {
            self.front = (None, 0);
            self.back = (None, 0);
        } else if val_idx > 0 {
            self.back = (back, val_idx - 1);
        } else {
            let next = self.tree.descend(back);
            let next_val_idx = self.tree.at(next).map_or(0, |n| n.value.len() - 1);
            self.back = (next, next_val_idx);
        }
        Some((&node.key, &node.value[val_idx]))
    }
}

/*
pub struct IntoIter<K, V> {
    tree: SplayTree<K, V>,
    front: Option<Node<K, V>>,
    back: Option<Node<K, V>>,
}

impl<K, V> IntoIter<K, V> {
    pub(self) fn new(tree: SplayTree<K, V>, front: Option<usize>, back: Option<usize>) -> Self {
        let mut tree = tree;
        let mut back = back;
        if !tree.is_empty() && back.is_some_and(|b| b == tree.len() - 1) {
            // if back node is the last element of tree.array,
            // just after the deletion of front node,
            // back node will take over the id of deleted front node
            // so overwrite 'back' beforehand.
            back = front;
        }
        let front = tree.delete_node(front);
        let back = tree.delete_node(back);
        Self { tree, front, back }
    }
}

impl<K: Ord, V> Iterator for IntoIter<K, V> {
    type Item = (K, V);

    fn next(&mut self) -> Option<Self::Item> {
        let id = self.front.map(|n| n.id);
        let next = self.tree.ascend(id);
        if self.front?.value.len() <= 1 &&
           next.is_some_and(|id| id == back.id) {

            self.back?.key {
            self.front = (None, 0);
            self.back = (None, 0);
        } else if val_idx < node_ref.value.len() - 1 {
            self.front = (front, val_idx + 1);
        } else {
            let next = self.tree.ascend(front);
            let next_val_idx = 0;
            self.front = (next, next_val_idx);
        }
        let node = self.tree.remove(node_ref.key)?;
        Some((node.key, node.value))
    }
}

impl<K: Ord, V> DoubleEndedIterator for IntoIter<K, V> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let (back, val_idx) = self.back;
        if self.front == self.back {
            self.front = (None, 0);
            self.back = (None, 0);
        } else if val_idx > 0 {
            self.back = (back, val_idx - 1);
        } else {
            let next = self.tree.descend(back);
            let next_val_idx = self.tree.at(next).map_or(0, |n| n.value.len() - 1);
            self.back = (next, next_val_idx);
        }
        let node = self.tree.remove(back)?;
        Some((node.key, node.value))
    }
}

*/
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

/*
impl<K: Ord, V>  {
}
*/
