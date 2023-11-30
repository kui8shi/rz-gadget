//! Defines traits that need to be implemented for a source to be considered as
//! an `InstructionStream`.

use std::fmt::Debug;
use std::path;
use std::collections::HashMap;
use std::hash::Hash;
use std::io::prelude::*;
use std::fs::File;
use serde::{Serialize, Deserialize};
use rzapi::api::RzApi;
use rzapi::structs::RzILInfo;

pub trait InstructionStream {
    type Output: Debug + Clone;
    type Index: Debug + Clone;

    fn new() -> Self;
    fn at(&mut self, _: Self::Index) -> Option<Self::Output>;
}


impl InstructionStream for RzApi {
    type Output = RzILInfo;
    type Index = u64;

    fn new() -> RzApi {
        RzApi::new::<&str>(None, None).expect("Unable to open RzApi")
    }

    fn at(&mut self, addr: u64) -> Option<Self::Output> {
        Some(self.get_n_insts(Some(1), Some(addr)).unwrap()[0].clone())
    }
}

// InstructionStream that reads and provides instructions from files.
// This is useful for tests, debug and other smaller applications.
// Maintains a HashMap from address to RzILInfo that it should provide
// when asked for that address.
#[derive(Clone, Debug, Default, Serialize, Deserialize)]
pub struct FileStream<'de, I, Op>
    where I: Debug + Clone + Deserialize<'de> + Hash + PartialEq + Eq,
          Op: Debug + Clone + Deserialize<'de>
{
    insts: HashMap<I, Op>,
}


impl<'de, I, Op> FileStream<'de, I, Op>
    where I: Debug + Clone + Deserialize<'de> + Hash + PartialEq + Eq,
          Op: Debug + Clone + Deserialize<'de>
{
    pub fn load<T: AsRef<path::Path>>(&mut self, fname: T) {
        let mut f = File::open(fname).expect("Failed to open file");
        let mut s = String::new();
        f.read_to_string(&mut s).expect("Failed to read from file");
        self.insts = serde_json::from_str(&s).expect("Failed to decode json");
    }
}

impl<'de, I, Op> InstructionStream for FileStream<'de, I, Op>
where I: Debug + Clone + Deserialize<'de> + Hash + PartialEq + Eq,
      Op: Debug + Clone + Deserialize<'de> {
    type Output = RzILInfo;
    type Index = I;

    fn new() -> FileStream<I, Op> {
        FileStream { insts: HashMap::new() }
    }

    fn at(&mut self, addr: I) -> Option<Op> {
        self.insts.get(&addr).cloned()
    }
}
