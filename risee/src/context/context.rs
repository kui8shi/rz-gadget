//!  Defines `Context` trait to be used by symbolic emulator

use std::fmt::Debug;
use std::rc::Rc;
use crate::rzil::RzIL;

pub trait Context: Clone + Debug
                   + RegisterRead
                   + RegisterWrite
                   + MemoryRead
                   + MemoryWrite
{
    fn get_pc(&self) -> u64;
    fn set_pc(&mut self, _: u64);
    fn is_symbolic(&self) -> bool {
        true
    }
    fn is_concrete(&self) -> bool {
        !self.is_symbolic()
    }
    fn alias_of(&self, _: String) -> Option<String>;

    fn get_local_var(&self, name: &str) -> Option<Rc<RzIL>>;
    fn set_local_var(&self, name: &str, value:Rc<RzIL>) -> Option<Rc<RzIL>>;
    fn clear_local_vars(&mut self);
}

pub trait MemoryRead: Sized {
    fn mem_read(&self, key: Rc<RzIL>, size: usize) -> Rc<RzIL>;
}

pub trait MemoryWrite: Sized {
    fn mem_write(&mut self, key: Rc<RzIL>, value: Rc<RzIL>, size: usize);
}

pub trait RegisterRead: Sized {
    fn reg_read(&self, name: &str) -> Rc<RzIL>;
}

pub trait RegisterWrite: Sized {
    fn reg_write(&mut self, name: &str, value: Rc<RzIL>);
}
