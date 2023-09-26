
use rzapi::structs::RegisterProfile;
use crate::rzil::RzIL;
use std::rc::Rc;
use std::fmt::Debug;

#[derive(Clone, Debug, Default)]
pub struct RegEntry {
    pub name: String,
    pub idx: usize,
    // 0 indexed
    pub start_bit: usize,
    pub end_bit: usize,
    pub is_whole: bool,
    pub alias: Option<String>,
}

impl RegEntry {
    pub fn new(name: String,
           idx: usize,
           start_bit: usize,
           end_bit: usize,
           is_whole: bool,
           alias: Option<String>)
           -> RegEntry {
        RegEntry {
            name,
            idx,
            start_bit,
            end_bit,
            is_whole,
            alias,
        }
    }
}

pub trait RegStore: Clone + Debug {
    type Ast;

    fn new(_: &mut RegisterProfile) -> Self;

    fn get_reg_entry(&self, _: &str) -> RegEntry;

    fn get_reg_ref(&self, _: &str) -> Option<Rc<RzIL>>;

    fn set_reg(&mut self, _: &str, _: Rc<RzIL>);

    fn read(&self, name: &str) -> Rc<RzIL>;

    fn write(&mut self, name: &str, value: Rc<RzIL>);
}
