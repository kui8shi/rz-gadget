//! Defines traits that guides the symbolic emulator

use crate::context::context::Context;
use std::fmt::Debug;

use std::rc::Rc;
use crate::rzil::RzIL;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Control {
    Continue,
    TerminatePath,
    ExploreTrue,
    ExploreFalse,
    Skip,
    Halt,
    Break,
}

pub trait PathExplorer {
    type Ctx: Context;

    fn new() -> Self;
    fn next(&mut self, ctx: &mut Self::Ctx) -> Control;
    fn next_job(&mut self, ctx: &mut Self::Ctx) -> Option<Control>;

    fn control_flow(&mut self, 
                    ctx: &mut Self::Ctx, 
                    addr: Rc<RzIL>);

    fn register_branch(&mut self, 
                       ctx: &mut Self::Ctx, 
                       branch: Rc<RzIL>) -> Control;
}
