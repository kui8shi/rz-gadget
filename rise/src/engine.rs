use rzapi::RzApi;
use std::rc::Rc;
use crate::context::Context;
use crate::explorer::PathExplorer;
//use crate::stream::InstructionStream;
use crate::error::RiseeResult;
use crate::rzil::{RzIL, Effect, Pure};
use crate::solver::Solver;
use crate::memory::Memory;
use std::fmt::Debug;
mod effect;
#[derive(Debug)]
pub struct Rise
{
    stream: Stream,
    explorer: PathExplorer,
    rzil: RzIL,
}

/*
 * 1. block-wise symbolic execution. if branches, leave them and throw status.
 * 2. there can be a sample control manager code.
 *
 * import rise
 * let s = rise::Rise::new(Some("/bin/ls"))
 * //s.add_inst("\x20\x13")
 * s.seek(0x40000)
 * let status = s.run(Mode::Block or Mode::Step or Mode::Explore)
 * match status {
 *  Status::DirectJump(id) => s.seek(id), s.run(Mode::Block),
 *  Status::IndirectJump(id) => let jump = s.eval(id) or s.get_pure(id),
 *  Status::Branch(cond, then, else) => ,
 *  Status::Continue => 
 * }
 *
 * let stream = risee::InstructionStream::new("/bin/ls")
 * stream.load_file("/bin/ls")
 * let se = risee::SymbolicEngine::new(stream.clone(), ip);
 * se.set_hook(0x40100000)
 * se.process_inst_n(9) or se.process_inst() or se.process_block()
 * se.process()
 *
 */

pub enum Mode {
    Step,
    Block,
    Explore,
}

impl Rise {
    /*
     * Panics if stream has not loaded any sources yet.
     */
    pub fn new(path: Option<String>) -> Self {
        let stream = Stream::new(path);
        let explorer = PathExplorer::new();
        let rzil = RzIL::new();
        Risee {
            stream,
            explorer,
            rzil,
        }
    }

    pub fn 
    pub fn run(&mut self, mode: Mode) -> RiseeResult<Status> {
        let ctx = self.explorer.pop_ctx(self.stream.get_probe().unwrap())?;
        ctx = match mode {
            Mode::Step => {
                let op = self.stream.read_inst(&mut self.rzil)?;
                self.process(ctx, op)?
            },
            Mode::Block => {
                let ops = self.stream.read_block(&mut self.rzil)?;
                self.process(ctx, ops)?
            },
            Mode::Explore => {
                while self.is_stopped() {
                    ctx = self.run(Mode::Block)?;
                    if let Status::DirectJump(addr) = ctx.get_status() {
                        self.explorer.push_ctx(ctx);
                        self.seek(addr);
                    }
                }
            }
        };
        self.explorer.push_ctx(ctx);
        ctx.get_status()
    }
}
