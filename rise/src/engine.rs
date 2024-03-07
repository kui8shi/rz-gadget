use rzapi::RzApi;
use std::rc::Rc;
use crate::context::Context;
use crate::explorer::PathExplorer;
use crate::error::RiseResult;
use crate::rzil::{RzILLifter, RzILBuilder, Variables, Effect, PureRef};
use crate::register;
use crate::solver::Solver;
use crate::memory::Memory;
use std::fmt::Debug;
mod effect;
#[derive(Debug)]
pub struct Rise
{
    api: RzApi,
    explorer: PathExplorer,
    lifter: Lifter,
    builder: RzILBuilder,
    vars: Variables,
}

/*
 * 1. block-wise symbolic execution. if branches, leave them and throw status.
 * 2. there can be control manager as a sample wrapper of this.
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
        let mut api = RzApi::new(path);
        let explorer = PathExplorer::new();
        let lifter = RzILLifter::new();
        let builder = RzILBuilder::new();
        let mut vars = Variables::new();
        register::bind_registers(&mut api, &mut vars)?;
        Risee {
            api,
            explorer,
            lifter,
            builder,
            vars,
        }
    }

    pub fn run(&mut self, mode: Mode) -> RiseResult<Status> {
        let ctx = self.explorer.pop_ctx()?;
        let pc = ctx.get_pc();
        ctx = match mode {
            Mode::Step => {
                let ops = self.read_insts(pc, 1)?;
                self.process(ctx, ops)?
            },
            Mode::Block => {
                let n = self.num_insts_in_current_block()?;
                let ops = self.read_insts(pc, n)?;
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

    fn process(&mut self, ctx: Context, ops: Vec<Effect>) -> RiseResult<Context> {
        for op in ops {
        }
    }

    fn read_insts(&mut self, pc: u64, n: u64) -> RiseResult<Vec<Effect>> {
        let mut ops = Vec::new();
        for inst in self.api.get_n_insts(Some(n), Some(pc))? {
            ops.push(self.lifter.parse_effect(self.builder, &mut self.vars, inst)?);
        }
        Ok(ops)
    }
}
