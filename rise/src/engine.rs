use crate::context::{Context, Status};
use crate::error::Result;
use crate::explorer::PathExplorer;
use crate::registers;
use crate::rzil::{ast::Effect, builder::RzILCache, lifter::RzILLifter};
use crate::variables::Variables;
use rzapi::api::RzApi;
use std::rc::Rc;
pub struct Rise<C: Context> {
    api: RzApi,
    explorer: PathExplorer<C>,
    lifter: RzILLifter,
    builder: RzILCache,
    vars: Variables,
    addr: u64,
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

impl<C: Context> Rise<C> {
    /*
     * Panics if stream has not loaded any sources yet.
     */
    pub fn new(path: Option<String>) -> Result<Self> {
        let mut api = RzApi::new(path)?;
        let explorer = PathExplorer::new();
        let lifter = RzILLifter::new();
        let builder = RzILCache::new();
        let mut vars = Variables::new();
        let addr = 0;
        registers::bind_registers(&mut api, &mut vars, &builder)?;
        Ok(Rise {
            api,
            explorer,
            lifter,
            builder,
            vars,
            addr,
        })
    }

    pub fn run(&mut self, mode: Mode) -> Result<Status> {
        let mut ctx = self.explorer.pop_ctx()?;
        let pc = ctx.get_pc();
        match mode {
            Mode::Step => {
                let ops = self.read_insts(pc, 1)?;
                ctx.process(ops)?;
            }
            Mode::Block => {
                let n = self.num_insts_in_current_block()?;
                let ops = self.read_insts(pc, n)?;
                ctx.process(ops)?;
            }
            Mode::Explore => {
                while self.is_stopped() {
                    let status = self.run(Mode::Block)?;
                    if let Status::DirectJump(addr) = status {
                        self.seek(addr);
                    }
                }
            }
        };
        let status = ctx.get_status();
        self.explorer.push_ctx(ctx);
        Ok(status)
    }

    fn read_insts(&mut self, pc: u64, n: u64) -> Result<Vec<Rc<Effect>>> {
        let mut ops = Vec::new();
        for inst in self.api.get_n_insts(Some(n), Some(pc))? {
            ops.push(
                self.lifter
                    .parse_effect(&self.builder, &mut self.vars, &inst.rzil)?,
            );
        }
        Ok(ops)
    }

    fn seek(&mut self, addr: u64) {
        self.addr = addr;
    }

    fn num_insts_in_current_block(&self) -> Result<u64> {
        Ok(0)
    }

    fn is_stopped(&self) -> bool {
        false
    }
}
