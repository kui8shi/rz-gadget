use crate::state::process::Process;
use crate::state::solver::Z3Solver;
use crate::state::{State, State, Status};
use crate::error::Result;
use crate::explorer::PathExplorer;
use crate::registers;
use crate::rzil::ast::Sort;
use crate::rzil::builder::RzILBuilder;
use crate::rzil::{ast::Effect, builder::RzILCache, lifter::RzILLifter};
use crate::variables::Variables;
use rzapi::api::RzApi;
use rzapi::structs::FlagInfo;
use std::collections::HashMap;
use std::rc::Rc;
pub struct Rise {
    api: RzApi,
    explorer: PathExplorer,
    lifter: RzILLifter,
    builder: RzILCache,
    vars: Variables,
    addr: u64,
    flags: HashMap<String, FlagInfo>,
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
    pub fn new(path: Option<&str>) -> Result<Self> {
        let mut api = RzApi::new(path)?;
        let lifter = RzILLifter::new();
        let builder = RzILCache::new();
        let mut vars = Variables::new();
        let addr = 0;
        registers::bind_registers(&mut api, &mut vars, &builder)?;
        let flaginfo = api.get_flags()?;
        let mut flags = HashMap::new();
        for flag in flaginfo {
            flags.insert(flag.name.clone(), flag);
        }
        let solver = Z3Solver::new();
        let ctx = State::new(solver, builder.clone());
        let mut explorer = PathExplorer::new();
        explorer.push_ctx(ctx);

        Ok(Rise {
            api,
            explorer,
            lifter,
            builder,
            vars,
            addr,
            flags,
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
                    self.run(Mode::Block)?;
                }
            }
        }
        let status = {
            // optional operations
            let next_pc = match ctx.get_status() {
                Status::Jump(addr) => addr,
                Status::Goto(label) => {
                    let addr = self.flags.get(&label).unwrap().offset;
                    self.builder.new_const(Sort::Bitv(64), addr)
                }
                Status::Branch(c, t, o) => {
                    //let path = self.explorer.branch(c, t, o);
                    //match path ... if terminated then break else take either branch op
                    if c.is_concretized() {
                        if c.evaluate_bool() {
                            t
                        } else {
                            o
                        }
                    }
                }
                Status::Continue => {
                    self.api.seek()
                    ctx.set_pc()
                }
                Status::Terminated => (),
            }
            ctx.get_status()
        };
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

    /*
     *
     * needed functions
     *
     * - set register symbolic/concrete value
     * - set entry point / or blank state(?)
     * - set options (aligned memory, zero filled memory etc..)
     *
     *
     */
}

#[cfg(test)]
mod test {
    use crate::state::State;

    use super::{Mode, Rise};

    #[test]
    fn new() {
        let mut rise = Rise::new(Some("test/dummy")).unwrap();
        dbg!(rise.run(Mode::Step));
    }
}
