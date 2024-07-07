use crate::error::Result;
use crate::explorer::{PathExplorer, StatePool};
use crate::registers;
use crate::rzil::{builder::RzILCache, lifter::RzILLifter};
use crate::state::process::Process;
use crate::state::{State, Status};
use crate::variables::{VarStorage, Variables};
use rzapi::api::RzApi;
use rzapi::structs::{FlagInfo, Instruction};
use std::collections::HashMap;
pub struct Rise<S> {
    pub api: RzApi,
    states: StatePool<S>,
    lifter: RzILLifter,
    builder: RzILCache,
    vars: VarStorage,
    addr: u64,
    flags: HashMap<String, FlagInfo>,
}

/*
 * 1. block-wise symbolic execution. if it reaches a branch, return status.
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

impl<S: Process> Rise<S> {
    /*
     * Panics if stream has not loaded any sources yet.
     */
    pub fn new(path: Option<&str>) -> Result<Self> {
        let api = RzApi::new(path)?;
        let lifter = RzILLifter::new();
        let builder = RzILCache::new();
        let mut vars = VarStorage::new();
        let addr = 0;
        let flaginfo = api.get_flags()?;
        let mut flags = HashMap::new();
        for flag in flaginfo {
            flags.insert(flag.name.clone(), flag);
        }
        let entry_info = api.get_entrypoint()?;
        let entry_point = (!entry_info.is_empty()).then_some(entry_info[0].vaddr);
        let ctx = State::new(builder.clone(), entry_point);
        let mut explorer = StatePool::new();

        explorer.push_ctx(ctx);
        registers::bind_registers(&api, &builder, &mut vars)?;

        Ok(Rise {
            api,
            states: explorer,
            lifter,
            builder,
            vars,
            addr,
            flags,
        })
    }

    pub fn run(&mut self, mode: Mode) -> Result<Status> {
        let mut state = self.states.pop_ctx()?;
        while let Status::LoadInst = state.get_status() {
            let pc = state.get_pc();
            let inst = self.read_inst(pc)?;
            state.set_pc(pc + inst.size);
            match self
                .lifter
                .parse_effect_optional(&self.builder, &mut self.vars, &inst.rzil)?
            {
                None => {
                    continue;
                }
                Some(op) => {
            println!("Program at {:#x}, inst = {:?}", pc, inst.pseudo); dbg!(&op);
                    state.process(op)?;
                }
            }
            self.vars.clear_local();
        }
        let status = state.get_status();
        self.states.push_ctx(state);
        Ok(status)
    }

    fn read_inst(&mut self, pc: u64) -> Result<Instruction> {
        Ok(self.api.get_n_insts(Some(1), Some(pc))?.pop().unwrap())
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

    pub fn set_register(&mut self, name: &str, val: u64) {
        self.vars.concretize_register(name, val)
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
    use crate::state::StateZ3Backend;

    use super::{Mode, Rise};

    #[test]
    fn new() {
        //let mut rise = Rise::<StateZ3Backend>::new(Some("test/dummy")).unwrap();
        //dbg!(rise.run(Mode::Step));
    }
}
