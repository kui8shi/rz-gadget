use crate::context::Context;
use crate::error::{Result, RiseError};
use crate::solver::Solver;

pub struct PathExplorer<S: Solver> {
    ctx_pool: Vec<Context<S>>,
}

impl<S: Solver> PathExplorer<S> {
    pub fn new() -> Self {
        PathExplorer {
            ctx_pool: Vec::new(),
        }
    }
    pub fn push_ctx(&mut self, ctx: Context<S>) {
        self.ctx_pool.push(ctx)
    }

    pub fn pop_ctx(&mut self) -> Result<Context<S>> {
        match self.ctx_pool.pop() {
            Some(ctx) => Ok(ctx),
            None => Err(RiseError::Explorer(
                "There is no contexts in the path explorer.".to_string(),
            )),
        }
    }
}
