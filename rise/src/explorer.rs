use crate::error::{Result, RiseError};
use crate::state::State;

pub struct PathExplorer {
    ctx_pool: Vec<State>,
}

impl PathExplorer {
    pub fn new() -> Self {
        PathExplorer {
            ctx_pool: Vec::new(),
        }
    }
    pub fn push_ctx(&mut self, ctx: State) {
        self.ctx_pool.push(ctx)
    }

    pub fn pop_ctx(&mut self) -> Result<State> {
        match self.ctx_pool.pop() {
            Some(ctx) => Ok(ctx),
            None => Err(RiseError::Explorer(
                "There is no states in the path explorer.".to_string(),
            )),
        }
    }
}
