use crate::context::Context;
use crate::error::{Result, RiseError};

pub struct PathExplorer<C: Context> {
    ctx_pool: Vec<C>,
}

impl<C: Context> PathExplorer<C> {
    pub fn new() -> Self {
        PathExplorer {
            ctx_pool: Vec::new(),
        }
    }
    pub fn push_ctx(&mut self, ctx: C) {
        self.ctx_pool.push(ctx)
    }

    pub fn pop_ctx(&mut self) -> Result<C> {
        match self.ctx_pool.pop() {
            Some(ctx) => Ok(ctx),
            None => Err(RiseError::Explorer(
                "There is no contexts in the path explorer.".to_string(),
            )),
        }
    }
}
