use crate::error::{Result, RiseError};
use crate::state::State;

pub struct StatePool<S> {
    pool: Vec<S>,
}

impl<S> StatePool<S> {
    pub fn new() -> Self {
        StatePool { pool: Vec::new() }
    }
}

pub trait PathExplorer<S: State> {
    fn push_state(&mut self, state: S);
    fn pop_state(&mut self) -> Result<S>;
}

impl<S: State> PathExplorer<S> for StatePool<S> {
    fn push_state(&mut self, state: S) {
        self.pool.push(state)
    }

    fn pop_state(&mut self) -> Result<S> {
        match self.pool.pop() {
            Some(state) => Ok(state),
            None => Err(RiseError::Explorer(
                "There is no states in the path explorer.".to_string(),
            )),
        }
    }
}
