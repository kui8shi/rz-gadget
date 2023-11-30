use crate::rzil::RzILError;

pub type RiseeResult<T> = std::result::Result<T, RiseeError>;

#[derive(thiserror::Error, Debug)]
pub enum RiseeError {
    #[error("RzIL generation failed: {0}")]
    RzIL(#[from] RzILError),

    #[error("RzIL to Z3 conversion failed: {0}")]
    ToZ3(String),

    #[error("Z3 Solver failed: {0}")]
    Z3(String),

    #[error("Unsat")]
    Unsat,
}
