use crate::rzil;

pub type Result<T> = std::result::Result<T, RiseError>;

#[derive(thiserror::Error, Debug)]
pub enum RiseError {
    #[error("RzIL generation failed: {0}")]
    RzIL(#[from] rzil::error::RzILError),

    #[error("RzIL to Z3 conversion failed: {0}")]
    RzILToZ3(String),

    #[error("Z3 Solver failed: {0}")]
    Z3(String),

    #[error("Unsat")]
    Unsat,

    #[error("RzApi failed: {0}")]
    RzApi(#[from] rzapi::api::RzError),

    #[error("{0}")]
    Explorer(String),
}
