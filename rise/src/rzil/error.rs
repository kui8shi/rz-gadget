use super::{PureCode, Sort};
use std::num::ParseIntError;

pub type RzILResult<T> = std::result::Result<T, RzILError>;

#[derive(thiserror::Error, Debug)]
pub enum RzILError {
    #[error("Sort {0} was expected, but {1} was found.")]
    UnexpectedSort(Sort, Sort),

    #[error("Sort {0} and {1} should be identical.")]
    SortIntegrity(Sort, Sort),

    #[error("Undefined variable {0} was referenced.")]
    UndefinedVariableReferenced(String),

    #[error("Code {0} was expected, but got {1}.")]
    UnexpectedCode(String, PureCode),

    #[error("'{0}' should be hex-decimal.")]
    ParseHex(String),

    #[error("invaid integer.")]
    ParseInt(#[from] ParseIntError),

    #[error("Pure {0} is Unimplemented.")]
    UnimplementedRzILPure(PureCode),

    #[error("Effect {0} is Unimplemented.")]
    UnimplementedRzILEffect(String),

    #[error("Unkown (Unimplemented or Invalid) Pure detected.")]
    UnkownPure,

    #[error("Empty detected. Unable to continue.")]
    Empty,

    #[error("Variable {0} is immutable but changed.")]
    ImmutableVariable(String),

    #[error("Currently Unable to handle Set operations with a duplicate target in a Branch.")]
    DuplicateSetInBranch,

    #[error("No return value.")]
    None,
}
