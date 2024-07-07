use super::{PureCode, Scope, Sort};
use std::num::ParseIntError;

pub type Result<T> = std::result::Result<T, RzILError>;

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

    #[error("Unkown (Unimplemented or Invalid) Op detected.")]
    UnkownOp,

    #[error("No effective operations were lifted.")]
    Empty,

    #[error("Variable {0} is immutable but changed.")]
    ImmutableVariable(String),

    #[error("Variable {0}'s scope has changed from {1} to {2}.")]
    InconsistentScope(String, Scope, Scope),

    #[error("Currently Unable to handle Set operations with a duplicate target in a Branch.")]
    DoubleSetInBranch,

    #[error("Currently Unable to handle operations other than Nop, Jmp, or Goto in a Branch.")]
    InvalidBranch,
}
