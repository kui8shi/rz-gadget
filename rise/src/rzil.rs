pub(crate) mod builder;
pub(crate) mod error;
pub(crate) mod lifter;

use crate::variables::VarId;
use error::{Result, RzILError};
use std::cell::Cell;
use std::collections::hash_map::DefaultHasher;
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use std::rc::Rc;

/*
 * rzapi::RzILInfo -> rz-sym::Pure(sorted, unrolled) -> Expr.add_solver
 * let rzg = RzGadget::new()
 * rzg.set_register("rax",10);
 * //rzg.call(r#"write(0, "String",)"#)
 * //rzg.call(r#"execve("/bin/bash",NULL,NULL)"#)
 * rzg.syscall("execve",[Str("/bin/bash"),NULL,NULL])
 * rzg.syscall("write",[Int(1),Str("Hello world!"),Int(0)])
 * rzg.syscall("execve",[Str("/bin/bash"),NULL,NULL])
 * rzg.syscall("execve",["bin/bash",0,0])
 * rzg.call("printf",[]);
 * let payload = rzg.build()?;
 * //rzg.call("execve", "/bin/bash", "NULL", "NULL" );
 * //rzg.call("write", "1", "\xfc\x6c", "0" );
 *
 * impl rzg{
 *      self.sym_engine.
 * }
 * */

#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub enum Sort {
    Bitv(usize),
    Bool,
}

impl Sort {
    pub fn is_bitv(&self) -> bool {
        match self {
            Sort::Bitv(_) => true,
            Sort::Bool => false,
        }
    }
    pub fn is_bool(&self) -> bool {
        match self {
            Sort::Bitv(_) => false,
            Sort::Bool => true,
        }
    }
    pub fn get_size(&self) -> usize {
        match self {
            Sort::Bitv(len) => *len,
            Sort::Bool => 1,
        }
    }

    pub fn expect_same_with(self, other: Self) -> Result<()> {
        if self != other {
            return Err(RzILError::SortIntegrity(self, other));
        }
        Ok(())
    }
}

impl Display for Sort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub enum Scope {
    Global, // represent physical registers
    Local,  // variables valid inside an Instruction
    Let,    // variables valid inside a Let expression
}

impl Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub enum PureCode {
    Var { scope: Scope, id: VarId }, // (scope, id)
    Ite,
    Let,
    Bool,
    BoolInv,
    BoolAnd,
    BoolOr,
    BoolXor,
    Bitv,
    Msb,
    Lsb,
    IsZero,
    Neg,
    LogNot,
    Add,
    Sub,
    Mul,
    Div,
    Sdiv,
    Mod,
    Smod,
    LogAnd,
    LogOr,
    LogXor,
    ShiftRight,
    ShiftLeft,
    Equal,
    Sle,
    Ule,
    Cast { expand: bool },
    Append,
    Load,
    // Float Instructions (Unimplemented yet)
    Float,
    Fbits,
    IsFinite,
    IsNan,
    IsInf,
    IsFzero,
    IsFneg,
    IsFpos,
    Fneg,
    Fpos,
    FcastInt,
    FcastSint,
    FcastFloat,
    FcastSfloat,
    Fconvert,
    Fround,
    Frequal,
    Fsucc,
    Fpred,
    Forder,
    Fsqrt,
    Frsqrt,
    Fadd,
    Fsub,
    Fmul,
    Fdiv,
    Fmod,
    Hypot,
    Pow,
    Fmad,
    Fpown,
    Frootn,
    Fcompound,
    Extract(u32, u32), // this exactly is not RzIL opcode.
                       // internally used just for symbolic expression.
}

impl Display for PureCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone, Eq, PartialEq)]
pub struct PureRef {
    pure: Rc<Pure>,
    hash: u64, // unique per semantics
}

impl Deref for PureRef {
    type Target = Pure;

    fn deref(&self) -> &Self::Target {
        &self.pure
    }
}

impl Debug for PureRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pure.fmt(f)
    }
}

impl From<Pure> for PureRef {
    fn from(pure: Pure) -> Self {
        let mut to = Self {
            pure: Rc::new(pure),
            hash: 0,
        };
        let mut hasher = DefaultHasher::new();
        to.hash(&mut hasher);
        to.hash = hasher.finish();
        to
    }
}

impl PureRef {
    pub fn get_hash(&self) -> u64 {
        self.hash
    }
}

impl Hash for PureRef {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.pure.code.hash(state);
        for arg in &self.pure.args {
            arg.hash.hash(state);
        }
        self.pure.sort.hash(state);
        self.pure.symbolized.get().hash(state);
        self.pure.eval.get().hash(state);
    }
}

#[derive(Eq, PartialEq)]
pub struct Pure {
    code: PureCode,
    args: Vec<PureRef>,
    sort: Sort,
    symbolized: Cell<bool>,
    eval: Cell<u64>, // if symbolized, it has 0.
}

impl Pure {
    pub fn new(
        code: PureCode,
        args: Vec<PureRef>,
        symbolized: bool,
        sort: Sort,
        eval: u64,
    ) -> Self {
        Pure {
            code,
            args,
            sort,
            symbolized: Cell::new(symbolized),
            eval: Cell::new(eval),
        }
    }

    pub fn evaluate(&self) -> u64 {
        self.eval.get()
    }
    pub fn evaluate_bool(&self) -> bool {
        self.evaluate() != 0
    }
    pub fn get_code(&self) -> PureCode {
        self.code.clone()
    }
    pub fn get_sort(&self) -> Sort {
        self.sort
    }
    pub fn get_size(&self) -> usize {
        self.sort.get_size()
    }
    pub fn num_args(&self) -> usize {
        self.args.len()
    }
    pub fn get_arg(&self, i: usize) -> PureRef {
        self.args[i].clone()
    }
    pub fn get_bitmask(&self) -> u64 {
        u64::MAX >> (u64::BITS as usize - self.get_size())
    }
    pub fn is_bitv(&self) -> bool {
        self.get_sort().is_bitv()
    }
    pub fn is_bool(&self) -> bool {
        self.get_sort().is_bool()
    }
    pub fn is_symbolized(&self) -> bool {
        self.symbolized.get()
    }
    pub fn is_concretized(&self) -> bool {
        !self.symbolized.get()
    }
    pub fn is_true(&self) -> bool {
        self.is_bool() && self.is_concretized() && self.evaluate_bool()
    }
    pub fn is_false(&self) -> bool {
        self.is_bool() && self.is_concretized() && !self.evaluate_bool()
    }
    pub fn is_zero(&self) -> bool {
        self.is_bitv() && self.is_concretized() && self.evaluate() == 0
    }
    pub fn concretize(&self, eval: u64) {
        self.symbolized.set(false);
        self.eval.set(eval);
    }
    pub fn expect_bool(&self) -> Result<()> {
        if !self.is_bool() {
            return Err(RzILError::UnexpectedSort(Sort::Bool, self.get_sort()));
        }
        Ok(())
    }
    pub fn expect_bitv(&self) -> Result<()> {
        if !self.is_bitv() {
            return Err(RzILError::UnexpectedSort(Sort::Bitv(0), self.get_sort()));
        }
        Ok(())
    }
    pub fn expect_same_sort_with(&self, other: &Pure) -> Result<()> {
        self.get_sort().expect_same_with(other.get_sort())
    }
    pub fn expect_var(&self) -> Result<(Scope, VarId)> {
        match self.get_code() {
            PureCode::Var { scope, id } => Ok((scope, id)),
            other => Err(RzILError::UnexpectedCode("Var".to_string(), other)),
        }
    }
}

impl Debug for Pure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.get_code() {
            PureCode::Var { scope, id } => match scope {
                Scope::Global => write!(f, "Var({}:g)", id.get_uniq_name()),
                Scope::Local => write!(f, "Var({}:l)", id.get_uniq_name()),
                Scope::Let => write!(f, "Var({}:let)", id.get_name()),
            },
            PureCode::Let => f
                .debug_struct("Let")
                .field("dst", &self.get_arg(0))
                .field("exp", &self.get_arg(1))
                .field("body", &self.get_arg(2))
                .finish(),
            PureCode::Bool => write!(f, "Bool({})", self.evaluate_bool()),
            PureCode::Bitv => write!(
                f,
                "Bitv({:#0width$x})",
                self.evaluate(),
                width = self.get_size() / 4 + 2
            ),
            PureCode::ShiftRight | PureCode::ShiftLeft => f
                .debug_struct(&format!("{:?}", self.get_code()))
                .field("fill_bit", &self.get_arg(0))
                .field("value", &self.get_arg(1))
                .field("shift", &self.get_arg(2))
                .finish(),
            PureCode::Cast { expand } => {
                if expand {
                    f.debug_struct("Cast")
                        .field(
                            "size",
                            &format_args!("{} -> {}", self.get_arg(1).get_size(), self.get_size(),),
                        )
                        .field("fill_bit", &self.get_arg(0))
                        .field("value", &self.get_arg(1))
                        .finish()
                } else {
                    f.debug_struct("Cast")
                        .field(
                            "size",
                            &format_args!("{} -> {}", self.get_arg(1).get_size(), self.get_size(),),
                        )
                        .field("value", &self.get_arg(1))
                        .finish()
                }
            }
            _ => {
                if self.is_concretized() {
                    if self.is_bool() {
                        f.debug_tuple(&format!("{:?}", self.get_code()))
                            .field(&self.evaluate_bool())
                            .field(&self.args)
                            .finish()
                    } else {
                        f.debug_tuple(&format!("{:?}", self.get_code()))
                            .field(&format_args!(
                                "{:#0width$x}",
                                self.evaluate(),
                                width = self.get_size() / 4 + 2
                            ))
                            .field(&self.args)
                            .finish()
                    }
                } else if self.num_args() == 1 {
                    f.debug_tuple(&format!("{:?}", self.get_code()))
                        .field(&self.get_arg(0))
                        .finish()
                } else if self.num_args() == 2 {
                    f.debug_tuple(&format!("{:?}", self.get_code()))
                        .field(&self.get_arg(0))
                        .field(&self.get_arg(1))
                        .finish()
                } else {
                    f.debug_tuple(&format!("{:?}", self.get_code()))
                        .field(&self.args)
                        .finish()
                }
            }
        }
    }
}

#[derive(Clone)]
pub enum Effect {
    Nop,
    Set {
        var: PureRef,
    },
    Jmp {
        dst: PureRef,
    },
    Goto {
        label: String,
    },
    Seq {
        args: Vec<Effect>,
    },
    Blk,
    Repeat,
    // For simplicity, we convert
    // each Branch-Set operation to an equivalent Set-Ite operation (see rzil/lifter.rs)
    // as wel as restrict Branch taking only Nop, Jmp, or Goto as its arguments,
    // by which we treat it as a conditional jump operation.
    Branch {
        condition: PureRef,
        then: Box<Effect>,
        otherwise: Box<Effect>,
    },
    Store {
        key: PureRef,
        value: PureRef,
    },
    Empty,
}

impl Debug for Effect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Effect::Set { var } => f
                .debug_struct("Set")
                .field("dst", &var)
                .field("src", &var.get_arg(0))
                .finish(),
            Effect::Jmp { dst } => f.debug_struct("Jmp").field("dst", &dst).finish(),
            Effect::Goto { label } => f.debug_struct("Goto").field("label", &label).finish(),
            Effect::Seq { args } => f.debug_struct("Seq").field("args", &args).finish(),
            Effect::Branch {
                condition,
                then,
                otherwise,
            } => f
                .debug_struct("Branch")
                .field("condition", &condition)
                .field("then", &then)
                .field("otherwise", &otherwise)
                .finish(),
            Effect::Store { key, value } => f
                .debug_struct("Store")
                .field("key", &key)
                .field("value", &value)
                .finish(),
            Effect::Nop => f.debug_struct("Nop").finish(),
            Effect::Blk => f.debug_struct("Blk").finish(),
            Effect::Repeat => f.debug_struct("Repeat").finish(),
            Effect::Empty => f.debug_struct("Empty").finish(),
        }
    }
}

impl Effect {
    pub fn is_nop(&self) -> bool {
        matches!(self, Effect::Nop)
    }
}

#[cfg(test)]
mod test {
    use std::hash::{DefaultHasher, Hash, Hasher};

    use crate::variables::VarId;

    use super::{PureCode, Scope};

    #[test]
    fn pure_code_eq() {
        let id0 = VarId::new_with_count("v", 0);
        let id1 = VarId::new_with_count("v", 1);
        let v0 = PureCode::Var {
            scope: Scope::Global,
            id: id0,
        };
        let v1 = PureCode::Var {
            scope: Scope::Global,
            id: id1,
        };
        let mut state0 = DefaultHasher::new();
        let mut state1 = DefaultHasher::new();
        v0.hash(&mut state0);
        v1.hash(&mut state1);
        let hash0 = state0.finish();
        let hash1 = state1.finish();
        assert_ne!(v0, v1);
        assert_ne!(hash0, hash1);
    }
}
