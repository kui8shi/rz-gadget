use super::error::{Result, RzILError};
use crate::variables::VarId;
use std::collections::hash_map::DefaultHasher;
use std::fmt::{write, Display};
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};
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
}

impl std::fmt::Display for Sort {
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

impl std::fmt::Display for Scope {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PureCode {
    Var(Scope, VarId), // (scope, id)
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
    Cast(bool), // expand(true) or shrink(false)
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

#[derive(Clone, Debug, Eq, PartialEq)]
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
        self.pure.symbolized.hash(state);
        self.pure.sort.hash(state);
        self.pure.eval.hash(state);
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Pure {
    code: PureCode,
    args: Vec<PureRef>,
    symbolized: bool,
    sort: Sort,
    eval: u64, // if symbolized, it has 0.
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
            symbolized,
            sort,
            eval,
        }
    }

    pub fn evaluate(&self) -> u64 {
        self.eval
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
    pub fn iter_args(&self) -> impl Iterator<Item = &PureRef> {
        self.args.iter()
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
        self.symbolized
    }
    pub fn is_concretized(&self) -> bool {
        !self.symbolized
    }
    pub fn is_zero(&self) -> bool {
        self.is_bitv() && self.is_concretized() && self.evaluate() == 0
    }
    pub fn concretize(&mut self, eval: u64) {
        self.symbolized = false;
        self.eval = eval;
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
        if self.get_sort() != other.get_sort() {
            return Err(RzILError::SortIntegrity(self.get_sort(), other.get_sort()));
        }
        Ok(())
    }
    pub fn expect_var(&self) -> Result<(Scope, VarId)> {
        match self.get_code() {
            PureCode::Var(scope, id) => Ok((scope, id)),
            other => Err(RzILError::UnexpectedCode("Var".to_string(), other)),
        }
    }
    pub fn set_var_scope(&mut self, scope: Scope) -> Result<()> {
        let (_, id) = self.expect_var()?;
        self.code = PureCode::Var(scope, id);
        Ok(())
    }
    pub fn set_var_id(&mut self, id: VarId) -> Result<()> {
        let (scope, _) = self.expect_var()?;
        self.code = PureCode::Var(scope, id);
        Ok(())
    }
}

#[derive(Clone, Debug)]
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
        args: Vec<Rc<Effect>>,
    },
    Blk,
    Repeat,
    Branch {
        condition: PureRef,
        then: Rc<Effect>,
        otherwise: Rc<Effect>,
    },
    Store {
        key: PureRef,
        value: PureRef,
    },
    Empty,
}

impl Display for Effect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let code = match self {
            Effect::Nop => "Nop".to_string(),
            Effect::Set { .. } => "Set".to_string(),
            Effect::Jmp { .. } => "Jmp".to_string(),
            Effect::Goto { .. } => "Goto".to_string(),
            Effect::Seq { .. } => "Seq".to_string(),
            Effect::Blk => "Blk".to_string(),
            Effect::Repeat => "Repeat".to_string(),
            Effect::Branch { .. } => "Branch".to_string(),
            Effect::Store { .. } => "Store".to_string(),
            Effect::Empty => "Empty".to_string(),
        };
        write!(f, "{}", code)
    }
}

impl Effect {
    pub fn is_nop(&self) -> bool {
        matches!(self, Effect::Nop)
    }
}
