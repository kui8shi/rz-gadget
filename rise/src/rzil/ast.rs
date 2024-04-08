use super::error::{Result, RzILError};
use crate::variables::VarId;
use std::collections::hash_map::DefaultHasher;
use std::fmt::{Debug, Display, Write};
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
        self.pure.symbolized.hash(state);
        self.pure.sort.hash(state);
        self.pure.eval.hash(state);
    }
}

#[derive(Eq, PartialEq)]
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
        self.get_sort().expect_same_with(other.get_sort())
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

impl Debug for Pure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.get_code() {
            PureCode::Var(scope, id) => write!(
                f,
                "Var({}:{})",
                id.get_uniq_name(),
                match scope {
                    Scope::Global => "g",
                    Scope::Local => "l",
                    Scope::Let => "let",
                }
            ),
            PureCode::Bool => write!(f, "Bool({})", self.evaluate_bool()),
            PureCode::Bitv => write!(f, "Bitv(0x{:x})", self.evaluate()),
            _ => {
                if self.is_concretized() {
                    if self.is_bool() {
                        f.debug_tuple(&format!("{:?}", self.get_code()))
                            .field(&self.evaluate_bool())
                            .field(&self.args)
                            .finish()
                    } else {
                        f.debug_tuple(&format!("{:?}", self.get_code()))
                            .field(&format_args!("0x{:x}", self.evaluate()))
                            .field(&self.args)
                            .finish()
                    }
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
        let v0 = PureCode::Var(Scope::Global, id0);
        let v1 = PureCode::Var(Scope::Global, id1);
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
