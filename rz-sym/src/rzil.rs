use bitflags::bitflags;
use rzapi::api::RzApi;
use rzapi::api::RzResult;
use rzapi::api::{RzILInfo, RzILVMRegValue};
use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt::Display;
use std::hash::Hash;
use std::rc::Rc;
use std::vec;
use thiserror::Error;
pub mod to_z3;
/*
 * rzapi::RzILInfo -> rz-sym::RzIL(sorted, unrolled) -> Expr.add_solver
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
type RzILResult<T> = std::result::Result<T, RzILError>;

#[derive(Error, Debug)]
pub enum RzILError {
    #[error("Sort {0} was expected, but {1} was found.")]
    UnexpectedSort(Sort, Sort),

    #[error("Sort {0} and {1} should be identical.")]
    SortIntegrity(Sort, Sort),

    #[error("Undefined variable {0} was referenced.")]
    UndefinedVariableReferenced(String),

    #[error("Parse Int failed")]
    ParseInt(#[from] std::num::ParseIntError),

    #[error("String: {0} was not hex-decimal")]
    ParseStringToBv(String),

    #[error("RzApi failed: {0}")]
    ApiError(String),

    #[error("RzIL {0} is Unimplemented")]
    UnimplementedRzILPure(PureCode),

    #[error("RzIL {0} is Unimplemented")]
    UnimplementedRzILEffect(EffectCode),

    #[error("Unkown (Unimplemented or Invalid) RzIL detected.")]
    UnkownRzIL,

    #[error("Empty detected. Unable to continue.")]
    Empty,

    #[error("Invalid access to Effect Args")]
    InvalidAccessToArgs,

    #[error("Variable {0} is immutable but changed")]
    ImmutableVariable(String),

    #[error("Unable to handle Branch Set")]
    BranchSetHandle,

    #[error("Unhandled Error : Postponed RzIL Node")]
    Postponed,

    #[error("Unhandled Error : Vector should have at least 1 element")]
    EmptyVector,
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum Sort {
    Bv(u32),
    Bool,
}
impl Sort {
    pub fn is_bv(&self) -> bool {
        match self {
            Sort::Bv(_) => true,
            Sort::Bool => false,
        }
    }
    pub fn is_bool(&self) -> bool {
        match self {
            Sort::Bv(_) => false,
            Sort::Bool => true,
        }
    }
}
impl std::fmt::Display for Sort {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub enum Scope {
    Global,    // represent registers
    Local,     // inside an Instruction
    LocalPure, // only inside a Let expression
}

pub enum Label {
    Address(u64),
    Syscall,
}

#[derive(Clone, Debug)]
pub enum PureCode {
    Var(Scope, String),
    Ite(Rc<Pure>, Rc<Pure>, Rc<Pure>),
    Let(Rc<Pure>, Rc<Pure>, Rc<Pure>),
    Bool,
    BoolInv(Rc<Pure>),
    BoolAnd(Rc<Pure>, Rc<Pure>),
    BoolOr(Rc<Pure>, Rc<Pure>),
    BoolXor(Rc<Pure>, Rc<Pure>),
    Bitv,
    Msb(Rc<Pure>),
    Lsb(Rc<Pure>),
    IsZero(Rc<Pure>),
    Neg(Rc<Pure>),
    LogNot(Rc<Pure>),
    Add(Rc<Pure>, Rc<Pure>),
    Sub(Rc<Pure>, Rc<Pure>),
    Mul(Rc<Pure>, Rc<Pure>),
    Div(Rc<Pure>, Rc<Pure>),
    Sdiv(Rc<Pure>, Rc<Pure>),
    Mod(Rc<Pure>, Rc<Pure>),
    Smod(Rc<Pure>, Rc<Pure>),
    LogAnd(Rc<Pure>, Rc<Pure>),
    LogOr(Rc<Pure>, Rc<Pure>),
    LogXor(Rc<Pure>, Rc<Pure>),
    ShiftRight(Rc<Pure>, Rc<Pure>, Rc<Pure>),
    ShiftLeft(Rc<Pure>, Rc<Pure>, Rc<Pure>),
    Equal(Rc<Pure>, Rc<Pure>),
    Sle(Rc<Pure>, Rc<Pure>),
    Ule(Rc<Pure>, Rc<Pure>),
    Cast(Rc<Pure>, Rc<Pure>),
    Append(Rc<Pure>, Rc<Pure>),
    Load(Rc<Pure>),
    Loadw(Rc<Pure>),
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
}

impl Display for PureCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}


#[derive(Debug)]
pub struct Pure {
    /*
     * context
     * id // uniq
     * eval
     * sort(BV(size),BOOL)
     * symbolized
     * args Vec<Pure>
     *
     * * fn get_size(&self){
     * match self.sort{
     * BV(size)=>size,
     * BOOL=>1,
     * }
     * }
     * fn get_bitmask(&self){
     * constants::max_size(=64)
     * }
     */
    code: PureCode,
    sort: Sort,
    eval: u64,
    id: u64,
    symbolized: bool,
}

/*
impl Display for Pure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.code)?;
        if let Some(args) = &self.args {
            write!(f, "(")?;
            for arg in args {
                write!(f, "{}", arg)?;
            }
            write!(f, ")")?;
        }
        Ok(())
    }
}
*/

impl Pure {
    fn evaluate(&self) -> u64 {
        self.eval
    }
    fn evaluate_bool(&self) -> bool {
        if self.evaluate() != 0 {
            true
        } else {
            false
        }
    }
    fn get_sort(&self) -> Sort {
        self.sort.clone()
    }
    fn get_id(&self) -> u64 {
        self.id
    }
    fn get_size(&self) -> u32 {
        match self.sort {
            Sort::Bv(len) => len,
            Sort::Bool => 1,
        }
    }
    fn get_bitmask(&self) -> u64 {
        u64::MAX >> (u64::BITS - self.get_size())
    }
    fn is_bv(&self) -> bool {
        self.get_sort().is_bv()
    }
    fn is_bool(&self) -> bool {
        self.get_sort().is_bool()
    }
    fn is_symbolized(&self) -> bool {
        self.symbolized
    }
    fn is_concretized(&self) -> bool {
        !self.symbolized
    }
    fn is_zero(&self) -> bool {
        self.is_bv() && self.is_concretized() && self.evaluate() == 0
    }
    fn concretize(&mut self, eval: u64) {
        self.symbolized = false;
        self.eval = eval;
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum EffectCode {
    Nop,
    Set,
    Jmp,
    Goto,
    Seq,
    Blk,
    Repeat,
    Branch,
    Store,
    Storew,
    Empty,
}

impl Display for EffectCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug)]
pub struct Effect {
    /*
     * context
     * //comment
     * hash
     * effects
     * args Vec<Pure>
     * */
    //context: Weak<RzILContext>,
    code: EffectCode,
    label: Option<String>,
    symbolized: bool,
    pure_args: Option<Vec<Rc<Pure>>>,
    effect_args: Option<Vec<Rc<Effect>>>,
}

impl Effect {
    fn is(&self, code: EffectCode) -> bool {
        self.code == code
    }
    fn is_symbolized(&self) -> bool {
        self.symbolized
    }
}

#[derive(Debug)]
pub struct Instruction {
    /*
     * //thread_id
     * address
     * events
     * expressions Vec<RzILEffect>
     * */
    address: u64,
    expression: Rc<Effect>,
}

bitflags! {
    //#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct RzILContextConfig: u32 {
        const BranchToIte      = 0b00000001;
        const AnalyzeDependencies = 0b00000010;
        const All = Self::BranchToIte.bits() | Self::AnalyzeDependencies.bits();
    }
}

#[derive(Debug)]
struct BranchToIteEntry {
    name: String,
    dst: Rc<Pure>,
    condition: Rc<Pure>,
    then: Option<Rc<Pure>>,
    else_: Option<Rc<Pure>>,
    default: Rc<Pure>,
}

#[derive(Debug)]
struct BranchToIteContext {
    conditions: Vec<Rc<Pure>>,
    taken_path: Vec<bool>,
    entries: Vec<BranchToIteEntry>,
}

pub struct RzILContext {
    /*
     * options Option
     * values HashMap<name, Pure>
     * labels HashMap<label, Label>
     * cache HashMap<hash, Pure>
     * uniq_id
     * */
    option: RzILContextConfig,
    global_vars: HashMap<String, Rc<Pure>>,
    local_vars: HashMap<String, Rc<Pure>>,
    local_pure_vars: HashMap<String, Rc<Pure>>,
    labels: HashMap<String, Label>,
    uniq_id: Cell<u64>,
    branch_to_ite_ctx: BranchToIteContext,
}

impl RzILContext {
    pub fn new() -> Self {
        RzILContext {
            option: RzILContextConfig::All,
            global_vars: HashMap::new(),
            local_vars: HashMap::new(),
            local_pure_vars: HashMap::new(),
            labels: HashMap::new(),
            uniq_id: Cell::new(0),
            branch_to_ite_ctx: BranchToIteContext {
                conditions: Vec::new(),
                taken_path: Vec::new(),
                entries: Vec::new(),
            },
        }
    }
    fn clear(&mut self) {
       self.local_vars.clear();
       self.local_pure_vars.clear();
       self.branch_to_ite_ctx.conditions.clear();
       self.branch_to_ite_ctx.taken_path.clear();
       self.branch_to_ite_ctx.entries.clear();
    }
    pub fn bind_registers(&mut self, api: &mut RzApi) -> RzILResult<()> {
        let rzilvm_status = self.api_result(api.get_rzil_vm_status())?;
        for (k, v) in &rzilvm_status {
            let sort = match v {
                RzILVMRegValue::String(string) => {
                    if !string.starts_with("0x") {
                        return Err(RzILError::ParseStringToBv(string.to_owned()));
                    }
                    let size = (string.len() as u32 - 2) * 4;
                    Sort::Bv(size)
                }
                RzILVMRegValue::Bool(_) => Sort::Bool,
                _ => {
                    continue;
                }
            };
            let id = self.get_uniq_id();
            self.global_vars.insert(
                k.to_owned(),
                Rc::new(Pure {
                    code: PureCode::Var(Scope::Global, k.to_owned()),
                    args: None,
                    sort,
                    eval: 0,
                    id,
                    symbolized: true,
                }),
            );
        }
        Ok(())
    }

    pub fn lift_inst(&mut self, api: &mut RzApi, addr: u64) -> RzILResult<Instruction> {
        match self.lift_n_insts(api, addr, 1) {
            Ok(vec) => Ok(vec.into_iter().nth(0).unwrap()),
            Err(e) => Err(e),
        }
    }
    pub fn lift_n_insts(
        &mut self,
        api: &mut RzApi,
        addr: u64,
        n: u64,
    ) -> RzILResult<Vec<Instruction>> {
        let insts = self.api_result(api.get_n_insts(Some(n), Some(addr)))?;
        let mut vec = Vec::new();
        for inst in insts {
            self.clear();
            vec.push(Instruction {
                address: inst.addr,
                expression: self.create_effect(&inst.rzil)?,
            });
        }
        Ok(vec)
    }

    fn api_result<T>(&self, result: RzResult<T>) -> RzILResult<T> {
        match result {
            Ok(res) => Ok(res),
            Err(e) => Err(RzILError::ApiError(e)),
        }
    }

    fn get_uniq_id(&self) -> u64 {
        self.uniq_id.set(self.uniq_id.get() + 1);
        self.uniq_id.get()
    }
    fn create_pure_bv(&mut self, op: &RzILInfo) -> RzILResult<Rc<Pure>> {
        let bitvector = self.create_pure(op)?;
        if !bitvector.is_bv() {
            Err(RzILError::UnexpectedSort(Sort::Bv(0), bitvector.get_sort()))
        } else {
            Ok(bitvector)
        }
    }
    fn create_pure_bool(&mut self, op: &RzILInfo) -> RzILResult<Rc<Pure>> {
        let boolean = self.create_pure(op)?;
        if !boolean.is_bool() {
            Err(RzILError::UnexpectedSort(Sort::Bool, boolean.get_sort()))
        } else {
            Ok(boolean)
        }
    }
    fn create_pure(&mut self, op: &RzILInfo) -> RzILResult<Rc<Pure>> {
        match op {
            RzILInfo::Var { value } => 
                if let Some(var) = self.global_vars.get(value) {
                    Ok(var.clone())
                }else if let Some(var) = self.local_vars.get(value) {
                    Ok(var.clone())
                }else if let Some(var) = self.local_pure_vars.get(value) {
                    Ok(var.clone())
                }else{
                    Err(RzILError::UndefinedVariableReferenced(value.to_owned()))
                }
            RzILInfo::Ite { condition, x, y } => {
                let condition = self.create_pure_bool(condition)?;
                let x = self.create_pure(x)?;
                let y = self.create_pure(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let sort = x.get_sort();
                let eval = if condition.evaluate() != 0 {
                    x.evaluate()
                } else {
                    y.evaluate()
                };
                let symbolized = condition.is_symbolized()
                    || (condition.evaluate() != 0 && x.is_symbolized())
                    || (condition.evaluate() == 0 && y.is_symbolized());
                let id = self.get_uniq_id();
                Ok(Rc::new(Pure {
                    code: PureCode::Ite(condition, x, y),
                    sort,
                    eval,
                    id,
                    symbolized,
                }))
            }
            RzILInfo::Let { dst, exp, body } => {
                let binding = self.create_pure(exp)?;
                let var = self.fresh_var(dst, Scope::LocalPure, &binding);
                self.local_pure_vars.insert(dst.to_owned(), var.clone());
                let body = self.create_pure(body)?;
                let sort = body.get_sort();
                let eval = body.evaluate();
                let id = self.get_uniq_id();
                let symbolized = body.is_symbolized();
                self.local_pure_vars.remove(dst);

                Ok(Rc::new(Pure {
                    code: PureCode::Let(var, binding, body),
                    sort,
                    eval,
                    id,
                    symbolized,
                }))
            }
            RzILInfo::Bool { value } => Ok(Rc::new(Pure {
                code: PureCode::Bool,
                sort: Sort::Bool,
                eval: if value.clone() { 1 } else { 0 },
                id: self.get_uniq_id(),
                symbolized: false,
            })),
            RzILInfo::BoolInv { x } => {
                let x = self.create_pure_bool(x)?;
                let eval = !(x.evaluate() != 0) as u64;
                let symbolized = x.is_symbolized();
                Ok(Rc::new(Pure {
                    code: PureCode::BoolInv(x),
                    sort: Sort::Bool,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::BoolAnd { x, y } => {
                let x = self.create_pure_bool(x)?;
                let y = self.create_pure_bool(y)?;
                let eval = x.evaluate() & y.evaluate();
                let symbolized = x.is_symbolized() | y.is_symbolized();
                Ok(Rc::new(Pure {
                    code: PureCode::BoolAnd(x, y),
                    sort: Sort::Bool,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::BoolOr { x, y } => {
                let x = self.create_pure_bool(x)?;
                let y = self.create_pure_bool(y)?;
                let eval = x.evaluate() | y.evaluate();
                let symbolized = x.is_symbolized() | y.is_symbolized();
                Ok(Rc::new(Pure {
                    code: PureCode::BoolOr(x, y),
                    sort: Sort::Bool,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::BoolXor { x, y } => {
                let x = self.create_pure_bool(x)?;
                let y = self.create_pure_bool(y)?;
                let eval = x.evaluate() ^ y.evaluate();
                let symbolized = x.is_symbolized() | y.is_symbolized();
                Ok(Rc::new(Pure {
                    code: PureCode::BoolXor(x, y),
                    sort: Sort::Bool,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::Bitv { bits, len } => Ok(Rc::new(Pure {
                code: PureCode::Bitv,
                sort: Sort::Bv(len.clone()),
                eval: u64::from_str_radix(&bits[2..], 16)?,
                id: self.get_uniq_id(),
                symbolized: false,
            })),
            RzILInfo::Msb { bv } => {
                let bv = self.create_pure_bv(bv)?;
                let sort = Sort::Bool;
                let eval = (bv.evaluate() >> (bv.get_size() - 1)) & 0x1;
                let symbolized = bv.is_symbolized();
                Ok(Rc::new(Pure {
                    code: PureCode::Msb(bv),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::Lsb { bv } => {
                let bv = self.create_pure_bv(bv)?;
                let sort = Sort::Bool;
                let eval = bv.evaluate() & 0x1;
                let symbolized = bv.is_symbolized();
                Ok(Rc::new(Pure {
                    code: PureCode::Lsb(bv),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::IsZero { bv } => {
                let bv = self.create_pure_bv(bv)?;
                let sort = Sort::Bool;
                let eval = if bv.evaluate() != 0 { 1 } else { 0 };
                let symbolized = bv.is_symbolized();
                Ok(Rc::new(Pure {
                    code: PureCode::IsZero(bv),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::Neg { bv } => {
                let bv = self.create_pure_bv(bv)?;
                let sort = Sort::Bv(bv.get_size());
                let eval = (bv.evaluate() as i64 * -1i64) as u64;
                let symbolized = bv.is_symbolized();
                Ok(Rc::new(Pure {
                    code: PureCode::Neg(bv),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::LogNot { bv } => {
                let bv = self.create_pure_bv(bv)?;
                let sort = Sort::Bv(bv.get_size());
                let symbolized = bv.is_symbolized();
                let eval = !bv.evaluate();
                Ok(Rc::new(Pure {
                    code: PureCode::LogNot(bv),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::Add { x, y } => {
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let sort = Sort::Bv(x.get_size());
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized {
                    (x.evaluate() + y.evaluate()) & x.get_bitmask()
                } else {
                    0
                };
                if x.is_zero() {
                    Ok(y)
                } else if y.is_zero() {
                    Ok(x)
                } else {
                    Ok(Rc::new(Pure {
                        code: PureCode::Add(x, y),
                        sort,
                        eval,
                        id: self.get_uniq_id(),
                        symbolized,
                    }))
                }
            }
            RzILInfo::Sub { x, y } => {
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let sort = Sort::Bv(x.get_size());
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized {
                    (x.evaluate() - y.evaluate()) & x.get_bitmask()
                } else {
                    0
                };
                if x.is_zero() {
                    Ok(y)
                } else if y.is_zero() {
                    Ok(x)
                } else {
                    Ok(Rc::new(Pure {
                        code: PureCode::Sub(x, y),
                        sort,
                        eval,
                        id: self.get_uniq_id(),
                        symbolized,
                    }))
                }
            }
            RzILInfo::Mul { x, y } => {
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let sort = Sort::Bv(x.get_size());
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized {
                    (x.evaluate() * y.evaluate()) & x.get_bitmask()
                } else {
                    0
                };
                Ok(Rc::new(Pure {
                    code: PureCode::Mul(x, y),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::Div { x, y } => {
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let sort = Sort::Bv(x.get_size());
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized && y.evaluate() != 0 {
                    (x.evaluate() / y.evaluate()) & x.get_bitmask()
                } else {
                    0
                };
                Ok(Rc::new(Pure {
                    code: PureCode::Div(x, y),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::Sdiv { x, y } => {
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let sort = Sort::Bv(x.get_size());
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized && y.evaluate() != 0 {
                    ((x.evaluate() as i64 / y.evaluate() as i64) as u64) & x.get_bitmask()
                } else {
                    0
                };
                Ok(Rc::new(Pure {
                    code: PureCode::Sdiv(x, y),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::Mod { x, y } => {
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let sort = Sort::Bv(x.get_size());
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized && y.evaluate() != 0 {
                    (x.evaluate() % y.evaluate()) & x.get_bitmask()
                } else {
                    0
                };
                Ok(Rc::new(Pure {
                    code: PureCode::Mod(x, y),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::Smod { x, y } => {
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let sort = Sort::Bv(x.get_size());
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized && y.evaluate() != 0 {
                    ((x.evaluate() as i64 % y.evaluate() as i64) as u64) & x.get_bitmask()
                } else {
                    0
                };
                Ok(Rc::new(Pure {
                    code: PureCode::Smod(x, y),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::LogAnd { x, y } => {
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let sort = Sort::Bv(x.get_size());
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized {
                    x.evaluate() & y.evaluate()
                } else {
                    0
                };
                if x.is_zero() || y.is_zero() {
                    Ok(Rc::new(Pure {
                        code: PureCode::Bitv,
                        sort,
                        eval: 0,
                        id: self.get_uniq_id(),
                        symbolized: false,
                    }))
                } else {
                    Ok(Rc::new(Pure {
                        code: PureCode::LogAnd(x, y),
                        sort,
                        eval,
                        id: self.get_uniq_id(),
                        symbolized,
                    }))
                }
            }
            RzILInfo::LogOr { x, y } => {
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let sort = Sort::Bv(x.get_size());
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized {
                    x.evaluate() | y.evaluate()
                } else {
                    0
                };
                if x.is_zero() {
                    Ok(y)
                } else if y.is_zero() {
                    Ok(x)
                } else {
                    Ok(Rc::new(Pure {
                        code: PureCode::LogOr(x, y),
                        sort,
                        eval,
                        id: self.get_uniq_id(),
                        symbolized,
                    }))
                }
            }
            RzILInfo::LogXor { x, y } => {
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let sort = Sort::Bv(x.get_size());
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized {
                    x.evaluate() ^ y.evaluate()
                } else {
                    0
                };
                if x.is_zero() {
                    Ok(y)
                } else if y.is_zero() {
                    Ok(x)
                } else {
                    Ok(Rc::new(Pure {
                        code: PureCode::LogXor(x, y),
                        sort,
                        eval,
                        id: self.get_uniq_id(),
                        symbolized,
                    }))
                }
            }
            RzILInfo::ShiftRight { fill_bit, x, y } => {
                let fill_bit = self.create_pure_bool(fill_bit)?;
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                let sort = Sort::Bv(x.get_size());
                let symbolized = fill_bit.is_symbolized() | x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized {
                    ((x.evaluate() >> y.evaluate())
                        | if fill_bit.evaluate_bool() {
                            u64::MAX & !(x.get_bitmask() >> y.evaluate())
                        } else {
                            0
                        })
                        & x.get_bitmask()
                } else {
                    0
                };
                Ok(Rc::new(Pure {
                    code: PureCode::ShiftRight(fill_bit, x, y),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::ShiftLeft { fill_bit, x, y } => {
                let fill_bit = self.create_pure_bool(fill_bit)?;
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                let sort = Sort::Bv(x.get_size());
                let symbolized = fill_bit.is_symbolized() | x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized {
                    ((x.evaluate() << y.evaluate())
                        | if fill_bit.evaluate_bool() {
                            (1 << y.evaluate()) - 1
                        } else {
                            0
                        })
                        & x.get_bitmask()
                } else {
                    0
                };
                Ok(Rc::new(Pure {
                    code: PureCode::ShiftLeft(fill_bit, x, y),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::Equal { x, y } => {
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let sort = Sort::Bool;
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized {
                    if x.evaluate() == y.evaluate() {
                        1
                    } else {
                        0
                    }
                } else {
                    0
                };
                Ok(Rc::new(Pure {
                    code: PureCode::Equal(x, y),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::Sle { x, y } => {
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let sort = Sort::Bool;
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized && x.evaluate() as i64 >= y.evaluate() as i64 {
                    1
                } else {
                    0
                };
                Ok(Rc::new(Pure {
                    code: PureCode::Sle(x, y),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::Ule { x, y } => {
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let sort = Sort::Bool;
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized && x.evaluate() >= y.evaluate() {
                    1
                } else {
                    0
                };
                Ok(Rc::new(Pure {
                    code: PureCode::Ule(x, y),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::Cast {
                value,
                length,
                fill,
            } => {
                let value = self.create_pure_bv(value)?;
                let length = length.clone();
                let fill = self.create_pure_bool(fill)?;
                let sort = Sort::Bv(length);
                let symbolized = value.is_symbolized() | fill.is_symbolized();
                let eval = if !symbolized {
                    if value.get_size() >= length {
                        value.evaluate() & (1 << length - 1)
                    } else {
                        let fill_bits = (fill.evaluate() << length - 1) & !value.get_bitmask();
                        value.evaluate() | fill_bits
                    }
                } else {
                    0
                };
                if value.get_sort() == sort {
                    Ok(value)
                } else {
                    Ok(Rc::new(Pure {
                        code: PureCode::Cast(value, fill),
                        sort,
                        eval,
                        id: self.get_uniq_id(),
                        symbolized,
                    }))
                }
            }
            RzILInfo::Append { high, low } => {
                let high = self.create_pure_bv(high)?;
                let low = self.create_pure_bv(low)?;
                let sort = Sort::Bv(high.get_size() + low.get_size());
                let symbolized = high.is_symbolized() | low.is_symbolized();
                let eval = if !symbolized {
                    (high.evaluate() << low.get_size()) | low.evaluate()
                } else {
                    0
                };
                Ok(Rc::new(Pure {
                    code: PureCode::Append(high, low),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::Load { mem: _, key } => {
                let key = self.create_pure_bv(key)?;
                let sort = Sort::Bv(8);
                Ok(Rc::new(Pure {
                    code: PureCode::Load(key),
                    sort,
                    eval: 0,
                    id: self.get_uniq_id(),
                    symbolized: true,
                }))
            }
            RzILInfo::Loadw { mem: _, key, bits } => {
                let key = self.create_pure_bv(key)?;
                let sort = Sort::Bv(bits.to_owned() as u32);
                Ok(Rc::new(Pure {
                    code: PureCode::Loadw(key),
                    sort,
                    eval: 0,
                    id: self.get_uniq_id(),
                    symbolized: true,
                }))
            },
            RzILInfo::Float {
                format: _,
                bv: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Float)),
            RzILInfo::Fbits {
                f: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fbits)),
            RzILInfo::IsFinite {
                f: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::IsFinite)),
            RzILInfo::IsNan {
                f: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::IsNan)),
            RzILInfo::IsInf {
                f: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::IsInf)),
            RzILInfo::IsFzero {
                f: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::IsFzero)),
            RzILInfo::IsFneg {
                f: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::IsFneg)),
            RzILInfo::IsFpos {
                f: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::IsFpos)),
            RzILInfo::Fneg {
                f: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fneg)),
            RzILInfo::Fpos {
                f: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fneg)),
            RzILInfo::FcastInt {
                length: _,
                rmode: _,
                value: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::FcastInt)),
            RzILInfo::FcastSint {
                length: _,
                rmode: _,
                value: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::FcastSint)),
            RzILInfo::FcastFloat {
                format: _,
                rmode: _,
                value: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::FcastFloat)),
            RzILInfo::FcastSfloat {
                format: _,
                rmode: _,
                value: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::FcastSfloat)),
            RzILInfo::Fconvert {
                format: _,
                rmode: _,
                value: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fconvert)),
            RzILInfo::Fround {
                rmode: _,
                value: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fround)),
            RzILInfo::Frequal {
                rmode_x: _,
                rmode_y: _,
                value: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Frequal)),
            RzILInfo::Fsucc {
                f: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fsucc)),
            RzILInfo::Fpred {
                f: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fpred)),
            RzILInfo::Forder {
                x: _,
                y: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Forder)),
            RzILInfo::Fsqrt {
                rmode: _,
                f: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fsqrt)),
            RzILInfo::Frsqrt {
                rmode: _,
                f: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Frsqrt)),
            RzILInfo::Fadd {
                rmode: _,
                x: _,
                y: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fadd)),
            RzILInfo::Fsub {
                rmode: _,
                x: _,
                y: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fsub)),
            RzILInfo::Fmul {
                rmode: _,
                x: _,
                y: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fmul)),
            RzILInfo::Fdiv {
                rmode: _,
                x: _,
                y: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fdiv)),
            RzILInfo::Fmod {
                rmode: _,
                x: _,
                y: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fmod)),
            RzILInfo::Hypot {
                rmode: _,
                x: _,
                y: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Hypot)),
            RzILInfo::Pow {
                rmode: _,
                x: _,
                y: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Pow)),
            RzILInfo::Fmad {
                rmode: _,
                x: _,
                y: _,
                z: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fmad)),
            RzILInfo::Fpown {
                rmode: _,
                f: _,
                n: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fpown)),
            RzILInfo::Frootn {
                rmode: _,
                f: _,
                n: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Frootn)),
            RzILInfo::Fcompound {
                rmode: _,
                f: _,
                n: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fcompound)),
            _ => Err(RzILError::UnkownRzIL),
        }
    }

    fn create_effect(&mut self, op: &RzILInfo) -> RzILResult<Rc<Effect>> {
        match op {
            RzILInfo::Nop => Ok(Rc::new(Effect {
                code: EffectCode::Nop,
                label: None,
                symbolized: false,
                pure_args: None,
                effect_args: None,
            })),
            RzILInfo::Set { dst, src } => {
                let name = dst;
                let src = self.create_pure(src)?;
                let dst = match self.global_vars.get(name) {
                    Some(var) => {
                        if src.get_sort() != var.get_sort() {
                            return Err(RzILError::SortIntegrity(src.get_sort(), var.get_sort()));
                        }
                        self.fresh_var(
                            name,
                            Scope::Global,
                            &src,
                        )
                    }
                    None => self.fresh_var(
                        name,
                        Scope::Local,
                        &src,
                    ),
                };
                if self.in_branch() && self.option.contains(RzILContextConfig::BranchToIte) {
                    self.branch_to_ite_add_entry(name,src,dst)?;
                    return Err(RzILError::Postponed);
                }
                self.update_var(dst.clone())?;
                let symbolized = dst.is_symbolized();
                Ok(Rc::new(Effect {
                    code: EffectCode::Set,
                    label: None,
                    symbolized,
                    pure_args: Some(vec![dst, src]),
                    effect_args: None,
                }))
            }

            RzILInfo::Jmp { dst } => {
                let dst = self.create_pure_bv(dst)?;
                let symbolized = dst.is_symbolized();
                Ok(Rc::new(Effect {
                    code: EffectCode::Jmp,
                    label: None,
                    symbolized,
                    pure_args: Some(vec![dst]),
                    effect_args: None,
                }))
            }

            RzILInfo::Goto { label } => Ok(Rc::new(Effect {
                code: EffectCode::Goto,
                label: Some(label.to_owned()),
                symbolized: false,
                pure_args: None,
                effect_args: None,
            })),

            RzILInfo::Seq { x, y } => {
                let mut args = Vec::new();
                self.create_effect_seq(x, &mut args)?;
                self.create_effect_seq(y, &mut args)?;
                if args.len() == 0 {
                    return Ok(self.create_nop());
                } else if args.len() == 1 {
                    return Ok(args.pop().unwrap());
                }
                let mut symbolized = false;
                for arg in &args {
                    symbolized |= arg.is_symbolized();
                }
                Ok(Rc::new(Effect {
                    code: EffectCode::Seq,
                    label: None,
                    symbolized,
                    pure_args: None,
                    effect_args: Some(args),
                }))
            }
            RzILInfo::Branch {
                condition,
                true_eff,
                false_eff,
            } => {
                let condition = self.create_pure_bool(condition)?;

                self.branch_to_ite_ctx.conditions.push(condition.clone());

                self.branch_to_ite_ctx.taken_path.push(true);
                let then_effect = self.create_effect(true_eff)?;
                self.branch_to_ite_ctx.taken_path.pop();

                self.branch_to_ite_ctx.taken_path.push(false);
                let else_effect = self.create_effect(false_eff)?;
                self.branch_to_ite_ctx.taken_path.pop();

                self.branch_to_ite_ctx.conditions.pop();

                let mut args = Vec::new();
                if !self.in_branch() {
                    let mut ids = Vec::new();
                    for i in 0..self.branch_to_ite_ctx.entries.len() {
                        ids.push(self.get_uniq_id());
                    }
                    ids.reverse();
                    for entry in self.branch_to_ite_ctx.entries.drain(..) {
                        let then = entry.then.unwrap_or(entry.default.clone());
                        let else_ = entry.else_.unwrap_or(entry.default.clone());
                        if then.get_sort() != else_.get_sort() {
                            return Err(RzILError::SortIntegrity(
                                then.get_sort(),
                                else_.get_sort(),
                            ));
                        }
                        let sort = then.get_sort();
                        let eval = if condition.evaluate() != 0 {
                            then.evaluate()
                        } else {
                            else_.evaluate()
                        };
                        let symbolized = condition.is_symbolized()
                            || (condition.evaluate() != 0 && then.is_symbolized())
                            || (condition.evaluate() == 0 && else_.is_symbolized());
                        let id = ids.pop().unwrap();
                        let ite = Rc::new(Pure {
                            code: PureCode::Ite(entry.condition.clone(), then, else_),
                            sort,
                            eval,
                            id,
                            symbolized,
                        });
                        args.push(Rc::new(Effect {
                            code: EffectCode::Set,
                            label: None,
                            symbolized,
                            pure_args: Some(vec![entry.dst, ite]),
                            effect_args: None,
                        }));
                    }
                }
                let need_branch =
                    !then_effect.is(EffectCode::Nop) || !else_effect.is(EffectCode::Nop);
                let symbolized = condition.is_symbolized()
                    || (condition.evaluate() != 0 && then_effect.is_symbolized())
                    || (condition.evaluate() == 0 && else_effect.is_symbolized());
                if need_branch {
                    args.push(Rc::new(Effect {
                        code: EffectCode::Branch,
                        label: None,
                        symbolized,
                        pure_args: Some(vec![condition]),
                        effect_args: Some(vec![then_effect, else_effect]),
                    }))
                }
                if args.len() == 0 {
                    Ok(self.create_nop())
                } else if args.len() == 1 {
                    Ok(args.pop().unwrap())
                } else {
                    let mut symbolized = false;
                    for arg in &args {
                        symbolized |= arg.is_symbolized();
                    }
                    Ok(Rc::new(Effect {
                        code: EffectCode::Seq,
                        label: None,
                        symbolized,
                        pure_args: None,
                        effect_args: Some(args),
                    }))
                }
            }
            RzILInfo::Store { mem: _, key, value } => {
                let key = self.create_pure_bv(key)?;
                let value = self.create_pure_bv(value)?;
                let symbolized = key.is_symbolized() || value.is_symbolized();
                Ok(Rc::new(Effect {
                    code: EffectCode::Store,
                    label: None,
                    symbolized,
                    pure_args: Some(vec![key, value]),
                    effect_args: None,
                }))
            }
            RzILInfo::Storew { mem: _, key, value } => {
                let key = self.create_pure_bv(key)?;
                let value = self.create_pure_bv(value)?;
                let symbolized = key.is_symbolized() || value.is_symbolized();
                Ok(Rc::new(Effect {
                    code: EffectCode::Storew,
                    label: None,
                    symbolized,
                    pure_args: Some(vec![key, value]),
                    effect_args: None,
                }))
            }
            RzILInfo::Blk {
                label: _,
                data: _,
                ctrl: _,
            } => Err(RzILError::UnimplementedRzILEffect(EffectCode::Blk)),
            RzILInfo::Repeat {
                condition: _,
                data_eff: _,
            } => Err(RzILError::UnimplementedRzILEffect(EffectCode::Repeat)),//Ok(self.create_nop()), //
            RzILInfo::Empty => Err(RzILError::Empty),
            _ => Err(RzILError::UnkownRzIL),
        }
    }

    fn create_nop(&mut self) -> Rc<Effect> {
        Rc::new(Effect {
            code: EffectCode::Nop,
            label: None,
            symbolized: false,
            pure_args: None,
            effect_args: None,
        })
    }

    fn create_effect_seq(&mut self, op: &RzILInfo, vec: &mut Vec<Rc<Effect>>) -> RzILResult<()> {
        match op {
            RzILInfo::Seq { x, y } => {
                self.create_effect_seq(x, vec)?;
                self.create_effect_seq(y, vec)?;
            }
            _ => {
                match self.create_effect(op) {
                    Ok(ret) => {
                        if ret.is(EffectCode::Seq) {
                            if let Some(args) = ret.effect_args {
                                for e in args {
                                    vec.push(e.clone())
                                }
                            }
                        }else{
                            vec.push(ret)
                        }
                    },
                    Err(RzILError::Postponed) => (),
                    Err(err) => return Err(err),
                };
            }
        };
        Ok(())
    }

    fn fresh_var(
        &mut self,
        name: &str,
        scope: Scope,
        src: &Rc<Pure>,
    ) -> Rc<Pure> {
        Rc::new(Pure {
            code: PureCode::Var(scope, name.to_owned()),
            args: None,
            sort: src.get_sort(), //self.regs.get(value)?.get_sort()
            eval: src.evaluate(),
            id: self.get_uniq_id(),
            symbolized: src.is_symbolized(),
        })
    }
    fn update_var(&mut self, new_var: Rc<Pure>) -> RzILResult<()> {
        if let PureCode::Var(scope, name) = &new_var.code {
            let sort = new_var.get_sort();
            match scope {
                Scope::Global => match self.global_vars.get(name) {
                    Some(var) => {
                        if var.get_sort() != sort {
                            return Err(RzILError::SortIntegrity(var.get_sort(), sort));
                        }
                        self.global_vars.insert(name.to_owned(), new_var);
                    }
                    None => return Err(RzILError::UndefinedVariableReferenced(name.to_owned())),
                },
                Scope::Local => match self.local_vars.get(name) {
                    None => {
                        self.local_vars.insert(name.to_owned(), new_var);
                    }
                    Some(_) => return Err(RzILError::ImmutableVariable(name.to_owned())),
                },
                Scope::LocalPure => return Err(RzILError::ImmutableVariable(name.to_owned())),
            }
            Ok(())
        } else {
            Err(RzILError::UndefinedVariableReferenced(
                "Not Variable".to_owned(),
            ))
        }
    }

    fn in_branch(&self) -> bool {
        !self.branch_to_ite_ctx.conditions.is_empty() && !self.branch_to_ite_ctx.taken_path.is_empty()
    }

    fn connect_condition(&self, conditions: &[Rc<Pure>], taken: &[bool]) -> RzILResult<Rc<Pure>> {
        if conditions.is_empty() {
            return Err(RzILError::EmptyVector);
        }
        let mut x = conditions.first().unwrap().clone();
        if !x.is_bool() {
            return Err(RzILError::UnexpectedSort(Sort::Bool, x.get_sort()));
        }
        if taken.len() > 1 {
            let taken = taken.first().unwrap();
            if !taken {
                let eval = !(x.evaluate() != 0) as u64;
                let symbolized = x.is_symbolized();
                x = Rc::new(Pure {
                    code: PureCode::BoolInv(x),
                    sort: Sort::Bool,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                });
            }
        }
        match self.connect_condition(&conditions[1..], &taken[1..]) {
            Ok(y) => {
                let eval = x.evaluate() & y.evaluate();
                let symbolized = x.is_symbolized() | y.is_symbolized();
                Ok(Rc::new(Pure {
                    code: PureCode::BoolAnd(x,y),
                    sort: Sort::Bool,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            Err(RzILError::EmptyVector) => Ok(x),
            Err(e) => Err(e),
        }
    }
    fn branch_to_ite_add_entry(&mut self, name: &str, src: Rc<Pure>, dst: Rc<Pure> ) -> RzILResult<()> {
        let condition =
            self.connect_condition(&self.branch_to_ite_ctx.conditions, &self.branch_to_ite_ctx.taken_path)?;
        let taken = self.branch_to_ite_ctx.taken_path.first().unwrap().clone();
        let mut entry = None;
        for e in self.branch_to_ite_ctx.entries.iter_mut() {
            if e.name.eq(name) {
                entry = Some(e);
            }
        }
        match entry {
            Some(e) => {
                if taken {
                    if let Some(_) = e.then {
                        return Err(RzILError::BranchSetHandle);
                    }
                    e.then = Some(src);
                } else {
                    if let Some(_) = e.else_ {
                        return Err(RzILError::BranchSetHandle);
                    }
                    e.else_ = Some(src);
                }
            }
            None=>{
                let default = match self.global_vars.get(name) {
                    Some(var) => var.clone(),
                    None => {
                        let code = match dst.get_sort() {
                            Sort::Bv(_) => PureCode::Bitv,
                            Sort::Bool => PureCode::Bool,
                        };
                        Rc::new(Pure {
                            code,
                            args: None,
                            sort: dst.get_sort(),
                            eval: 0,
                            id: self.get_uniq_id(),
                            symbolized: false,
                        })
                    }
                };
                if taken {
                    self.branch_to_ite_ctx.entries.push(
                        BranchToIteEntry {
                            name:name.to_owned(),
                            dst: dst.clone(),
                            condition,
                            then: Some(src),
                            else_: None,
                            default,
                        },
                    );
                } else {
                    self.branch_to_ite_ctx.entries.push(
                        BranchToIteEntry {
                            name: name.to_owned(),
                            dst: dst.clone(),
                            condition,
                            then: None,
                            else_: Some(src),
                            default,
                        },
                    );
                }
            }
        };
        self.update_var(dst)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests_x86 {
    use super::*;
    use rzapi::api;
    fn init(rzapi: &mut api::RzApi) -> RzILContext {
        let mut ctx = RzILContext::new();
        ctx.bind_registers(rzapi).expect("failed to bind_registers");
        ctx
    }
    #[test]
    fn xor() {
        let mut rzapi = api::RzApi::new(Some("/bin/ls"), None).unwrap();
        let mut ctx = init(&mut rzapi);
        dbg!(ctx.lift_inst(&mut rzapi, 0x67d4).unwrap());
    }
    #[test]
    fn sub() {
        let mut rzapi = api::RzApi::new(Some("/bin/ls"), None).unwrap();
        let mut ctx = init(&mut rzapi);
        dbg!(ctx.lift_inst(&mut rzapi, 0x4e04).unwrap());
    }
    #[test]
    fn shl() {
        let mut rzapi = api::RzApi::new(Some("/bin/ls"), None).unwrap();
        let mut ctx = init(&mut rzapi);
        dbg!(ctx.lift_inst(&mut rzapi, 0x6a44).unwrap());
    }
}
