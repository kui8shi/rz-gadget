use bitflags::bitflags;
use rzapi::api::RzApi;
use rzapi::api::RzResult;
use rzapi::structs::{RzILInfo,RzILVMRegValue};
use std::cell::Cell;
use std::collections::hash_map::{HashMap,DefaultHasher};
use std::collections::hash_set::HashSet;
use std::fmt::Display;
use std::hash::Hasher;
use std::rc::Rc;
use std::vec;
use std::hash::Hash;
use thiserror::Error;
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

    #[error("Code {0} was unexpected.")]
    UnexpectedCode(Code),

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

    #[error("Unable to convert Set in Branch to Set with Ite")]
    BranchSetToSetIteFailed,

    #[error("Unhandled Error : RzIL Node no need or postponed to evaluate")]
    EmptyNode,
}

#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub enum Sort {
    Bitv(usize),
    Bool,
    None,
}
impl Sort {
    fn is_bv(&self) -> bool {
        match self {
            Sort::Bitv(_) => true,
            Sort::Bool => false,
            Sort::None => false,
        }
    }
    fn is_bool(&self) -> bool {
        match self {
            Sort::Bitv(_) => false,
            Sort::Bool => true,
            Sort::None => false,
        }
    }
    fn get_size(&self) -> usize {
        match self {
            Sort::Bitv(len) => len.clone(),
            Sort::Bool => 1,
            Sort::None => 0,
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
    Global,    // represent registers
    Local,     // inside an Instruction
    LocalPure, // only inside a Let expression
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum PureCode {
    Var(Scope, u64), // (scope, var_id)
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
    LoadW,
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
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
    StoreW,
    Empty,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Code {
    Pure(PureCode),
    Effect(EffectCode),
}

impl Display for EffectCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Display for PureCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Display for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct RzIL {
    code: Code,
    symbolized: bool,
    sort: Sort,             // if code is not Pure, it has Sort::None.
    eval: u64,              // if symbolized or code is not Pure, it has 0.
    label: Option<String>,  // if code is not Effect, is has None
    args: Vec<Rc<RzIL>>,
    hash: u64,              // special for the semantics of the code and its children
}

impl Hash for RzIL {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.code.hash(state);
        for arg in &self.args {
            arg.hash.hash(state);
        }
        self.symbolized.hash(state);
        self.sort.hash(state);
        self.eval.hash(state);
        self.label.hash(state);
    }
}

impl RzIL {
    fn is_pure(&self) -> bool {
        match self.code {
            Code::Pure(_) => true,
            Code::Effect(_) => false,
        }
    }
    fn is_effect(&self) -> bool {
        match self.code {
            Code::Pure(_) => false,
            Code::Effect(_) => true,
        }
    }
    fn is_nop(&self) -> bool {
        if let Code::Effect(EffectCode::Nop) = self.code {
            true
        } else {
            false
        }
    }
    pub fn evaluate(&self) -> u64 {
        self.eval
    }
    pub fn evaluate_bool(&self) -> bool {
        if self.evaluate() != 0 {
            true
        } else {
            false
        }
    }
    pub fn get_sort(&self) -> Sort {
        self.sort.clone()
    }
    pub fn get_size(&self) -> usize {
        self.sort.get_size()
        
    }
    fn get_bitmask(&self) -> u64 {
        u64::MAX >> (u64::BITS as usize - self.get_size())
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

bitflags! {
    //#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct RzILContextConfig: u64 {
        const ConvertBranchSetToSetIte      = 0b00000001;
        const AnalyzeDependencies = 0b00000010;
        const All = Self::ConvertBranchSetToSetIte.bits() | Self::AnalyzeDependencies.bits();
    }
}

#[derive(Debug)]
struct BranchSetToSetIteEntry {
    name: String,
    dst: Rc<RzIL>,
    condition: Rc<RzIL>,
    then: Option<Rc<RzIL>>,
    otherwise: Option<Rc<RzIL>>,
    default: Rc<RzIL>,
}

#[derive(Debug)]
struct BranchSetToSetIte {
    conditions: Vec<Rc<RzIL>>,
    taken: Vec<bool>,
    entries: Vec<BranchSetToSetIteEntry>,
}

pub struct RzILContext {
    /*
     * options Option
     * values HashMap<name, Pure>
     * cache HashMap<hash, Pure>
     * uniq_id
     * */
    option: RzILContextConfig,
    var_ids: HashMap<(String,Scope), u64>,
    vars: HashMap<u64, Rc<RzIL>>,
    cache: HashSet<Rc<RzIL>>,
    uniq_var_id: Cell<u64>,
    bs_to_si: BranchSetToSetIte,
}

impl BranchSetToSetIte {
    fn clear(&mut self) {
        self.conditions.clear();
        self.taken.clear();
        self.entries.clear();
    }
}

impl RzILContext {
    pub fn new() -> Self {
        RzILContext {
            option: RzILContextConfig::All,
            var_ids: HashMap::new(),
            vars: HashMap::new(),
            cache: HashSet::new(),
            uniq_var_id: Cell::new(0),
            bs_to_si: BranchSetToSetIte {
                conditions: Vec::new(),
                taken: Vec::new(),
                entries: Vec::new(),
            },
        }
    }
    fn full_clear(&mut self) {
        self.partial_clear();
        self.var_ids.clear();
        self.vars.clear();
    }

    fn partial_clear(&mut self) {
        let ids_to_remove: Vec<Option<u64>> = 
            self.var_ids.iter().map(
                |(k, v)| if k.1 != Scope::Global {Some(v.clone())} else {None}
            ).collect();
        for id in ids_to_remove {
            if let Some(ref id) = id {
                self.vars.remove(id);
            }
        }
        self.var_ids.retain(|k,_| k.1 == Scope::Global);
        self.bs_to_si.clear();
    }

    fn set_var(&mut self, name: &str, value: Rc<RzIL>) -> RzILResult<()> {
        if let Code::Pure(PureCode::Var(scope, id)) = value.code {
            match scope {
                Scope::Global | Scope::Local => (),
                Scope::LocalPure => {
                    if self.var_ids.contains_key(&(name.to_owned(), scope)) {
                        return Err(RzILError::ImmutableVariable(name.to_owned()));
                    }
                }
            }
            self.var_ids.insert((name.to_owned(), scope), id);
            self.vars.insert(id, value);
            Ok(())
        } else {
            Err(RzILError::UnexpectedCode(value.code.clone()))
        }
    }

    fn get_var(&self, name: &str, scope: Scope) -> Option<Rc<RzIL>> {
        if let Some(id) = self.var_ids.get(&(name.to_owned(), scope)) {
            if let Some(var) = self.vars.get(id) {
                return Some(var.clone())
            }
        }
        None
    }

    fn remove_var(&mut self, name: &str, scope: Scope) -> Option<Rc<RzIL>> {
        if let Some(id) = self.var_ids.remove(&(name.to_owned(), scope)) {
            return self.vars.remove(&id);
        }
        None
    }

    pub fn bind_registers(&mut self, api: &mut RzApi) -> RzILResult<()> {
        let rzilvm_status = self.api_result(api.get_rzil_vm_status())?;
        for (key, val) in &rzilvm_status {
            let sort = match val {
                RzILVMRegValue::String(string) => {
                    if !string.starts_with("0x") {
                        return Err(RzILError::ParseStringToBv(string.clone()));
                    }
                    let size = (string.len() - 2) * 4;
                    Sort::Bitv(size)
                }
                RzILVMRegValue::Bool(_) => Sort::Bool,
                _ => {
                    continue;
                }
            };
            let scope = Scope::Global;
            let symbolized = true;
            let eval = 0;
            let id = self.get_uniq_var_id();
            let reg = self.new_pure(PureCode::Var(scope, id), vec![], symbolized, sort, eval);
            self.set_var(key, reg)?;
        }
        Ok(())
    }

    pub fn lift_inst(&mut self, api: &mut RzApi, addr: u64) -> RzILResult<Rc<RzIL>> {
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
    ) -> RzILResult<Vec<Rc<RzIL>>> {
        let insts = self.api_result(api.get_n_insts(Some(n), Some(addr)))?;
        let mut vec = Vec::new();
        for inst in insts {
            self.partial_clear();
            vec.push(self.create_effect(&inst.rzil)?);
        }
        Ok(vec)
    }

    fn api_result<T>(&self, result: RzResult<T>) -> RzILResult<T> {
        match result {
            Ok(res) => Ok(res),
            Err(e) => Err(RzILError::ApiError(e)),
        }
    }

    fn get_uniq_var_id(&self) -> u64 {
        self.uniq_var_id.set(self.uniq_var_id.get() + 1);
        self.uniq_var_id.get()
    }

    fn new_pure(&mut self, code: PureCode, args: Vec<Rc<RzIL>>, symbolized: bool, sort: Sort, eval: u64) -> Rc<RzIL> {
        let mut rzil = RzIL {
                code: Code::Pure(code), 
                args, 
                symbolized, 
                sort, 
                eval,
                label: None,
                hash: 0,
        };
        let mut hasher = DefaultHasher::new();
        rzil.hash(&mut hasher);
        rzil.hash = hasher.finish();
        if let Some(cached) = self.cache.get(&rzil) {
            cached.clone()
        } else {
            let rzil = Rc::new(rzil);
            self.cache.insert(rzil.clone());
            rzil
        }
    }

    fn new_effect(&mut self, code: EffectCode, args: Vec<Rc<RzIL>>, symbolized: bool, label: Option<String>) -> Rc<RzIL> {
        let mut rzil = RzIL {
                code: Code::Effect(code), 
                args, 
                symbolized, 
                sort: Sort::None, 
                eval: 0,
                label,
                hash: 0,
        };
        let mut hasher = DefaultHasher::new();
        rzil.hash(&mut hasher);
        rzil.hash = hasher.finish();
        if let Some(cached) = self.cache.get(&rzil) {
            cached.clone()
        } else {
            let rzil = Rc::new(rzil);
            self.cache.insert(rzil.clone());
            rzil
        }
    }

    pub fn new_ite(&mut self, condition: Rc<RzIL>, then: Rc<RzIL>, otherwise: Rc<RzIL>) -> Rc<RzIL> {
        // this function is assuming that the sort integrity between then and otherwise holds.
        // please make sure that before call this.
        let sort = then.get_sort();
        let eval = if condition.evaluate() != 0 {
            then.evaluate()
        } else {
            otherwise.evaluate()
        };
        let symbolized = condition.is_symbolized()
            || (condition.evaluate() != 0 && then.is_symbolized())
            || (condition.evaluate() == 0 && otherwise.is_symbolized());
        if !symbolized{
            match sort {
                Sort::Bool => 
                    self.new_pure(PureCode::Bool, vec![], symbolized, sort, eval),
                Sort::Bitv(_) => 
                    self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval),
                Sort::None => unreachable!(),
            }
        } else if condition.is_concretized() {
            if condition.evaluate_bool() {
                then
            } else {
                otherwise
            }
        } else {
            self.new_pure(PureCode::Ite, vec![condition, then, otherwise], symbolized, sort, eval)
        }
    }

    pub fn new_const(&mut self, sort: Sort, val: u64) -> Rc<RzIL> {
        match sort {
            Sort::Bitv(_) => self.new_pure(PureCode::Bitv, vec![], false, sort, val),
            Sort::Bool => self.new_pure(PureCode::Bool, vec![], false, sort, val),
            Sort::None => unreachable!(),
        }
    }

    pub fn new_add(&mut self, x: Rc<RzIL>, y: Rc<RzIL>) -> Rc<RzIL> {
        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bitv(x.get_size());
        let eval = if !symbolized { (x.evaluate() + y.evaluate()) & x.get_bitmask() } else { 0 };
        if !symbolized{
            self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval)
        } else if x.is_zero() {
            y
        } else if y.is_zero() {
            x
        } else {
            self.new_pure(PureCode::Add, vec![x, y], symbolized, sort, eval)
        }
    }

    pub fn new_cast(&mut self, value: Rc<RzIL>, fill_bit: Rc<RzIL>, length: usize) -> Rc<RzIL> {
        let symbolized = fill_bit.is_symbolized() | value.is_symbolized() | fill_bit.is_symbolized();
        let sort = Sort::Bitv(length);
        let eval = if !symbolized {
            if value.get_size() >= length {
                value.evaluate() & (1 << length - 1)
            } else {
                let fill_bits = (fill_bit.evaluate() << length - 1) & !value.get_bitmask();
                value.evaluate() | fill_bits
            }
        } else {
            0
        };
        if !symbolized {
            self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval)
        } else if value.get_sort() == sort {
            value
        } else {
            let expand = value.get_size() < sort.get_size();
            self.new_pure(PureCode::Cast(expand), vec![fill_bit, value], symbolized, sort, eval)
        }
    }

    fn pure_bv(&mut self, op: &RzILInfo) -> RzILResult<Rc<RzIL>> {
        let bitvector = self.pure(op)?;
        if !bitvector.is_bv() {
            Err(RzILError::UnexpectedSort(Sort::Bitv(0), bitvector.get_sort()))
        } else {
            Ok(bitvector)
        }
    }
    fn pure_bool(&mut self, op: &RzILInfo) -> RzILResult<Rc<RzIL>> {
        let boolean = self.pure(op)?;
        if !boolean.is_bool() {
            Err(RzILError::UnexpectedSort(Sort::Bool, boolean.get_sort()))
        } else {
            Ok(boolean)
        }
    }
    fn pure(&mut self, op: &RzILInfo) -> RzILResult<Rc<RzIL>> {
        match op {
            RzILInfo::Var { value } => 
                if let Some(var) = self.get_var(value, Scope::Global) {
                    Ok(var.clone())
                }else if let Some(var) = self.get_var(value, Scope::Local) {
                    if !var.is_symbolized() {
                        let sort = var.get_sort();
                        Ok(match sort {
                            Sort::Bool =>
                                self.new_pure(PureCode::Bool, vec![], false, sort, var.evaluate()),
                            Sort::Bitv(_) =>
                                self.new_pure(PureCode::Bitv, vec![], false, sort, var.evaluate()),
                            Sort::None => unreachable!()
                        })
                    } else {
                        Ok(var.clone())
                    }
                }else if let Some(var) = self.get_var(value, Scope::LocalPure) {
                    Ok(var.args[0].clone())
                }else{
                    Err(RzILError::UndefinedVariableReferenced(value.clone()))
                }
            RzILInfo::Ite { condition, x, y } => {
                let condition = self.pure_bool(condition)?;
                let x = self.pure(x)?;
                let y = self.pure(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                Ok(self.new_ite(condition, x, y))
            }
            RzILInfo::Let { dst, exp, body } => {
                // currently we can't use let-expressions. 
                // so exp is directly assigned to body.
                let id = self.get_uniq_var_id();
                let exp = self.pure(exp)?;
                let symbolized = exp.is_symbolized();
                let sort = exp.get_sort();
                let eval = exp.evaluate();
                let exp = self.new_pure(
                    PureCode::Var(Scope::LocalPure, id), 
                    vec![exp], 
                    symbolized, sort, eval);
                self.set_var(dst, exp.clone())?;
                let body = self.pure(body)?;
                self.remove_var(dst, Scope::LocalPure);
                Ok(body)
            }
            RzILInfo::Bool { value } => {
                let symbolized = false;
                let sort = Sort::Bool;
                let eval = if value.clone() { 1 } else { 0 };
                Ok(self.new_pure(PureCode::Bool, vec![], symbolized, sort, eval))
            }
            RzILInfo::BoolInv { x } => {
                let x = self.pure_bool(x)?;
                let symbolized = x.is_symbolized();
                let sort = Sort::Bool;
                let eval = !(x.evaluate() != 0) as u64;
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bool, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::BoolInv, vec![x], symbolized, sort, eval))
                }
            }
            RzILInfo::BoolAnd { x, y } => {
                let x = self.pure_bool(x)?;
                let y = self.pure_bool(y)?;
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bool;
                let eval = x.evaluate() & y.evaluate();
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bool, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::BoolAnd, vec![x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::BoolOr { x, y } => {
                let x = self.pure_bool(x)?;
                let y = self.pure_bool(y)?;
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bool;
                let eval = x.evaluate() | y.evaluate();
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bool, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::BoolOr, vec![x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::BoolXor { x, y } => {
                let x = self.pure_bool(x)?;
                let y = self.pure_bool(y)?;
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bool;
                let eval = x.evaluate() ^ y.evaluate();
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bool, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::BoolXor, vec![x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::Bitv { bits, len } => {
                let symbolized = false;
                let sort = Sort::Bitv(len.clone());
                let eval = u64::from_str_radix(&bits[2..], 16)?;
                Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
            }
            RzILInfo::Msb { bv } => {
                let bv = self.pure_bv(bv)?;
                let symbolized = bv.is_symbolized();
                let sort = Sort::Bool;
                let eval = (bv.evaluate() >> (bv.get_size() - 1)) & 0x1;
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bool, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::Msb, vec![bv], symbolized, sort, eval))
                }
            }
            RzILInfo::Lsb { bv } => {
                let bv = self.pure_bv(bv)?;
                let symbolized = bv.is_symbolized();
                let sort = Sort::Bool;
                let eval = bv.evaluate() & 0x1;
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bool, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::Lsb, vec![bv], symbolized, sort, eval))
                }
            }
            RzILInfo::IsZero { bv } => {
                let bv = self.pure_bv(bv)?;
                let symbolized = bv.is_symbolized();
                let sort = Sort::Bool;
                let eval = if bv.is_zero() { 1 } else { 0 };
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::IsZero, vec![bv], symbolized, sort, eval))
                }
            }
            RzILInfo::Neg { bv } => {
                let bv = self.pure_bv(bv)?;
                let symbolized = bv.is_symbolized();
                let sort = Sort::Bitv(bv.get_size());
                let eval = (bv.evaluate() as i64 * -1i64) as u64;
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::Neg, vec![bv], symbolized, sort, eval))
                }
            }
            RzILInfo::LogNot { bv } => {
                let bv = self.pure_bv(bv)?;
                let symbolized = bv.is_symbolized();
                let sort = Sort::Bitv(bv.get_size());
                let eval = !bv.evaluate();
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::LogNot, vec![bv], symbolized, sort, eval))
                }
            }

            RzILInfo::Add { x, y } => {
                let x = self.pure_bv(x)?;
                let y = self.pure_bv(y)?;
                Ok(self.new_add(x, y))
            }

            RzILInfo::Sub { x, y } => {
                let x = self.pure_bv(x)?;
                let y = self.pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bitv(x.get_size());
                let eval = if !symbolized { (x.evaluate() - y.evaluate()) & x.get_bitmask() } else { 0 };
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else if x.is_zero() {
                    Ok(y)
                } else if y.is_zero() {
                    Ok(x)
                } else {
                    Ok(self.new_pure(PureCode::Sub, vec![x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::Mul { x, y } => {
                let x = self.pure_bv(x)?;
                let y = self.pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bitv(x.get_size());
                let eval = if !symbolized { (x.evaluate() * y.evaluate()) & x.get_bitmask() } else { 0 };
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::Mul, vec![x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::Div { x, y } => {
                let x = self.pure_bv(x)?;
                let y = self.pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bitv(x.get_size());
                let eval = if !symbolized && y.evaluate() != 0 {
                    (x.evaluate() / y.evaluate()) & x.get_bitmask() 
                } else { 
                    0
                };

                if !symbolized{
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::Div, vec![x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::Sdiv { x, y } => {
                let x = self.pure_bv(x)?;
                let y = self.pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bitv(x.get_size());
                let eval = if !symbolized && y.evaluate() != 0 {
                    ((x.evaluate() as i64 / y.evaluate() as i64) as u64) & x.get_bitmask()
                } else {
                    0
                };
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::Sdiv, vec![x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::Mod { x, y } => {
                let x = self.pure_bv(x)?;
                let y = self.pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bitv(x.get_size());
                let eval = if !symbolized && y.evaluate() != 0 {
                    (x.evaluate() % y.evaluate()) & x.get_bitmask()
                } else {
                    0
                };
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::Mod, vec![x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::Smod { x, y } => {
                let x = self.pure_bv(x)?;
                let y = self.pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bitv(x.get_size());
                let eval = if !symbolized && y.evaluate() != 0 {
                    ((x.evaluate() as i64 % y.evaluate() as i64) as u64) & x.get_bitmask()
                } else {
                    0
                };

                if !symbolized{
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::Smod, vec![x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::LogAnd { x, y } => {
                let x = self.pure_bv(x)?;
                let y = self.pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let sort = Sort::Bitv(x.get_size());
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let eval = if !symbolized {
                    x.evaluate() & y.evaluate()
                } else {
                    0
                };
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else if x.is_zero() || y.is_zero() {
                    // Bitv(0)
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::LogAnd, vec![x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::LogOr { x, y } => {
                let x = self.pure_bv(x)?;
                let y = self.pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bitv(x.get_size());
                let eval = if !symbolized {
                    x.evaluate() | y.evaluate()
                } else {
                    0
                };
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else if x.is_zero() {
                    Ok(y)
                } else if y.is_zero() {
                    Ok(x)
                } else {
                    Ok(self.new_pure(PureCode::LogOr, vec![x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::LogXor { x, y } => {
                let x = self.pure_bv(x)?;
                let y = self.pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bitv(x.get_size());
                let eval = if !symbolized {
                    x.evaluate() ^ y.evaluate()
                } else {
                    0
                };
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else if x.is_zero() {
                    Ok(y)
                } else if y.is_zero() {
                    Ok(x)
                } else if x.hash == y.hash {
                    // xor eax, eax
                    Ok(self.new_pure(PureCode::Bitv, vec![], false, sort, 0))
                } else {
                    Ok(self.new_pure(PureCode::LogXor, vec![x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::ShiftRight { fill_bit, x, y } => {
                let fill_bit = self.pure_bool(fill_bit)?;
                // convert fill_bit's sort from Bool to Bv(1)
                let fill_bit = if fill_bit.is_concretized() {
                    self.new_const(Sort::Bitv(1), fill_bit.evaluate())
                } else {
                    let one = self.new_const(Sort::Bitv(1), 1);
                    let zero = self.new_const(Sort::Bitv(1), 0);
                    self.new_ite(fill_bit, one, zero)
                };

                let x = self.pure_bv(x)?;
                let y = self.pure_bv(y)?;
                let symbolized = fill_bit.is_symbolized() | x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bitv(x.get_size());
                let eval = if !symbolized {
                    ((x.evaluate() >> y.evaluate())
                        | if fill_bit.evaluate() != 0 {
                            u64::MAX & !(x.get_bitmask() >> y.evaluate())
                        } else {
                            0
                        })
                        & x.get_bitmask()
                } else {
                    0
                };
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::ShiftRight, vec![fill_bit, x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::ShiftLeft { fill_bit, x, y } => {
                let fill_bit = self.pure_bool(fill_bit)?;
                // convert fill_bit's sort from Bool to Bv(1)
                let fill_bit = if fill_bit.is_concretized() {
                    self.new_const(Sort::Bitv(1), fill_bit.evaluate())
                } else {
                    let one = self.new_const(Sort::Bitv(1), 1);
                    let zero = self.new_const(Sort::Bitv(1), 0);
                    self.new_ite(fill_bit, one, zero)
                };

                let x = self.pure_bv(x)?;
                let y = self.pure_bv(y)?;
                let symbolized = fill_bit.is_symbolized() | x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bitv(x.get_size());
                let eval = if !symbolized {
                    ((x.evaluate() << y.evaluate())
                        | if fill_bit.evaluate() != 0 {
                            (1 << y.evaluate()) - 1
                        } else {
                            0
                        })
                        & x.get_bitmask()
                } else {
                    0
                };
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::ShiftLeft, vec![fill_bit, x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::Equal { x, y } => {
                let x = self.pure_bv(x)?;
                let y = self.pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bool;
                let eval = if !symbolized {
                    if x.evaluate() == y.evaluate() {
                        1
                    } else {
                        0
                    }
                } else {
                    0
                };
                Ok(self.new_pure(PureCode::Equal, vec![x, y], symbolized, sort, eval))
            }
            RzILInfo::Sle { x, y } => {
                let x = self.pure_bv(x)?;
                let y = self.pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bool;
                let eval = if !symbolized && x.evaluate() as i64 >= y.evaluate() as i64 {
                    1
                } else {
                    0
                };
                if !symbolized{
                    Ok(self.new_pure(PureCode::Bool, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::Sle, vec![x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::Ule { x, y } => {
                let x = self.pure_bv(x)?;
                let y = self.pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                let symbolized = x.is_symbolized() | y.is_symbolized();
                let sort = Sort::Bool;
                let eval = if !symbolized && x.evaluate() >= y.evaluate() {
                    1
                } else {
                    0
                };

                if !symbolized{
                    Ok(self.new_pure(PureCode::Bool, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::Ule, vec![x, y], symbolized, sort, eval))
                }
            }
            RzILInfo::Cast {
                value,
                length,
                fill,
            } => {
                let fill_bit = self.pure_bool(fill)?;
                // convert fill's sort from Bool to Bv(1)
                let fill_bit = if fill_bit.is_concretized() {
                    self.new_const(Sort::Bitv(1), fill_bit.evaluate())
                } else {
                    let one = self.new_const(Sort::Bitv(1), 1);
                    let zero = self.new_const(Sort::Bitv(1), 0);
                    self.new_ite(fill_bit, one, zero)
                };

                let value = self.pure_bv(value)?;
                let length = length.clone();
                Ok(self.new_cast(value, fill_bit, length))
            }
            RzILInfo::Append { high, low } => {
                let high = self.pure_bv(high)?;
                let low = self.pure_bv(low)?;
                let symbolized = high.is_symbolized() | low.is_symbolized();
                let sort = Sort::Bitv(high.get_size() + low.get_size());
                let eval = if !symbolized {
                    (high.evaluate() << low.get_size()) | low.evaluate()
                } else {
                    0
                };
                if !symbolized {
                    Ok(self.new_pure(PureCode::Bitv, vec![], symbolized, sort, eval))
                } else {
                    Ok(self.new_pure(PureCode::Append, vec![high, low], symbolized, sort, eval))
                }
            }
            RzILInfo::Load { mem: _, key } => {
                let key = self.pure_bv(key)?;
                let symbolized = true;
                let sort = Sort::Bitv(8);
                let eval = 0;
                Ok(self.new_pure(PureCode::Load, vec![key], symbolized, sort, eval))
            }
            RzILInfo::Loadw { mem: _, key, bits } => {
                let key = self.pure_bv(key)?;
                let symbolized = true;
                let sort = Sort::Bitv(bits.clone() as usize);
                let eval = 0;
                Ok(self.new_pure(PureCode::Load, vec![key], symbolized, sort, eval))
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

    fn create_effect(&mut self, op: &RzILInfo) -> RzILResult<Rc<RzIL>> {
        match op {
            RzILInfo::Nop => Ok(self.create_nop()),
            RzILInfo::Set { dst, src } => {
                let name = dst;
                let src = self.pure(src)?;
                let dst = match self.get_var(name, Scope::Global) {
                    Some(var) => {
                        if src.get_sort() != var.get_sort() {
                            return Err(RzILError::SortIntegrity(src.get_sort(), var.get_sort()));
                        }
                        self.fresh_var(
                            Scope::Global,
                            &src,
                        )
                    }
                    None => {
                        let var = self.fresh_var(Scope::Local,&src);
                        if src.is_concretized() {
                            self.set_var(name, var)?;
                            return Err(RzILError::EmptyNode);
                        }
                        var
                    }
                };
                if self.in_branch() && self.option.contains(RzILContextConfig::ConvertBranchSetToSetIte) {
                    self.branch_set_to_set_ite_add_entry(name,src,dst)?;
                    return Err(RzILError::EmptyNode);
                }
                let symbolized = dst.is_symbolized();
                let label = None;
                self.set_var(name, dst.clone())?;
                Ok(self.new_effect(EffectCode::Set, vec![dst, src], symbolized, label))
            }

            RzILInfo::Jmp { dst } => {
                let dst = self.pure_bv(dst)?;
                let symbolized = dst.is_symbolized();
                Ok(self.new_effect(EffectCode::Jmp, vec![dst], symbolized, None))
            }

            RzILInfo::Goto { label } => {
                let symbolized = false;
                Ok(self.new_effect(EffectCode::Goto, vec![], symbolized, Some(label.clone())))
            }
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
                Ok(self.new_effect(EffectCode::Seq, args, symbolized, None))
            }
            RzILInfo::Branch {
                condition,
                true_eff,
                false_eff,
            } => {
                let condition = self.pure_bool(condition)?;

                self.bs_to_si.conditions.push(condition.clone());

                self.bs_to_si.taken.push(true);
                let then = self.create_effect(true_eff)?;
                self.bs_to_si.taken.pop();

                self.bs_to_si.taken.push(false);
                let otherwise = self.create_effect(false_eff)?;
                self.bs_to_si.taken.pop();

                self.bs_to_si.conditions.pop();

                let mut args = Vec::new();
                if !self.in_branch() {
                    let entries: Vec<BranchSetToSetIteEntry> 
                        = self.bs_to_si.entries.drain(..).collect();
                    for entry in entries {
                        let dst = entry.dst.clone();
                        let condition = entry.condition.clone();
                        let x = entry.then.clone().unwrap_or(entry.default.clone());
                        let y = entry.otherwise.clone().unwrap_or(entry.default.clone());
                        if x.get_sort() != y.get_sort() {
                            return Err(RzILError::SortIntegrity(
                                x.get_sort(),
                                y.get_sort(),
                            ));
                        }
                        let ite = self.new_ite(condition, x, y);
                        let symbolized = ite.is_symbolized();
                        args.push(self.new_effect(EffectCode::Set, vec![dst, ite], symbolized, None));
                    }
                    self.bs_to_si.clear();
                }
                let need_branch =
                    !then.is_nop() || !otherwise.is_nop();
                let symbolized = condition.is_symbolized();
                if need_branch {
                    args.push(self.new_effect(EffectCode::Branch, vec![condition, then, otherwise], symbolized, None));
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
                    Ok(self.new_effect(EffectCode::Seq, args, symbolized, None))
                }
            }
            RzILInfo::Store { mem: _, key, value } => {
                let key = self.pure_bv(key)?;
                let value = self.pure_bv(value)?;
                let symbolized = key.is_symbolized() || value.is_symbolized();
                Ok(self.new_effect(EffectCode::Store, vec![key, value], symbolized, None))
            }
            RzILInfo::Storew { mem: _, key, value } => {
                let key = self.pure_bv(key)?;
                let value = self.pure_bv(value)?;
                let symbolized = key.is_symbolized() || value.is_symbolized();
                Ok(self.new_effect(EffectCode::StoreW, vec![key, value], symbolized, None))
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

    fn create_nop(&mut self) -> Rc<RzIL> {
        self.new_effect(EffectCode::Nop, vec![], false, None)
    }

    fn create_effect_seq(&mut self, op: &RzILInfo, vec: &mut Vec<Rc<RzIL>>) -> RzILResult<()> {
        match op {
            RzILInfo::Seq { x, y } => {
                self.create_effect_seq(x, vec)?;
                self.create_effect_seq(y, vec)?;
            }
            _ => {
                match self.create_effect(op) {
                    Ok(ret) => {
                        if Code::Effect(EffectCode::Seq) == ret.code {
                            for e in &ret.args {
                                vec.push(e.clone())
                            }
                        }else{
                            vec.push(ret)
                        }
                    },
                    Err(RzILError::EmptyNode) => (),
                    Err(err) => return Err(err),
                };
            }
        };
        Ok(())
    }

    fn fresh_var(
        &mut self,
        scope: Scope,
        value: &Rc<RzIL>,
    ) -> Rc<RzIL> {
        let symbolized = value.is_symbolized();
        let sort = value.get_sort();
        let eval = value.evaluate();
        let id = self.get_uniq_var_id();
        self.new_pure(PureCode::Var(scope, id), vec![], symbolized, sort, eval)
    }

    fn in_branch(&self) -> bool {
        !self.bs_to_si.conditions.is_empty() && !self.bs_to_si.taken.is_empty()
    }

    fn branch_set_to_set_ite_connect_condition(&mut self) -> RzILResult<Rc<RzIL>> {
        let len = self.bs_to_si.conditions.len();
        let mut x = self.bs_to_si.conditions.last().unwrap().clone();
        for i in (0..len-1).rev() {
            if !x.is_bool() {
                return Err(RzILError::UnexpectedSort(Sort::Bool, x.get_sort()));
            }
            if i < len-2 && !self.bs_to_si.taken[i+1] {
                let symbolized = x.is_symbolized();
                let sort = Sort::Bool;
                let eval = !(x.evaluate() != 0) as u64;
                x = self.new_pure(PureCode::BoolInv, vec![], symbolized, sort, eval)
            }
            let y = self.bs_to_si.conditions[i].clone();
            let symbolized = x.is_symbolized() | y.is_symbolized();
            let sort = Sort::Bool;
            let eval = x.evaluate() & y.evaluate();
            x = self.new_pure(PureCode::BoolAnd, vec![x, y], symbolized, sort, eval)
        }
        Ok(x)
    }

    fn branch_set_to_set_ite_add_entry(&mut self, name: &str, src: Rc<RzIL>, dst: Rc<RzIL> ) -> RzILResult<()> {
        let condition =
            self.branch_set_to_set_ite_connect_condition()?;
        let taken = self.bs_to_si.taken.first().unwrap().clone();
        let mut entry = None;
        for e in self.bs_to_si.entries.iter_mut() {
            if e.name.eq(name) {
                entry = Some(e);
            }
        }
        match entry {
            Some(e) => {
                if taken {
                    if e.then.is_some() {
                        return Err(RzILError::BranchSetToSetIteFailed);
                    }
                    e.then = Some(src);
                } else {
                    if e.otherwise.is_some() {
                        return Err(RzILError::BranchSetToSetIteFailed);
                    }
                    e.otherwise = Some(src);
                }
            }
            None=>{
                let default = match self.get_var(name, Scope::Global) {
                    Some(var) => var.clone(),
                    None => {
                        let code = match dst.get_sort() {
                            Sort::Bitv(_) => PureCode::Bitv,
                            Sort::Bool => PureCode::Bool,
                            Sort::None => return Err(RzILError::UnkownRzIL)
                        };
                        let symbolized = false;
                        let sort = dst.get_sort();
                        let eval = 0;
                        self.new_pure(code, vec![], symbolized, sort, eval)
                    }
                };
                let (mut then, mut otherwise) = (None, None);
                if taken {
                    then = Some(src);
                } else {
                    otherwise = Some(src);
                }
                self.bs_to_si.entries.push(
                    BranchSetToSetIteEntry {
                        name:name.to_owned(),
                        dst: dst.clone(),
                        condition,
                        then,
                        otherwise,
                        default,
                    },
                );
            }
        };
        self.set_var(name, dst)?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
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
