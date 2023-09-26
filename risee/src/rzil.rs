use bitflags::bitflags;
use rzapi::api::RzApi;
use rzapi::api::RzResult;
use rzapi::structs::{RzILInfo,RzILVMRegValue};
use std::cell::Cell;
use std::collections::hash_map::{HashMap,DefaultHasher};
use quick_cache::sync::Cache;
use std::fmt::Display;
use std::hash::Hasher;
use std::rc::Rc;
use std::vec;
use std::hash::Hash;
use thiserror::Error;
pub mod to_z3;
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
    UnexpectedPureCode(PureCode),

    #[error("Parse Int failed")]
    ParseInt(#[from] std::num::ParseIntError),

    #[error("String: {0} was not hex-decimal")]
    ParseStringToBv(String),

    #[error("RzApi failed: {0}")]
    ApiError(String),

    #[error("Pure {0} is Unimplemented")]
    UnimplementedRzILPure(PureCode),

    #[error("Effect {0} is Unimplemented")]
    UnimplementedRzILEffect(String),

    #[error("Unkown (Unimplemented or Invalid) Pure detected.")]
    UnkownPure,

    #[error("Empty detected. Unable to continue.")]
    Empty,

    #[error("Invalid access to Effect Args")]
    InvalidAccessToArgs,

    #[error("Variable {0} is immutable but changed")]
    ImmutableVariable(String),

    #[error("Unable to convert Set in Branch to Set with Ite")]
    BranchSetToSetIteFailed,

    #[error("Unhandled Error : Pure Node no need or postponed to evaluate")]
    EmptyNode,
}

#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub enum Sort {
    Bitv(usize),
    Bool,
}
impl Sort {
    fn is_bv(&self) -> bool {
        match self {
            Sort::Bitv(_) => true,
            Sort::Bool => false,
        }
    }
    fn is_bool(&self) -> bool {
        match self {
            Sort::Bitv(_) => false,
            Sort::Bool => true,
        }
    }
    fn get_size(&self) -> usize {
        match self {
            Sort::Bitv(len) => len.clone(),
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

#[derive(Clone, Debug)]
pub enum Effect {
    Nop,
    Set{dst: Rc<Pure>, src: Rc<Pure>},
    Jmp{dst: Rc<Pure>},
    Goto{label: String},
    Seq{args: Vec<Rc<Effect>>},
    Blk,
    Repeat,
    Branch{condition: Rc<Pure>, then: Rc<Effect>, otherwise: Rc<Effect>},
    Store{key: Rc<Pure>, value: Rc<Pure>},
    StoreW{key: Rc<Pure>, value: Rc<Pure>},
    Empty,
}

impl Display for Effect {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let code = match self {
            Effect::Nop => "Nop".to_string(),
            Effect::Set{ .. } => "Set".to_string(),
            Effect::Jmp{ .. } => "Jmp".to_string(),
            Effect::Goto{ .. } => "Goto".to_string(),
            Effect::Seq{ .. } => "Seq".to_string(),
            Effect::Blk => "Blk".to_string(),
            Effect::Repeat => "Repeat".to_string(),
            Effect::Branch{ .. } => "Branch".to_string(),
            Effect::Store{ .. } => "Store".to_string(),
            Effect::StoreW{ .. } => "StoreW".to_string(),
            Effect::Empty => "Empty".to_string(),
        };
        write!(f, "{}", code)
    }
}

impl Display for PureCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Pure {
    code: PureCode,
    symbolized: bool,
    sort: Sort,
    eval: u64,              // if symbolized, it has 0.
    args: Vec<Rc<Pure>>,
    hash: u64,              // unique for the semantics
}

impl Hash for Pure {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.code.hash(state);
        for arg in &self.args {
            arg.hash.hash(state);
        }
        self.symbolized.hash(state);
        self.sort.hash(state);
        self.eval.hash(state);
    }
}

impl Pure {
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
    pub fn get_bitmask(&self) -> u64 {
        u64::MAX >> (u64::BITS as usize - self.get_size())
    }

    pub fn is_bv(&self) -> bool {
        self.get_sort().is_bv()
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
        self.is_bv() && self.is_concretized() && self.evaluate() == 0
    }
    pub fn concretize(&mut self, eval: u64) {
        self.symbolized = false;
        self.eval = eval;
    }
}

impl Effect {
    fn is_nop(&self) -> bool {
        if let Effect::Nop = self {
            true
        } else {
            false
        }
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
    dst: Rc<Pure>,
    condition: Rc<Pure>,
    then: Option<Rc<Pure>>,
    otherwise: Option<Rc<Pure>>,
    default: Rc<Pure>,
}

#[derive(Debug)]
struct BranchSetToSetIte {
    conditions: Vec<Rc<Pure>>,
    taken: Vec<bool>,
    entries: Vec<BranchSetToSetIteEntry>,
}

pub struct RzILContext {
    /*
     * This struct and its impl mainly has four objectives.
     * 1. Parse RzILInfo from RzApi
     * 2. Add supplemental information.
     *      (e.g. type information of every node, symbolized or not)
     * 3. Convert the syntax desired for symbolic execution.
     *      (e.g. Branch and Set -> Set and Ite)
     * 4. Provide Pure struct generation API for other modules.
     * */
    option: RzILContextConfig,
    var_ids: HashMap<(String,Scope), u64>,
    vars: HashMap<u64, Rc<Pure>>,
    cache: Cache<Rc<Pure>, Rc<Pure>>,
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
            cache: Cache::new(10000),
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

    fn set_var(&mut self, name: &str, var: Rc<Pure>) -> RzILResult<()> {
        if let PureCode::Var(scope, id) = var.code {
            match scope {
                Scope::Global | Scope::Local => (),
                Scope::LocalPure => {
                    if self.var_ids.contains_key(&(name.to_owned(), scope)) {
                        return Err(RzILError::ImmutableVariable(name.to_owned()));
                    }
                }
            }
            self.var_ids.insert((name.to_owned(), scope), id);
            self.vars.insert(id, var);
            Ok(())
        } else {
            Err(RzILError::UnexpectedPureCode(var.code.clone()))
        }
    }

    fn get_var(&self, name: &str, scope: Scope) -> Option<Rc<Pure>> {
        if let Some(id) = self.var_ids.get(&(name.to_owned(), scope)) {
            if let Some(var) = self.vars.get(id) {
                return Some(var.clone())
            }
        }
        None
    }

    fn remove_var(&mut self, name: &str, scope: Scope) -> Option<Rc<Pure>> {
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

    pub fn lift_inst(&mut self, api: &mut RzApi, addr: u64) -> RzILResult<Rc<Effect>> {
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
    ) -> RzILResult<Vec<Rc<Effect>>> {
        let insts = self.api_result(api.get_n_insts(Some(n), Some(addr)))?;
        let mut vec = Vec::new();
        for inst in insts {
            self.partial_clear();
            vec.push(self.parse_effect(&inst.rzil)?);
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

    fn new_pure(&self, code: PureCode, args: Vec<Rc<Pure>>, symbolized: bool, sort: Sort, eval: u64) -> Rc<Pure> {
        let mut op = Pure {
                code, 
                args, 
                symbolized, 
                sort, 
                eval,
                hash: 0,
        };
        let mut hasher = DefaultHasher::new();
        op.hash(&mut hasher);
        op.hash = hasher.finish();
        if let Some(cached) = self.cache.get(&op) {
            cached.clone()
        } else {
            let op = Rc::new(op);
            self.cache.insert(op.clone(), op.clone());
            op
        }
    }

    fn new_effect(&self, effect: Effect) -> Rc<Effect> {
        Rc::new(effect)
    }

    pub fn new_ite(&self, condition: Rc<Pure>, then: Rc<Pure>, otherwise: Rc<Pure>) -> Rc<Pure> {
        // this function is assuming the sort integrity between then and otherwise.
        // please make sure it before call this.
        assert!(then.get_sort() == otherwise.get_sort());
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

    pub fn new_const(&self, sort: Sort, val: u64) -> Rc<Pure> {
        match sort {
            Sort::Bitv(_) => self.new_pure(PureCode::Bitv, vec![], false, sort, val),
            Sort::Bool => self.new_pure(PureCode::Bool, vec![], false, sort, val),
        }
    }

    pub fn new_eq(&self, x: Rc<Pure>, y: Rc<Pure>) -> Rc<Pure> {
        // this function is assuming the sort integrity between x and y.
        // please make sure it before call this.
        assert!(x.get_sort() == y.get_sort());
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
        self.new_pure(PureCode::Equal, vec![x, y], symbolized, sort, eval)
    }

    pub fn new_boolinv(&self, x: Rc<Pure>) -> Rc<Pure> {
        // this function is assuming that the sort of x is Bool.
        // please make sure it before call this.
        assert!(x.is_bool());
        let symbolized = x.is_symbolized();
        let sort = Sort::Bool;
        let eval = !(x.evaluate() != 0) as u64;
        if !symbolized{
            self.new_pure(PureCode::Bool, vec![], symbolized, sort, eval)
        } else {
            self.new_pure(PureCode::BoolInv, vec![x], symbolized, sort, eval)
        }
    }

    pub fn new_booland(&self, x: Rc<Pure>, y: Rc<Pure>) -> Rc<Pure> {
        // this function is assuming that the sort of x and y are Bool.
        // please make sure it before call this.
        assert!(x.is_bool() && y.is_bool());
        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bool;
        let eval = x.evaluate() & y.evaluate();
        if !symbolized{
            self.new_pure(PureCode::Bool, vec![], symbolized, sort, eval)
        } else {
            self.new_pure(PureCode::BoolAnd, vec![x, y], symbolized, sort, eval)
        }
    }

    pub fn new_bvadd(&self, x: Rc<Pure>, y: Rc<Pure>) -> Rc<Pure> {
        // this function is assuming that the sort of x and y are Bitv.
        // please make sure it before call this.
        assert!(x.is_bv() && y.is_bv());
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

    pub fn new_cast(&self, value: Rc<Pure>, fill_bit: Rc<Pure>, length: usize) -> Rc<Pure> {
        // this function is assuming that the sort of value is Bitv, and that of fill_bit is Bool.
        // please make sure it before call this.
        assert!(value.is_bv() && fill_bit.is_bool());
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

    fn new_nop(&self) -> Rc<Effect> {
        self.new_effect(Effect::Nop)
    }

    fn parse_pure_bv(&mut self, op: &RzILInfo) -> RzILResult<Rc<Pure>> {
        let bitvector = self.parse_pure(op)?;
        if !bitvector.is_bv() {
            Err(RzILError::UnexpectedSort(Sort::Bitv(0), bitvector.get_sort()))
        } else {
            Ok(bitvector)
        }
    }
    fn parse_pure_bool(&mut self, op: &RzILInfo) -> RzILResult<Rc<Pure>> {
        let boolean = self.parse_pure(op)?;
        if !boolean.is_bool() {
            Err(RzILError::UnexpectedSort(Sort::Bool, boolean.get_sort()))
        } else {
            Ok(boolean)
        }
    }
    fn parse_pure(&mut self, op: &RzILInfo) -> RzILResult<Rc<Pure>> {
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
                let condition = self.parse_pure_bool(condition)?;
                let x = self.parse_pure(x)?;
                let y = self.parse_pure(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                Ok(self.new_ite(condition, x, y))
            }
            RzILInfo::Let { dst, exp, body } => {
                // currently we can't use let-expressions. 
                // so exp is directly assigned to body.
                let id = self.get_uniq_var_id();
                let exp = self.parse_pure(exp)?;
                let symbolized = exp.is_symbolized();
                let sort = exp.get_sort();
                let eval = exp.evaluate();
                let exp = self.new_pure(
                    PureCode::Var(Scope::LocalPure, id), 
                    vec![exp], 
                    symbolized, sort, eval);
                self.set_var(dst, exp.clone())?;
                let body = self.parse_pure(body)?;
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
                let x = self.parse_pure_bool(x)?;
                Ok(self.new_boolinv(x))
            }
            RzILInfo::BoolAnd { x, y } => {
                let x = self.parse_pure_bool(x)?;
                let y = self.parse_pure_bool(y)?;
                Ok(self.new_booland(x, y))
            }
            RzILInfo::BoolOr { x, y } => {
                let x = self.parse_pure_bool(x)?;
                let y = self.parse_pure_bool(y)?;
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
                let x = self.parse_pure_bool(x)?;
                let y = self.parse_pure_bool(y)?;
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
                let bv = self.parse_pure_bv(bv)?;
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
                let bv = self.parse_pure_bv(bv)?;
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
                let bv = self.parse_pure_bv(bv)?;
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
                let bv = self.parse_pure_bv(bv)?;
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
                let bv = self.parse_pure_bv(bv)?;
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
                let x = self.parse_pure_bv(x)?;
                let y = self.parse_pure_bv(y)?;
                Ok(self.new_bvadd(x, y))
            }

            RzILInfo::Sub { x, y } => {
                let x = self.parse_pure_bv(x)?;
                let y = self.parse_pure_bv(y)?;
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
                let x = self.parse_pure_bv(x)?;
                let y = self.parse_pure_bv(y)?;
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
                let x = self.parse_pure_bv(x)?;
                let y = self.parse_pure_bv(y)?;
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
                let x = self.parse_pure_bv(x)?;
                let y = self.parse_pure_bv(y)?;
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
                let x = self.parse_pure_bv(x)?;
                let y = self.parse_pure_bv(y)?;
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
                let x = self.parse_pure_bv(x)?;
                let y = self.parse_pure_bv(y)?;
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
                let x = self.parse_pure_bv(x)?;
                let y = self.parse_pure_bv(y)?;
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
                let x = self.parse_pure_bv(x)?;
                let y = self.parse_pure_bv(y)?;
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
                let x = self.parse_pure_bv(x)?;
                let y = self.parse_pure_bv(y)?;
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
                let fill_bit = self.parse_pure_bool(fill_bit)?;
                // convert fill_bit's sort from Bool to Bv(1)
                let fill_bit = if fill_bit.is_concretized() {
                    self.new_const(Sort::Bitv(1), fill_bit.evaluate())
                } else {
                    let one = self.new_const(Sort::Bitv(1), 1);
                    let zero = self.new_const(Sort::Bitv(1), 0);
                    self.new_ite(fill_bit, one, zero)
                };

                let x = self.parse_pure_bv(x)?;
                let y = self.parse_pure_bv(y)?;
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
                let fill_bit = self.parse_pure_bool(fill_bit)?;
                // convert fill_bit's sort from Bool to Bv(1)
                let fill_bit = if fill_bit.is_concretized() {
                    self.new_const(Sort::Bitv(1), fill_bit.evaluate())
                } else {
                    let one = self.new_const(Sort::Bitv(1), 1);
                    let zero = self.new_const(Sort::Bitv(1), 0);
                    self.new_ite(fill_bit, one, zero)
                };

                let x = self.parse_pure_bv(x)?;
                let y = self.parse_pure_bv(y)?;
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
                let x = self.parse_pure_bv(x)?;
                let y = self.parse_pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
                Ok(self.new_eq(x, y))
            }
            RzILInfo::Sle { x, y } => {
                let x = self.parse_pure_bv(x)?;
                let y = self.parse_pure_bv(y)?;
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
                let x = self.parse_pure_bv(x)?;
                let y = self.parse_pure_bv(y)?;
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
                let fill_bit = self.parse_pure_bool(fill)?;
                // convert fill's sort from Bool to Bv(1)
                let fill_bit = if fill_bit.is_concretized() {
                    self.new_const(Sort::Bitv(1), fill_bit.evaluate())
                } else {
                    let one = self.new_const(Sort::Bitv(1), 1);
                    let zero = self.new_const(Sort::Bitv(1), 0);
                    self.new_ite(fill_bit, one, zero)
                };

                let value = self.parse_pure_bv(value)?;
                let length = length.clone();
                Ok(self.new_cast(value, fill_bit, length))
            }
            RzILInfo::Append { high, low } => {
                let high = self.parse_pure_bv(high)?;
                let low = self.parse_pure_bv(low)?;
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
                let key = self.parse_pure_bv(key)?;
                let symbolized = true;
                let sort = Sort::Bitv(8);
                let eval = 0;
                Ok(self.new_pure(PureCode::Load, vec![key], symbolized, sort, eval))
            }
            RzILInfo::Loadw { mem: _, key, bits } => {
                let key = self.parse_pure_bv(key)?;
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
            _ => Err(RzILError::UnkownPure),
        }
    }

    fn parse_effect(&mut self, op: &RzILInfo) -> RzILResult<Rc<Effect>> {
        match op {
            RzILInfo::Nop => Ok(self.new_nop()),
            RzILInfo::Set { dst, src } => {
                let name = dst;
                let src = self.parse_pure(src)?;
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
                self.set_var(name, dst.clone())?;
                Ok(self.new_effect(Effect::Set{dst, src}))
            }

            RzILInfo::Jmp { dst } => {
                let dst = self.parse_pure_bv(dst)?;
                Ok(self.new_effect(Effect::Jmp{dst}))
            }

            RzILInfo::Goto { label } => {
                Ok(self.new_effect(Effect::Goto{ label: label.to_owned() }))
            }
            RzILInfo::Seq { x, y } => {
                let mut args = Vec::new();
                self.parse_effect_seq(x, &mut args)?;
                self.parse_effect_seq(y, &mut args)?;
                if args.len() == 0 {
                    return Ok(self.new_nop());
                } else if args.len() == 1 {
                    return Ok(args.pop().unwrap());
                }
                Ok(self.new_effect(Effect::Seq{args}))
            }
            RzILInfo::Branch {
                condition,
                true_eff,
                false_eff,
            } => {
                let condition = self.parse_pure_bool(condition)?;

                self.bs_to_si.conditions.push(condition.clone());

                self.bs_to_si.taken.push(true);
                let then = self.parse_effect(true_eff)?;
                self.bs_to_si.taken.pop();

                self.bs_to_si.taken.push(false);
                let otherwise = self.parse_effect(false_eff)?;
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
                        args.push(self.new_effect(Effect::Set{dst, src:ite}));
                    }
                    self.bs_to_si.clear();
                }
                let need_branch =
                    !then.is_nop() || !otherwise.is_nop();
                if need_branch {
                    args.push(self.new_effect(Effect::Branch{condition, then, otherwise}));
                }
                if args.len() == 0 {
                    Ok(self.new_nop())
                } else if args.len() == 1 {
                    Ok(args.pop().unwrap())
                } else {
                    Ok(self.new_effect(Effect::Seq{args}))
                }
            }
            RzILInfo::Store { mem: _, key, value } => {
                let key = self.parse_pure_bv(key)?;
                let value = self.parse_pure_bv(value)?;
                Ok(self.new_effect(Effect::Store{key, value}))
            }
            RzILInfo::Storew { mem: _, key, value } => {
                let key = self.parse_pure_bv(key)?;
                let value = self.parse_pure_bv(value)?;
                Ok(self.new_effect(Effect::StoreW{key, value}))
            }
            RzILInfo::Blk {
                label: _,
                data: _,
                ctrl: _,
            } => Err(RzILError::UnimplementedRzILEffect("Blk".to_string())),
            RzILInfo::Repeat {
                condition: _,
                data_eff: _,
            } => Err(RzILError::UnimplementedRzILEffect("Repeat".to_string())),//Ok(self.new_nop()), //
            RzILInfo::Empty => Err(RzILError::Empty),
            _ => Err(RzILError::UnkownPure),
        }
    }

        fn parse_effect_seq(&mut self, op: &RzILInfo, vec: &mut Vec<Rc<Effect>>) -> RzILResult<()> {
        match op {
            RzILInfo::Seq { x, y } => {
                self.parse_effect_seq(x, vec)?;
                self.parse_effect_seq(y, vec)?;
            }
            _ => {
                match self.parse_effect(op) {
                    Ok(ret) => match &*ret {
                        Effect::Seq{args} => for op in args {
                                vec.push(op.clone())
                        }
                        _ => vec.push(ret)
                    },
                    Err(RzILError::EmptyNode) => (),
                    Err(err) => return Err(err),
                };
            }
        };
        Ok(())
    }

    fn fresh_var(
        &self,
        scope: Scope,
        value: &Rc<Pure>,
    ) -> Rc<Pure> {
        let symbolized = value.is_symbolized();
        let sort = value.get_sort();
        let eval = value.evaluate();
        let id = self.get_uniq_var_id();
        self.new_pure(PureCode::Var(scope, id), vec![], symbolized, sort, eval)
    }

    fn in_branch(&self) -> bool {
        !self.bs_to_si.conditions.is_empty() && !self.bs_to_si.taken.is_empty()
    }

    fn branch_set_to_set_ite_connect_condition(&self) -> RzILResult<Rc<Pure>> {
        let len = self.bs_to_si.conditions.len();
        let mut x = self.bs_to_si.conditions.last().unwrap().clone();
        for i in (0..len-1).rev() {
            if !x.is_bool() {
                return Err(RzILError::UnexpectedSort(Sort::Bool, x.get_sort()));
            }
            if i < len-2 && !self.bs_to_si.taken[i+1] {
                x = self.new_boolinv(x)
            }
            let y = self.bs_to_si.conditions[i].clone();
            x = self.new_booland(x, y);
        }
        Ok(x)
    }

    fn branch_set_to_set_ite_add_entry(&mut self, name: &str, src: Rc<Pure>, dst: Rc<Pure>) -> RzILResult<()> {
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
                        self.new_const(dst.get_sort(), 0)
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
