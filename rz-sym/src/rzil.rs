use bitflags::bitflags;
use rzapi::api::RzApi;
use rzapi::api::RzResult;
use rzapi::api::{RzILInfo, RzILVMRegValue};
use std::collections::{HashMap, HashSet};
use std::fmt::Display;
use std::hash::Hash;
use std::rc::Rc;
use std::vec;
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
    #[error("Parse Int failed")]
    ParseInt(#[from] std::num::ParseIntError),
    #[error("String: {0} was not hex-decimal")]
    ParseFromStringToHexInt(String),
    #[error("RzApi failed: {0}")]
    ApiError(String),
    #[error("Optimized and Deleted RzIL Node")]
    OptimizeOut,
    #[error("RzIL {0} is Unimplemented")]
    UnimplementedRzIL(Code),
    #[error("Lifter of {0} is Unimplemented")]
    UnimplementedInstruction(String),
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
    Local,     // inside a Instruction
    LocalPure, // only inside a Let expression
}

pub enum Label {
    Address(u64),
    Syscall,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PureCode {
    Var(Scope, String),
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
    Cast,
    Append,
    Load,
    Loadw,
}

impl Display for PureCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
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
pub enum Code {
    Pure(PureCode),
    Effect(EffectCode),
    Unkown,
}

impl Display for Code {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Code::Pure(code) => code.fmt(f),
            Code::Effect(code) => code.fmt(f),
            Code::Unkown => write!(f, "Unkown"),
        }
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
    args: Option<Vec<Rc<Pure>>>,
    sort: Sort,
    eval: u64,
    id: u64,
    symbolized: bool,
}
impl Pure {
    fn evaluate(&self) -> u64 {
        self.eval
    }
    fn get_sort(&self) -> Sort {
        self.sort.clone()
    }
    fn get_id(&self) -> u64 {
        self.id
    }
    fn get_size(&self) -> u32 {
        match self.sort {
            Sort::Bv(len) => len.min(64),
            Sort::Bool => 1,
        }
    }
    fn get_bitmask(&self) -> u64 {
        u64::MAX >> (u64::BITS - self.get_size())
    }
    fn is(&self, code: PureCode) -> bool {
        self.code == code
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

#[derive(Debug)]
pub enum EffectArgs {
    PureArgs(Vec<Rc<Pure>>),
    EffectArgs(Vec<Rc<Effect>>),
    BranchArgs {
        condition: Rc<Pure>,
        then_effect: Rc<Effect>,
        else_effect: Rc<Effect>,
    },
    None,
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
    args: EffectArgs,
    symbolized: bool,
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
        const OptimizeBranch      = 0b00000001;
        const AnalyzeDependencies = 0b00000010;
        const All = Self::OptimizeBranch.bits() | Self::AnalyzeDependencies.bits();
    }
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
    vars: HashMap<String, Rc<Pure>>,
    tmp_vars: HashMap<String, Rc<Pure>>,
    labels: HashMap<String, Label>,
    uniq_id: u64,
    in_branch: bool,
}

impl RzILContext {
    pub fn new() -> Self {
        RzILContext {
            option: RzILContextConfig::All,
            vars: HashMap::new(),
            tmp_vars: HashMap::new(),
            labels: HashMap::new(),
            uniq_id: 0,
            in_branch: false,
        }
    }
    pub fn bind_registers(&mut self, api: &mut RzApi) -> RzILResult<()> {
        let rzilvm_status = self.api_result(api.get_rzil_vm_status())?;
        for (k, v) in &rzilvm_status {
            let sort = match v {
                RzILVMRegValue::String(string) => {
                    if !string.starts_with("0x") {
                        return Err(RzILError::ParseFromStringToHexInt(string.to_owned()));
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
            self.vars.insert(
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
        let insts = self.api_result(api.get_n_insts(Some(1), Some(addr)))?;
        let rzil = &insts[0].rzil;
        self.tmp_vars.clear();
        Ok(Instruction {
            address: addr,
            expression: self.create_effect(rzil)?,
        })
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
            println!("addr:{:x}", inst.addr);
            self.tmp_vars.clear();
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

    fn get_uniq_id(&mut self) -> u64 {
        self.uniq_id = self.uniq_id + 1;
        self.uniq_id
    }
    fn create_pure_bv(&mut self, op: &RzILInfo) -> RzILResult<Rc<Pure>> {
        let bitvector = self.create_pure(op)?;
        if !bitvector.is_bv() {
            Err(RzILError::UnexpectedSort(
                Sort::Bv(64),
                bitvector.get_sort(),
            ))
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
            RzILInfo::Var { value } => match self.vars.get(value) {
                Some(var) => Ok(var.clone()),
                None => match self.tmp_vars.get(value) {
                    Some(var) => Ok(var.clone()),
                    None => Err(RzILError::UndefinedVariableReferenced(value.to_owned())),
                },
            },
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
                    code: PureCode::Ite,
                    args: Some(vec![condition, x, y]),
                    sort,
                    eval,
                    id,
                    symbolized,
                }))
            }
            RzILInfo::Let { dst, exp, body } => {
                let binding = self.create_pure(exp)?;

                self.tmp_vars.insert(dst.to_owned(), binding);
                let body = self.create_pure(body)?;
                let sort = body.get_sort();
                let eval = body.evaluate();
                let id = self.get_uniq_id();
                let symbolized = body.is_symbolized();
                self.tmp_vars.remove(dst);

                Ok(Rc::new(Pure {
                    code: PureCode::Let,
                    args: Some(vec![body]),
                    sort,
                    eval,
                    id,
                    symbolized,
                }))
            }
            RzILInfo::Bool { value } => Ok(Rc::new(Pure {
                code: PureCode::Bool,
                args: None,
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
                    code: PureCode::BoolInv,
                    args: Some(vec![x]),
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
                    code: PureCode::BoolAnd,
                    args: Some(vec![x, y]),
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
                    code: PureCode::BoolOr,
                    args: Some(vec![x, y]),
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
                    code: PureCode::BoolXor,
                    args: Some(vec![x, y]),
                    sort: Sort::Bool,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::Bitv { bits, len } => Ok(Rc::new(Pure {
                code: PureCode::Bitv,
                args: None,
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
                    code: PureCode::Msb,
                    args: Some(vec![bv]),
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
                    code: PureCode::Lsb,
                    args: Some(vec![bv]),
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
                    code: PureCode::IsZero,
                    args: Some(vec![bv]),
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
                    code: PureCode::Neg,
                    args: Some(vec![bv]),
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
                    code: PureCode::LogNot,
                    args: Some(vec![bv]),
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
                        code: PureCode::Add,
                        args: Some(vec![x, y]),
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
                        code: PureCode::Sub,
                        args: Some(vec![x, y]),
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
                    code: PureCode::Mul,
                    args: Some(vec![x, y]),
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
                    code: PureCode::Div,
                    args: Some(vec![x, y]),
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
                    code: PureCode::Sdiv,
                    args: Some(vec![x, y]),
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
                    code: PureCode::Mod,
                    args: Some(vec![x, y]),
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
                    code: PureCode::Smod,
                    args: Some(vec![x, y]),
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
                        args: None,
                        sort,
                        eval: 0,
                        id: self.get_uniq_id(),
                        symbolized: false,
                    }))
                } else {
                    Ok(Rc::new(Pure {
                        code: PureCode::LogAnd,
                        args: Some(vec![x, y]),
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
                        code: PureCode::LogAnd,
                        args: Some(vec![x, y]),
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
                        code: PureCode::LogXor,
                        args: Some(vec![x, y]),
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
                let symbolized = x.is_symbolized() | y.is_symbolized();
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
                Ok(Rc::new(Pure {
                    code: PureCode::ShiftRight,
                    args: Some(vec![fill_bit, x, y]),
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
                let symbolized = x.is_symbolized() | y.is_symbolized();
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
                Ok(Rc::new(Pure {
                    code: PureCode::ShiftLeft,
                    args: Some(vec![fill_bit, x, y]),
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
                    code: PureCode::Equal,
                    args: Some(vec![x, y]),
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
                    code: PureCode::Sle,
                    args: Some(vec![x, y]),
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
                    code: PureCode::Ule,
                    args: Some(vec![x, y]),
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
                        code: PureCode::Cast,
                        args: Some(vec![value, fill]),
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
                let sort = Sort::Bv(64.min(high.get_size() + low.get_size()));
                let symbolized = high.is_symbolized() | low.is_symbolized();
                let eval = if !symbolized {
                    (high.evaluate() << low.get_size()) | low.evaluate()
                } else {
                    0
                };
                Ok(Rc::new(Pure {
                    code: PureCode::Append,
                    args: Some(vec![high, low]),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            _ => Err(RzILError::UnimplementedRzIL(Code::Unkown)),
        }
    }

    fn create_effect(&mut self, op: &RzILInfo) -> RzILResult<Rc<Effect>> {
        match op {
            RzILInfo::Nop => Ok(Rc::new(Effect {
                code: EffectCode::Nop,
                label: None,
                args: EffectArgs::None,
                symbolized: false,
            })),
            RzILInfo::Set { dst, src } => {
                let name = dst;
                let src = self.create_pure(src)?;
                if self.in_branch && self.option.contains(RzILContextConfig::OptimizeBranch) {
                    self.tmp_vars.insert(name.to_owned(), src);
                    return Err(RzILError::OptimizeOut);
                }
                let dst = match self.vars.get(name) {
                    Some(var) => {
                        if src.get_sort() != var.get_sort() {
                            return Err(RzILError::SortIntegrity(src.get_sort(), var.get_sort()));
                        }
                        let var = Rc::new(Pure {
                            code: PureCode::Var(Scope::Global, name.to_owned()),
                            args: None,
                            sort: src.get_sort(), //self.regs.get(value)?.get_sort()
                            eval: src.evaluate(),
                            id: self.get_uniq_id(),
                            symbolized: src.is_symbolized(),
                        });
                        self.vars.insert(name.to_owned(), var.clone());
                        var
                    }
                    None => {
                        let var = Rc::new(Pure {
                            code: PureCode::Var(Scope::Local, name.to_owned()),
                            args: None,
                            sort: src.get_sort(), //self.regs.get(value)?.get_sort()
                            eval: src.evaluate(),
                            id: self.get_uniq_id(),
                            symbolized: src.is_symbolized(),
                        });
                        self.tmp_vars.insert(name.to_owned(), var.clone());
                        var
                    }
                };
                let symbolized = dst.is_symbolized();
                println!("Set: {:? }", &dst);
                Ok(Rc::new(Effect {
                    code: EffectCode::Set,
                    label: None,
                    args: EffectArgs::PureArgs(vec![dst, src]),
                    symbolized,
                }))
            }

            RzILInfo::Jmp { dst } => {
                let dst = self.create_pure_bv(dst)?;
                let symbolized = dst.is_symbolized();
                Ok(Rc::new(Effect {
                    code: EffectCode::Jmp,
                    label: None,
                    args: EffectArgs::PureArgs(vec![dst]),
                    symbolized,
                }))
            }

            RzILInfo::Goto { label } => Ok(Rc::new(Effect {
                code: EffectCode::Goto,
                label: Some(label.to_owned()),
                args: EffectArgs::None,
                symbolized: false,
            })),

            RzILInfo::Seq { x, y } => {
                let mut args = Vec::new();
                self.create_effect_seq(x, &mut args)?;
                self.create_effect_seq(y, &mut args)?;
                if args.len() == 0 {
                    return Ok(self.create_effect_nop());
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
                    args: EffectArgs::EffectArgs(args),
                    symbolized,
                }))
            }
            RzILInfo::Branch {
                condition,
                true_eff,
                false_eff,
            } => {
                let condition = self.create_pure_bool(condition)?;

                self.in_branch = true;
                let then_effect = self.create_effect(true_eff)?;
                let then_set: HashMap<String, Rc<Pure>> = self.tmp_vars.drain().collect();
                let else_effect = self.create_effect(false_eff)?;
                let else_set: HashMap<String, Rc<Pure>> = self.tmp_vars.drain().collect();
                self.in_branch = false;

                let mut args = Vec::new();
                for k in HashSet::<&String>::from_iter(then_set.keys().chain(else_set.keys())) {
                    let then_pure = then_set
                        .get(k)
                        .map(|v| v.clone())
                        .unwrap_or(self.get_var(k)?);
                    let else_pure = else_set
                        .get(k)
                        .map(|v| v.clone())
                        .unwrap_or(self.get_var(k)?);
                    if then_pure.get_sort() != else_pure.get_sort() {
                        return Err(RzILError::SortIntegrity(
                            then_pure.get_sort(),
                            else_pure.get_sort(),
                        ));
                    }
                    let sort = then_pure.get_sort();
                    let eval = if condition.evaluate() != 0 {
                        then_pure.evaluate()
                    } else {
                        else_pure.evaluate()
                    };
                    let symbolized = condition.is_symbolized()
                        || (condition.evaluate() != 0 && then_pure.is_symbolized())
                        || (condition.evaluate() == 0 && else_pure.is_symbolized());
                    let id = self.get_uniq_id();
                    let ite = Rc::new(Pure {
                        code: PureCode::Ite,
                        args: Some(vec![condition.clone(), then_pure, else_pure]),
                        sort,
                        eval,
                        id,
                        symbolized,
                    });
                    let dst = self.add_fresh_var(k, eval, symbolized)?;
                    args.push(Rc::new(Effect {
                        code: EffectCode::Set,
                        label: None,
                        args: EffectArgs::PureArgs(vec![dst, ite]),
                        symbolized,
                    }));
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
                        args: EffectArgs::BranchArgs {
                            condition,
                            then_effect,
                            else_effect,
                        },
                        symbolized,
                    }))
                }
                if args.len() == 0 {
                    Ok(self.create_effect_nop())
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
                        args: EffectArgs::EffectArgs(args),
                        symbolized,
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
                    args: EffectArgs::PureArgs(vec![key, value]),
                    symbolized,
                }))
            }
            RzILInfo::Storew { mem: _, key, value } => {
                let key = self.create_pure_bv(key)?;
                let value = self.create_pure_bv(value)?;
                let symbolized = key.is_symbolized() || value.is_symbolized();
                Ok(Rc::new(Effect {
                    code: EffectCode::Storew,
                    label: None,
                    args: EffectArgs::PureArgs(vec![key, value]),
                    symbolized,
                }))
            }
            RzILInfo::Blk {
                label: _,
                data: _,
                ctrl: _,
            } => Err(RzILError::UnimplementedRzIL(Code::Effect(EffectCode::Blk))),
            RzILInfo::Repeat {
                condition: _,
                data_eff: _,
            } => Ok(self.create_effect_nop()), //Err(RzILError::Unimplemented(Code::Effect(EffectCode::Repeat))),
            RzILInfo::Empty => Err(RzILError::UnimplementedInstruction("unkown".to_owned())),
            _ => Err(RzILError::UnimplementedRzIL(Code::Unkown)),
        }
    }

    fn create_effect_nop(&mut self) -> Rc<Effect> {
        Rc::new(Effect {
            code: EffectCode::Nop,
            label: None,
            args: EffectArgs::None,
            symbolized: false,
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
                    Ok(ret) => vec.push(ret),
                    Err(RzILError::OptimizeOut) => (),
                    Err(err) => return Err(err),
                };
            }
        };
        Ok(())
    }

    fn get_var(&self, name: &str) -> RzILResult<Rc<Pure>> {
        match self.vars.get(name) {
            Some(var) => Ok(var.clone()),
            None => Err(RzILError::UndefinedVariableReferenced(name.to_owned())),
        }
    }
    fn add_fresh_var(&mut self, name: &str, eval: u64, symbolized: bool) -> RzILResult<Rc<Pure>> {
        match self.vars.get(name) {
            Some(var) => {
                let fresh_var = Rc::new(Pure {
                    code: PureCode::Var(Scope::Global, name.to_owned()),
                    args: None,
                    sort: var.get_sort(), //self.regs.get(value)?.get_sort()
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                });
                self.vars.insert(name.to_owned(), fresh_var.clone());
                Ok(fresh_var)
            }
            None => Err(RzILError::UndefinedVariableReferenced(name.to_owned())),
        }
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
