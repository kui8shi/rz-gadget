use bitflags::bitflags;
use rzapi::api::RzILInfo;
use std::collections::HashMap;
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
 * rzg.syscall("execve",[Str("/bin/bash"),NULL,NULL])
 * rzg.call("printf",[]);
 * let state = rzg.exec();
 * //rzg.call("execve", "/bin/bash", "NULL", "NULL" );
 * //rzg.call("write", "1", "\xfc\x6c", "0" );
 * */
type Result<T> = std::result::Result<T, RzILError>;
#[derive(Error, Debug)]
pub enum RzILError {
    #[error("Sort {expected} was expected, but {found} was found.")]
    UnexpectedSort { expected: Sort, found: Sort },
    #[error("Sort {0} and {1} should be identical.")]
    SortIntegrity(Sort, Sort),
    #[error("Unregistered variable {0} was rereferenced.")]
    UnregisteredVariableReferenced(String),
    #[error("Parse failed: {0}")]
    ParseError(#[from] std::num::ParseIntError),
    #[error("Unimplemented RzIL:{} detected", format!("{:?}",.0))]
    Unimplemented(Code),
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

pub enum Label {
    Address(u64),
    Syscall,
}

#[derive(Debug)]
pub enum PureCode {
    Var,
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

#[derive(Debug)]
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
}

#[derive(Debug)]
pub enum Code {
    Pure(PureCode),
    Effect(EffectCode),
    Unkown,
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
    pub fn evaluate(&self) -> u64 {
        self.eval
    }
    pub fn get_sort(&self) -> Sort {
        self.sort.clone()
    }
    pub fn get_size(&self) -> u32 {
        match self.sort {
            Sort::Bv(len) => len.min(64),
            Sort::Bool => 1,
        }
    }
    pub fn get_bitmask(&self) -> u64 {
        u64::MAX >> (u64::BITS - self.get_size())
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
}

#[derive(Debug)]
pub enum EffectArgs {
    PureArgs(Vec<Rc<Pure>>),
    EffectArgs(Vec<Rc<Effect>>),
    BranchArgs {
        condition: Rc<Pure>,
        then: Rc<Effect>,
        else_: Rc<Effect>,
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
}

impl Effect {}
pub struct Instruction {
    /*
     * //thread_id
     * address
     * events
     * expressions Vec<RzILEffect>
     * */
    address: u64,
    expressions: Vec<Effect>,
}

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct RzILContextConfig: u32 {
        const OptimizeBranch      = 0b00000001;
        const AnalyzeDependencies = 0b00000010;
        const All = Self::OptimizeBranch.bits() | Self::AnalyzeDependencies.bits();
    }
}
struct RzILContext {
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
}

impl RzILContext {
    pub fn new() -> Self {
        RzILContext {
            option: RzILContextConfig::All,
            vars: HashMap::new(),
            tmp_vars: HashMap::new(),
            labels: HashMap::new(),
            uniq_id: 0,
        }
    }

    fn get_uniq_id(&mut self) -> u64 {
        self.uniq_id = self.uniq_id + 1;
        self.uniq_id
    }
    fn create_pure_bv(&mut self, op: &RzILInfo) -> Result<Rc<Pure>> {
        let bitvector = self.create_pure(op)?;
        if !bitvector.is_bv() {
            Err(RzILError::UnexpectedSort {
                expected: Sort::Bv(64),
                found: bitvector.get_sort(),
            })
        } else {
            Ok(bitvector)
        }
    }
    fn create_pure_bool(&mut self, op: &RzILInfo) -> Result<Rc<Pure>> {
        let boolean = self.create_pure(op)?;
        if !boolean.is_bool() {
            Err(RzILError::UnexpectedSort {
                expected: Sort::Bool,
                found: boolean.get_sort(),
            })
        } else {
            Ok(boolean)
        }
    }
    fn create_pure(&mut self, op: &RzILInfo) -> Result<Rc<Pure>> {
        match op {
            RzILInfo::Var { value } => match self.vars.get(value) {
                Some(var) => Ok(var.clone()),
                None => {
                    let var = Rc::new(Pure {
                        code: PureCode::Var,
                        args: None,
                        sort: Sort::Bv(64), //self.regs.get(args.value).unwrap().get_size()
                        eval: 0,
                        id: self.get_uniq_id(),
                        symbolized: true,
                    });
                    self.vars.insert(value.to_owned(), var.clone());
                    Ok(var)
                }
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
                let id = self.get_uniq_id();
                let symbolized = condition.is_symbolized() | x.is_symbolized() | y.is_symbolized();
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
                code: PureCode::BoolInv,
                args: None,
                sort: Sort::Bv(len.clone()),
                eval: u64::from_str_radix(&bits[2..bits.len() - 1], 16).unwrap(),
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
                let sort = Sort::Bv(bv.get_size());
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
                Ok(Rc::new(Pure {
                    code: PureCode::Add,
                    args: Some(vec![x, y]),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
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
                Ok(Rc::new(Pure {
                    code: PureCode::Sub,
                    args: Some(vec![x, y]),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
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
                Ok(Rc::new(Pure {
                    code: PureCode::LogAnd,
                    args: Some(vec![x, y]),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
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
                Ok(Rc::new(Pure {
                    code: PureCode::LogAnd,
                    args: Some(vec![x, y]),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
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
                Ok(Rc::new(Pure {
                    code: PureCode::LogXor,
                    args: Some(vec![x, y]),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
            }
            RzILInfo::ShiftRight { fill_bit, x, y } => {
                let fill_bit = self.create_pure_bool(fill_bit)?;
                let x = self.create_pure_bv(x)?;
                let y = self.create_pure_bv(y)?;
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
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
                if x.get_sort() != y.get_sort() {
                    return Err(RzILError::SortIntegrity(x.get_sort(), y.get_sort()));
                }
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
                let sort = Sort::Bv(x.get_size());
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
                let sort = Sort::Bv(x.get_size());
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
                let sort = Sort::Bv(x.get_size());
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
                Ok(Rc::new(Pure {
                    code: PureCode::Cast,
                    args: Some(vec![value, fill]),
                    sort,
                    eval,
                    id: self.get_uniq_id(),
                    symbolized,
                }))
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
            _ => Err(RzILError::Unimplemented(Code::Unkown)),
        }
    }

    fn create_effect_seq(&mut self, op: &RzILInfo, vec: &mut Vec<Rc<Effect>>) -> Result<()> {
        match op {
            RzILInfo::Seq { x, y } => {
                self.create_effect_seq(x, vec)?;
                self.create_effect_seq(y, vec)?;
            }
            _ => {
                match self.create_effect(op) {
                    Ok(ret) => vec.push(ret),
                    Err(err) => return Err(err),
                };
            }
        };
        Ok(())
    }

    fn create_effect(&mut self, op: &RzILInfo) -> Result<Rc<Effect>> {
        match op {
            RzILInfo::Nop => Ok(Rc::new(Effect {
                code: EffectCode::Nop,
                label: None,
                args: EffectArgs::None,
            })),
            RzILInfo::Set { dst, src } => {
                let dst = match self.vars.get(dst) {
                    Some(var) => var.clone(),
                    None => return Err(RzILError::UnregisteredVariableReferenced(dst.to_owned())),
                };
                let src = self.create_pure_bv(src)?;
                Ok(Rc::new(Effect {
                    code: EffectCode::Set,
                    label: None,
                    args: EffectArgs::PureArgs(vec![dst, src]),
                }))
            }

            RzILInfo::Jmp { dst } => {
                let dst = self.create_pure_bv(dst)?;
                Ok(Rc::new(Effect {
                    code: EffectCode::Jmp,
                    label: None,
                    args: EffectArgs::PureArgs(vec![dst]),
                }))
            }

            RzILInfo::Goto { label } => Ok(Rc::new(Effect {
                code: EffectCode::Set,
                label: Some(label.to_owned()),
                args: EffectArgs::None,
            })),

            RzILInfo::Seq { x, y } => {
                let mut args = Vec::new();
                self.create_effect_seq(x, &mut args)?;
                self.create_effect_seq(y, &mut args)?;
                Ok(Rc::new(Effect {
                    code: EffectCode::Seq,
                    label: None,
                    args: EffectArgs::EffectArgs(args),
                }))
            }
            RzILInfo::Branch {
                condition,
                true_eff,
                false_eff,
            } => {
                let condition = self.create_pure_bool(condition)?;
                let true_eff = self.create_effect(true_eff)?;
                let false_eff = self.create_effect(false_eff)?;
                Ok(Rc::new(Effect {
                    code: EffectCode::Branch,
                    label: None,
                    args: EffectArgs::BranchArgs {
                        condition,
                        then: true_eff,
                        else_: false_eff,
                    },
                }))
            }
            RzILInfo::Store { mem: _, key, value } => {
                let key = self.create_pure_bv(key)?;
                let value = self.create_pure_bv(value)?;
                Ok(Rc::new(Effect {
                    code: EffectCode::Store,
                    label: None,
                    args: EffectArgs::PureArgs(vec![key, value]),
                }))
            }
            RzILInfo::Storew { mem: _, key, value } => {
                let key = self.create_pure_bv(key)?;
                let value = self.create_pure_bv(value)?;
                Ok(Rc::new(Effect {
                    code: EffectCode::Storew,
                    label: None,
                    args: EffectArgs::PureArgs(vec![key, value]),
                }))
            }
            RzILInfo::Blk {
                label: _,
                data: _,
                ctrl: _,
            } => Err(RzILError::Unimplemented(Code::Effect(EffectCode::Blk))),
            RzILInfo::Repeat {
                condition: _,
                data_eff: _,
            } => Err(RzILError::Unimplemented(Code::Effect(EffectCode::Repeat))),
            _ => Err(RzILError::Unimplemented(Code::Unkown)),
        }
    }
}
