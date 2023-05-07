//#![allow(non_snake_case)]
use librz_il_sys::*;
use std::cell::Cell;
use std::ffi::CString;
use std::rc::Rc;

//type RzILOpPureArgsUnion = rz_il_op_pure_t__bindgen_ty_1;
//type RzILOpEffectArgsUnion = rz_il_op_effect_t__bindgen_ty_1;
//#[derive(Clone, Copy)]
//enum OpCode {
//    Unkown,
//    // OpPureCode
//    Var,
//    Ite,
//    Let,
//    B0,
//    B1,
//    Inv,
//    And,
//    Or,
//    Xor,
//    Bitv,
//    Msb,
//    Lsb,
//    IsZero,
//    Neg,
//    Lognot,
//    Add,
//    Sub,
//    Mul,
//    Div,
//    Sdiv,
//    Mod,
//    Smod,
//    Logand,
//    Logor,
//    Logxor,
//    Shiftr,
//    Shiftl,
//    Eq,
//    Sle,
//    Ule,
//    Cast,
//    Append,
//    Float,
//    Fbits,
//    IsFinite,
//    IsNan,
//    IsInf,
//    IsFzero,
//    IsFneg,
//    IsFpos,
//    Fneg,
//    Fabs,
//    FcastInt,
//    FcastSint,
//    FcastFloat,
//    FcastSfloat,
//    Fconvert,
//    Frequal,
//    Fsucc,
//    Fpred,
//    Forder,
//    Fround,
//    Fsqrt,
//    Frsqrt,
//    Fadd,
//    Fsub,
//    Fmul,
//    Fdiv,
//    Fmod,
//    Fhypot,
//    Fpow,
//    Fmad,
//    Frootn,
//    Fpown,
//    Fcompound,
//    Load,
//    Loadw,
//    // OpEffectCode
//    Store,
//    Storew,
//    Empty,
//    Nop,
//    Set,
//    Jmp,
//    Goto,
//    Seq,
//    Blk,
//    Repeat,
//    Branch,
//}
//impl OpCode {
//    fn from_pure(code: RzILOpPureCode) -> Self {
//        match code {
//            RzILOpPureCode_RZ_IL_OP_VAR => OpCode::Var,
//            RzILOpPureCode_RZ_IL_OP_ITE => OpCode::Ite,
//            RzILOpPureCode_RZ_IL_OP_LET => OpCode::Let,
//            RzILOpPureCode_RZ_IL_OP_B0 => OpCode::B0,
//            RzILOpPureCode_RZ_IL_OP_B1 => OpCode::B1,
//            RzILOpPureCode_RZ_IL_OP_INV => OpCode::Inv,
//            RzILOpPureCode_RZ_IL_OP_AND => OpCode::And,
//            RzILOpPureCode_RZ_IL_OP_OR => OpCode::Or,
//            RzILOpPureCode_RZ_IL_OP_XOR => OpCode::Xor,
//            RzILOpPureCode_RZ_IL_OP_BITV => OpCode::Bitv,
//            RzILOpPureCode_RZ_IL_OP_MSB => OpCode::Msb,
//            RzILOpPureCode_RZ_IL_OP_LSB => OpCode::Lsb,
//            RzILOpPureCode_RZ_IL_OP_IS_ZERO => OpCode::IsZero,
//            RzILOpPureCode_RZ_IL_OP_NEG => OpCode::Neg,
//            RzILOpPureCode_RZ_IL_OP_LOGNOT => OpCode::Lognot,
//            RzILOpPureCode_RZ_IL_OP_ADD => OpCode::Add,
//            RzILOpPureCode_RZ_IL_OP_SUB => OpCode::Sub,
//            RzILOpPureCode_RZ_IL_OP_MUL => OpCode::Mul,
//            RzILOpPureCode_RZ_IL_OP_DIV => OpCode::Div,
//            RzILOpPureCode_RZ_IL_OP_SDIV => OpCode::Sdiv,
//            RzILOpPureCode_RZ_IL_OP_MOD => OpCode::Mod,
//            RzILOpPureCode_RZ_IL_OP_SMOD => OpCode::Smod,
//            RzILOpPureCode_RZ_IL_OP_LOGAND => OpCode::Logand,
//            RzILOpPureCode_RZ_IL_OP_LOGOR => OpCode::Logor,
//            RzILOpPureCode_RZ_IL_OP_LOGXOR => OpCode::Logxor,
//            RzILOpPureCode_RZ_IL_OP_SHIFTR => OpCode::Shiftr,
//            RzILOpPureCode_RZ_IL_OP_SHIFTL => OpCode::Shiftl,
//            RzILOpPureCode_RZ_IL_OP_EQ => OpCode::Eq,
//            RzILOpPureCode_RZ_IL_OP_SLE => OpCode::Sle,
//            RzILOpPureCode_RZ_IL_OP_ULE => OpCode::Ule,
//            RzILOpPureCode_RZ_IL_OP_CAST => OpCode::Cast,
//            RzILOpPureCode_RZ_IL_OP_APPEND => OpCode::Append,
//            RzILOpPureCode_RZ_IL_OP_FLOAT => OpCode::Float,
//            RzILOpPureCode_RZ_IL_OP_FBITS => OpCode::Fbits,
//            RzILOpPureCode_RZ_IL_OP_IS_FINITE => OpCode::IsFinite,
//            RzILOpPureCode_RZ_IL_OP_IS_NAN => OpCode::IsNan,
//            RzILOpPureCode_RZ_IL_OP_IS_INF => OpCode::IsInf,
//            RzILOpPureCode_RZ_IL_OP_IS_FZERO => OpCode::IsFzero,
//            RzILOpPureCode_RZ_IL_OP_IS_FNEG => OpCode::IsFneg,
//            RzILOpPureCode_RZ_IL_OP_IS_FPOS => OpCode::IsFpos,
//            RzILOpPureCode_RZ_IL_OP_FNEG => OpCode::Fneg,
//            RzILOpPureCode_RZ_IL_OP_FABS => OpCode::Fabs,
//            RzILOpPureCode_RZ_IL_OP_FCAST_INT => OpCode::FcastInt,
//            RzILOpPureCode_RZ_IL_OP_FCAST_SINT => OpCode::FcastSint,
//            RzILOpPureCode_RZ_IL_OP_FCAST_FLOAT => OpCode::FcastFloat,
//            RzILOpPureCode_RZ_IL_OP_FCAST_SFLOAT => OpCode::FcastSfloat,
//            RzILOpPureCode_RZ_IL_OP_FCONVERT => OpCode::Fconvert,
//            RzILOpPureCode_RZ_IL_OP_FREQUAL => OpCode::Frequal,
//            RzILOpPureCode_RZ_IL_OP_FSUCC => OpCode::Fsucc,
//            RzILOpPureCode_RZ_IL_OP_FPRED => OpCode::Fpred,
//            RzILOpPureCode_RZ_IL_OP_FORDER => OpCode::Forder,
//            RzILOpPureCode_RZ_IL_OP_FROUND => OpCode::Fround,
//            RzILOpPureCode_RZ_IL_OP_FSQRT => OpCode::Fsqrt,
//            RzILOpPureCode_RZ_IL_OP_FRSQRT => OpCode::Frsqrt,
//            RzILOpPureCode_RZ_IL_OP_FADD => OpCode::Fadd,
//            RzILOpPureCode_RZ_IL_OP_FSUB => OpCode::Fsub,
//            RzILOpPureCode_RZ_IL_OP_FMUL => OpCode::Fmul,
//            RzILOpPureCode_RZ_IL_OP_FDIV => OpCode::Fdiv,
//            RzILOpPureCode_RZ_IL_OP_FMOD => OpCode::Fmod,
//            RzILOpPureCode_RZ_IL_OP_FHYPOT => OpCode::Fhypot,
//            RzILOpPureCode_RZ_IL_OP_FPOW => OpCode::Fpow,
//            RzILOpPureCode_RZ_IL_OP_FMAD => OpCode::Fmad,
//            RzILOpPureCode_RZ_IL_OP_FROOTN => OpCode::Frootn,
//            RzILOpPureCode_RZ_IL_OP_FPOWN => OpCode::Fpown,
//            RzILOpPureCode_RZ_IL_OP_FCOMPOUND => OpCode::Fcompound,
//            RzILOpPureCode_RZ_IL_OP_LOAD => OpCode::Load,
//            RzILOpPureCode_RZ_IL_OP_LOADW => OpCode::Loadw,
//            _ => OpCode::Unkown,
//        }
//    }
//    fn from_effect(code: RzILOpEffectCode) -> Self {
//        match code {
//            RzILOpEffectCode_RZ_IL_OP_STORE => OpCode::Store,
//            RzILOpEffectCode_RZ_IL_OP_STOREW => OpCode::Storew,
//            RzILOpEffectCode_RZ_IL_OP_EMPTY => OpCode::Empty,
//            RzILOpEffectCode_RZ_IL_OP_NOP => OpCode::Nop,
//            RzILOpEffectCode_RZ_IL_OP_SET => OpCode::Set,
//            RzILOpEffectCode_RZ_IL_OP_JMP => OpCode::Jmp,
//            RzILOpEffectCode_RZ_IL_OP_GOTO => OpCode::Goto,
//            RzILOpEffectCode_RZ_IL_OP_SEQ => OpCode::Seq,
//            RzILOpEffectCode_RZ_IL_OP_BLK => OpCode::Blk,
//            RzILOpEffectCode_RZ_IL_OP_REPEAT => OpCode::Repeat,
//            RzILOpEffectCode_RZ_IL_OP_BRANCH => OpCode::Branch,
//            _ => OpCode::Unkown,
//        }
//    }
//}
//macro_rules! wrap_op {
//    ($c_op:ident, $op:ident,$args:ident, $code:ident) => {
//        struct $op {
//            code: OpCode,
//            args: $args,
//        }
//        impl $op {
//            unsafe fn wrap(op: $c_op) -> Self {
//                Self {
//                    code: OpCode::$code(op.code),
//                    args: op.op,
//                }
//            }
//            fn get_code(&self) -> OpCode {
//                self.code
//            }
//            fn get_args(&self) -> $args {
//                self.args
//            }
//        }
//    };
//}

//struct OpPure {
//    code: OpCode,
//    args: RzILOpPureArgsUnion,
//}
//impl OpPure {
//    unsafe fn wrap(op: RzILOpPure) -> Self {
//        Self {
//            code: op.code,
//            args: op.op,
//        }
//    }
//    fn get_code(&self) -> OpCode {
//        OpCode::from_pure(self.code)
//    }
//    fn get_args(&self) -> RzILOpPureArgsUnion {
//        self.args
//    }
//}
//struct OpEffect {
//    code: OpCode,
//    args: RzILOpEffectArgsUnion,
//}
//impl OpEffect {
//    unsafe fn wrap(op: RzILOpEffect) -> Self {
//        Self {
//            code: op.code,
//            args: op.op,
//        }
//    }
//    fn get_code(&self) -> OpCode {
//        OpCode::from_effect(self.code)
//    }
//    fn get_args(&self) -> RzILOpEffectArgsUnion {
//        self.args
//    }
//}
/*
let op = rzil::op::Eq(bitv1, bitv2);
let op = rzil::op::Add(bitv1, bitv2)
let op = rzil::op::BoolAnd(bool1, bool2)
op = rzil::op::BoolAnd::from(op);
args = op.get_args(); // already typed
args.x
args.y
*/
type Result<T> = std::result::Result<T, &'static str>;
type Dummy = std::marker::PhantomData<()>;
macro_rules! abstruct_ctype {
    ($c_name:ident, $struct:ident, $dtor:ident) => {
        pub struct $struct {
            rzil_sys: *mut $c_name,
            rc: Rc<Cell<usize>>,
        }
        #[allow(dead_code)]
        impl $struct {
            unsafe fn wrap(rzil_sys: *mut $c_name) -> Result<Self>
            where
                Self: Sized,
            {
                if rzil_sys.is_null() {
                    Err("failed to wrap")
                } else {
                    Ok($struct {
                        rzil_sys: rzil_sys,
                        rc: Rc::new(Cell::new(1)),
                    })
                }
            }
            fn get(&self) -> Result<&$c_name> {
                if self.rc.get() == 0 {
                    Err("does not have ownership")
                } else if self.rzil_sys.is_null() {
                    Err("null dereference detected.")
                } else {
                    unsafe { Ok(&*(self.rzil_sys)) }
                }
            }
            fn mov(self) -> Result<*mut $c_name> {
                println!(
                    "move occured! \n\t type:{} rc:{}->0\n\tdbg:{:?}",
                    stringify!($struct),
                    self.rc.get(),
                    self.rzil_sys
                );
                if self.rc.get() == 0 {
                    Err("does not have ownership")
                } else if self.rzil_sys.is_null() {
                    Err("null dereference detected.")
                } else {
                    self.rc.set(0);
                    Ok(as_mut_ptr!(&(self.rzil_sys)))
                }
            }
        }
        impl Clone for $struct {
            fn clone(&self) -> Self {
                println!(
                    "clone occured! \n\t type:{} rc:{}->{}\n\tdbg:{:?}",
                    stringify!($struct),
                    self.rc.get(),
                    self.rc.get() + 1,
                    self.rzil_sys
                );
                self.rc.set(self.rc.get() + 1);
                $struct {
                    rzil_sys: self.rzil_sys.clone(),
                    rc: self.rc.clone(),
                }
            }
        }
        impl Drop for $struct {
            fn drop(&mut self) {
                println!(
                    "drop occured! \n\t type:{} rc:{}\n\tdbg:{:?}",
                    stringify!($struct),
                    self.rc.get(),
                    self.rzil_sys
                );
                if self.rc.get() <= 0 {
                    return;
                }
                let rc = self.rc.get();
                if rc == 1 {
                    println!("\t{} occured!", stringify!($dtor),);
                    unsafe {
                        $dtor(self.rzil_sys);
                    }
                }
                self.rc.set(rc - 1);
                println!("\trc after:{}", self.rc.get(),);
            }
        }
    };
}
macro_rules! as_mut_ptr {
    ($ref: expr) => {
        $ref as *const _ as *mut _
    };
}
unsafe fn strbuf_free(strbuf: *mut RzStrBuf) {
    println!("strbuf_free called!");
    dbg!(strbuf);
    rz_strbuf_free(strbuf);
}
abstruct_ctype!(RzStrBuf, StrBuf, strbuf_free);
abstruct_ctype!(RzILOpPure, OpPure, rz_il_op_pure_free);
//abstruct_ctype!(RzILOpEffect, OpEffect, rz_il_op_effect_free);
//abstruct_ctype!(RzILMem, Mem, rz_il_mem_free);
//abstruct_ctype!(RzILVar, Var, rz_il_variable_free);
//abstruct_ctype!(RzILVal, Val, rz_il_value_free);
//abstruct_ctype!(RzBitVector, BitVector, rz_bv_free);

//pub trait Pure {
//    type OpArgs;
//    unsafe fn wrap(op_pure: *mut RzILOpPure) -> Result<Self> {
//        if op_pure.is_null() {
//            Err("failed")
//        } else {
//            Ok(Self {
//                op: Rc::new(op_pure),
//            })
//        }
//    }
//    fn new(args: OpArgs) -> Result<Self>;
//    fn args(&self) -> OpArgs;
//}
//enum OpPure {
//    BoolAnd(BoolAnd)
//}
//struct BoolAnd {
//    x: OpPure,
//    y: OpPure,
//}
//impl Pure for OpBoolAnd {
//    type RzILOpArgs = RzILOpArgsBoolAnd;
//    fn args(&self) -> OpArgs {
//        self.op
//    }
//}
impl Default for StrBuf {
    fn default() -> Self {
        //let c_string = CString::new("").unwrap();
        unsafe { Self::wrap(rz_strbuf_new(std::ptr::null())).unwrap() }
    }
}
impl StrBuf {
    fn get_string(&self) -> Result<String> {
        let str;
        unsafe { str = CString::from_raw(rz_strbuf_get(as_mut_ptr!(self.get()?))) }
        match str.into_string() {
            Ok(s) => Ok(s),
            Err(_) => Err("IntoStringError"),
        }
    }
}
// implement rz_il_op_new_~ functions
#[allow(dead_code)]
impl OpPure {
    fn new_ite(condition: OpPure, x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_ite(condition.mov()?, x.mov()?, y.mov()?)) }
    }
    fn new_var(var: &str, kind: RzILVarKind) -> Result<Self> {
        let c_string = CString::new(var).unwrap();
        unsafe { Self::wrap(rz_il_op_new_var(c_string.as_ptr(), kind)) }
    }
    fn new_let(name: &str, exp: OpPure, body: OpPure) -> Result<Self> {
        let c_string = CString::new(name).unwrap();
        unsafe { Self::wrap(rz_il_op_new_let(c_string.as_ptr(), exp.mov()?, body.mov()?)) }
    }
    fn new_false() -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_b0()) }
    }
    fn new_true() -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_b1()) }
    }
    fn new_bool_and(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_bool_and(x.mov()?, y.mov()?)) }
    }
    fn new_bool_or(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_bool_or(x.mov()?, y.mov()?)) }
    }
    fn new_bool_xor(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_bool_xor(x.mov()?, y.mov()?)) }
    }
    fn new_bool_inv(x: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_bool_inv(x.mov()?)) }
    }
    fn new_bitv(value: &RzBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_bitv(as_mut_ptr!(value))) }
    }
    fn new_bitv_from_ut64(length: u32, number: u64) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_bitv_from_ut64(length, number)) }
    }
    fn new_bitv_from_it64(length: u32, number: i64) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_bitv_from_st64(length, number)) }
    }
    fn new_msb(bv: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_msb(bv.mov()?)) }
    }
    fn new_lsb(bv: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_lsb(bv.mov()?)) }
    }
    fn new_is_zero(bv: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_is_zero(bv.mov()?)) }
    }
    fn new_non_zero(bv: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_is_zero(bv.mov()?)) }
    }
    fn new_eq(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_eq(x.mov()?, y.mov()?)) }
    }
    fn new_ule(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_ule(x.mov()?, y.mov()?)) }
    }
    fn new_sle(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_sle(x.mov()?, y.mov()?)) }
    }
    fn new_ult(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_ult(x.mov()?, y.mov()?)) }
    }
    fn new_slt(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_slt(x.mov()?, y.mov()?)) }
    }
    fn new_uge(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_uge(x.mov()?, y.mov()?)) }
    }
    fn new_sge(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_sge(x.mov()?, y.mov()?)) }
    }
    fn new_ugt(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_ugt(x.mov()?, y.mov()?)) }
    }
    fn new_sgt(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_sgt(x.mov()?, y.mov()?)) }
    }
    fn new_cast(length: u32, fill: OpPure, val: OpPure) -> Result<Self> {
        unsafe {
            Self::wrap(rz_il_op_new_cast(
                length as std::os::raw::c_uint,
                fill.mov()?,
                val.mov()?,
            ))
        }
    }
    fn new_unsinged(length: u32, val: OpPure) -> Result<Self> {
        unsafe {
            Self::wrap(rz_il_op_new_unsigned(
                length as std::os::raw::c_uint,
                val.mov()?,
            ))
        }
    }
    fn new_signed(length: u32, val: OpPure) -> Result<Self> {
        unsafe {
            Self::wrap(rz_il_op_new_signed(
                length as std::os::raw::c_uint,
                val.mov()?,
            ))
        }
    }

    fn new_neg(value: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_neg(value.mov()?)) }
    }
    fn new_add(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_add(x.mov()?, y.mov()?)) }
    }
    fn new_sub(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_sub(x.mov()?, y.mov()?)) }
    }
    fn new_mul(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_mul(x.mov()?, y.mov()?)) }
    }
    fn new_div(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_div(x.mov()?, y.mov()?)) }
    }
    fn new_sdiv(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_sdiv(x.mov()?, y.mov()?)) }
    }
    fn new_smod(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_smod(x.mov()?, y.mov()?)) }
    }
    fn new_mod(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_mod(x.mov()?, y.mov()?)) }
    }
    fn new_log_and(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_log_and(x.mov()?, y.mov()?)) }
    }
    fn new_log_or(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_log_or(x.mov()?, y.mov()?)) }
    }
    fn new_log_xor(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_log_xor(x.mov()?, y.mov()?)) }
    }
    fn new_shiftl(fill_bit: OpPure, x: OpPure, sh: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_shiftl(fill_bit.mov()?, x.mov()?, sh.mov()?)) }
    }
    fn new_shiftr(fill_bit: OpPure, x: OpPure, sh: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_shiftr(fill_bit.mov()?, x.mov()?, sh.mov()?)) }
    }
    fn new_shiftr_arith(x: OpPure, y: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_shiftr_arith(x.mov()?, y.mov()?)) }
    }
    fn new_append(high: OpPure, low: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_append(high.mov()?, low.mov()?)) }
    }
    fn new_load(mem: u32, key: OpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_load(mem as RzILMemIndex, key.mov()?)) }
    }
    fn new_loadw(mem: u32, key: OpPure, n_bits: u32) -> Result<Self> {
        unsafe {
            Self::wrap(rz_il_op_new_loadw(
                mem as RzILMemIndex,
                key.mov()?,
                n_bits as std::os::raw::c_uint,
            ))
        }
    }
}
macro_rules! dbg_if_err {
    ($expr:expr) => {
        match $expr {
            Ok(val) => val,
            Err(e) => {
                dbg!(e);
                panic!()
            }
        }
    };
}
#[allow(dead_code)]
impl OpPure {
    fn stringify(&self) -> Result<String> {
        let strbuf = StrBuf::default();
        unsafe { rz_il_op_pure_stringify(as_mut_ptr!(self.get()?), as_mut_ptr!(strbuf.get()?)) }
        strbuf.get_string()
    } // strbuf drop
}
#[cfg(test)]
mod tests {
    use super::*;
    //#[test]
    fn create_op_true() {
        let op = OpPure::new_true().unwrap();
        assert!(op.get().unwrap().code == RzILOpPureCode::RZ_IL_OP_B1);
    }
    //#[test]
    fn create_op_false() {
        let op = OpPure::new_false().unwrap();
        assert!(op.get().unwrap().code == RzILOpPureCode::RZ_IL_OP_B0);
    }
    //#[test]
    fn create_op_ite() {
        let condition = dbg_if_err!(OpPure::new_false());
        let x = dbg_if_err!(OpPure::new_bitv_from_ut64(64, 0xdeadbeef));
        let y = dbg_if_err!(OpPure::new_bitv_from_ut64(64, 0xffffffff));
        let op = dbg_if_err!(OpPure::new_ite(condition, x, y));
        assert!(op.get().unwrap().code == RzILOpPureCode::RZ_IL_OP_ITE);
    }
    #[test]
    fn stringify() {
        let op = OpPure::new_false().unwrap();
        //dbg_if_err!(op.get());
        //dbg_if_err!(op.stringify());
        let op_str = op.stringify().unwrap();
        dbg!(op_str);
    }
}
//pub struct OpBoolAnd {
//    code: RzILOpPureCode,
//    args: RzILOpArgs,
//}
//#[derive(Debug, Clone)]
//struct Error;
//impl std::fmt::Display for Error {
//    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
//        write!(f, "invalid opcode")
//    }
//}
//impl std::error::Error for Error {
//    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
//        None
//    }
//}
//type Result<T> = std::result::Result<T, Error>;
//impl OpBoolAnd {
//    unsafe fn from(op: *mut RzILOpPure) -> Result<Self> {
//        if (*op).code != RzILOpPureCode::RZ_IL_OP_AND {
//            Err(Error {})
//        } else {
//            Ok(OpBoolAnd {
//                code: (*op).code,
//                args: (*op).op.booland,
//            })
//        }
//    }
//    fn new(x: OpPure, y: OpPure) -> Result<Self> {
//        unsafe {
//            Self::from(rz_il_op_new_bool_and(
//                x as *const _ as *mut _,
//                y as *const _ as *mut _,
//            ))
//        }
//    }
//}
//
////abstruct_rzil_sys!(RzILEffectLabel, EffectLabel, rz_il_effect_label_free);
////abstruct_rzil_sys!(RzILVarSet, VarSet, rz_il_var_set_fini);
