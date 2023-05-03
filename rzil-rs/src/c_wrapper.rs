#![allow(non_upper_case_globals)]
//#![allow(non_snake_case)]
use librz_il_sys::*;
use std::cell::Cell;
use std::fmt;
use std::marker::PhantomData;
use std::rc::Rc;

//type RzILOpPureArgsUnion = rz_il_op_pure_t__bindgen_ty_1;
//type RzILOpEffectArgsUnion = rz_il_op_effect_t__bindgen_ty_1;
#[derive(Clone, Copy)]
enum OpCode {
    Unkown,
    // OpPureCode
    Var,
    Ite,
    Let,
    B0,
    B1,
    Inv,
    And,
    Or,
    Xor,
    Bitv,
    Msb,
    Lsb,
    IsZero,
    Neg,
    Lognot,
    Add,
    Sub,
    Mul,
    Div,
    Sdiv,
    Mod,
    Smod,
    Logand,
    Logor,
    Logxor,
    Shiftr,
    Shiftl,
    Eq,
    Sle,
    Ule,
    Cast,
    Append,
    Float,
    Fbits,
    IsFinite,
    IsNan,
    IsInf,
    IsFzero,
    IsFneg,
    IsFpos,
    Fneg,
    Fabs,
    FcastInt,
    FcastSint,
    FcastFloat,
    FcastSfloat,
    Fconvert,
    Frequal,
    Fsucc,
    Fpred,
    Forder,
    Fround,
    Fsqrt,
    Frsqrt,
    Fadd,
    Fsub,
    Fmul,
    Fdiv,
    Fmod,
    Fhypot,
    Fpow,
    Fmad,
    Frootn,
    Fpown,
    Fcompound,
    Load,
    Loadw,
    // OpEffectCode
    Store,
    Storew,
    Empty,
    Nop,
    Set,
    Jmp,
    Goto,
    Seq,
    Blk,
    Repeat,
    Branch,
}
impl OpCode {
    fn from_pure(code: RzILOpPureCode) -> Self {
        match code {
            RzILOpPureCode_RZ_IL_OP_VAR => OpCode::Var,
            RzILOpPureCode_RZ_IL_OP_ITE => OpCode::Ite,
            RzILOpPureCode_RZ_IL_OP_LET => OpCode::Let,
            RzILOpPureCode_RZ_IL_OP_B0 => OpCode::B0,
            RzILOpPureCode_RZ_IL_OP_B1 => OpCode::B1,
            RzILOpPureCode_RZ_IL_OP_INV => OpCode::Inv,
            RzILOpPureCode_RZ_IL_OP_AND => OpCode::And,
            RzILOpPureCode_RZ_IL_OP_OR => OpCode::Or,
            RzILOpPureCode_RZ_IL_OP_XOR => OpCode::Xor,
            RzILOpPureCode_RZ_IL_OP_BITV => OpCode::Bitv,
            RzILOpPureCode_RZ_IL_OP_MSB => OpCode::Msb,
            RzILOpPureCode_RZ_IL_OP_LSB => OpCode::Lsb,
            RzILOpPureCode_RZ_IL_OP_IS_ZERO => OpCode::IsZero,
            RzILOpPureCode_RZ_IL_OP_NEG => OpCode::Neg,
            RzILOpPureCode_RZ_IL_OP_LOGNOT => OpCode::Lognot,
            RzILOpPureCode_RZ_IL_OP_ADD => OpCode::Add,
            RzILOpPureCode_RZ_IL_OP_SUB => OpCode::Sub,
            RzILOpPureCode_RZ_IL_OP_MUL => OpCode::Mul,
            RzILOpPureCode_RZ_IL_OP_DIV => OpCode::Div,
            RzILOpPureCode_RZ_IL_OP_SDIV => OpCode::Sdiv,
            RzILOpPureCode_RZ_IL_OP_MOD => OpCode::Mod,
            RzILOpPureCode_RZ_IL_OP_SMOD => OpCode::Smod,
            RzILOpPureCode_RZ_IL_OP_LOGAND => OpCode::Logand,
            RzILOpPureCode_RZ_IL_OP_LOGOR => OpCode::Logor,
            RzILOpPureCode_RZ_IL_OP_LOGXOR => OpCode::Logxor,
            RzILOpPureCode_RZ_IL_OP_SHIFTR => OpCode::Shiftr,
            RzILOpPureCode_RZ_IL_OP_SHIFTL => OpCode::Shiftl,
            RzILOpPureCode_RZ_IL_OP_EQ => OpCode::Eq,
            RzILOpPureCode_RZ_IL_OP_SLE => OpCode::Sle,
            RzILOpPureCode_RZ_IL_OP_ULE => OpCode::Ule,
            RzILOpPureCode_RZ_IL_OP_CAST => OpCode::Cast,
            RzILOpPureCode_RZ_IL_OP_APPEND => OpCode::Append,
            RzILOpPureCode_RZ_IL_OP_FLOAT => OpCode::Float,
            RzILOpPureCode_RZ_IL_OP_FBITS => OpCode::Fbits,
            RzILOpPureCode_RZ_IL_OP_IS_FINITE => OpCode::IsFinite,
            RzILOpPureCode_RZ_IL_OP_IS_NAN => OpCode::IsNan,
            RzILOpPureCode_RZ_IL_OP_IS_INF => OpCode::IsInf,
            RzILOpPureCode_RZ_IL_OP_IS_FZERO => OpCode::IsFzero,
            RzILOpPureCode_RZ_IL_OP_IS_FNEG => OpCode::IsFneg,
            RzILOpPureCode_RZ_IL_OP_IS_FPOS => OpCode::IsFpos,
            RzILOpPureCode_RZ_IL_OP_FNEG => OpCode::Fneg,
            RzILOpPureCode_RZ_IL_OP_FABS => OpCode::Fabs,
            RzILOpPureCode_RZ_IL_OP_FCAST_INT => OpCode::FcastInt,
            RzILOpPureCode_RZ_IL_OP_FCAST_SINT => OpCode::FcastSint,
            RzILOpPureCode_RZ_IL_OP_FCAST_FLOAT => OpCode::FcastFloat,
            RzILOpPureCode_RZ_IL_OP_FCAST_SFLOAT => OpCode::FcastSfloat,
            RzILOpPureCode_RZ_IL_OP_FCONVERT => OpCode::Fconvert,
            RzILOpPureCode_RZ_IL_OP_FREQUAL => OpCode::Frequal,
            RzILOpPureCode_RZ_IL_OP_FSUCC => OpCode::Fsucc,
            RzILOpPureCode_RZ_IL_OP_FPRED => OpCode::Fpred,
            RzILOpPureCode_RZ_IL_OP_FORDER => OpCode::Forder,
            RzILOpPureCode_RZ_IL_OP_FROUND => OpCode::Fround,
            RzILOpPureCode_RZ_IL_OP_FSQRT => OpCode::Fsqrt,
            RzILOpPureCode_RZ_IL_OP_FRSQRT => OpCode::Frsqrt,
            RzILOpPureCode_RZ_IL_OP_FADD => OpCode::Fadd,
            RzILOpPureCode_RZ_IL_OP_FSUB => OpCode::Fsub,
            RzILOpPureCode_RZ_IL_OP_FMUL => OpCode::Fmul,
            RzILOpPureCode_RZ_IL_OP_FDIV => OpCode::Fdiv,
            RzILOpPureCode_RZ_IL_OP_FMOD => OpCode::Fmod,
            RzILOpPureCode_RZ_IL_OP_FHYPOT => OpCode::Fhypot,
            RzILOpPureCode_RZ_IL_OP_FPOW => OpCode::Fpow,
            RzILOpPureCode_RZ_IL_OP_FMAD => OpCode::Fmad,
            RzILOpPureCode_RZ_IL_OP_FROOTN => OpCode::Frootn,
            RzILOpPureCode_RZ_IL_OP_FPOWN => OpCode::Fpown,
            RzILOpPureCode_RZ_IL_OP_FCOMPOUND => OpCode::Fcompound,
            RzILOpPureCode_RZ_IL_OP_LOAD => OpCode::Load,
            RzILOpPureCode_RZ_IL_OP_LOADW => OpCode::Loadw,
            _ => OpCode::Unkown,
        }
    }
    fn from_effect(code: RzILOpEffectCode) -> Self {
        match code {
            RzILOpEffectCode_RZ_IL_OP_STORE => OpCode::Store,
            RzILOpEffectCode_RZ_IL_OP_STOREW => OpCode::Storew,
            RzILOpEffectCode_RZ_IL_OP_EMPTY => OpCode::Empty,
            RzILOpEffectCode_RZ_IL_OP_NOP => OpCode::Nop,
            RzILOpEffectCode_RZ_IL_OP_SET => OpCode::Set,
            RzILOpEffectCode_RZ_IL_OP_JMP => OpCode::Jmp,
            RzILOpEffectCode_RZ_IL_OP_GOTO => OpCode::Goto,
            RzILOpEffectCode_RZ_IL_OP_SEQ => OpCode::Seq,
            RzILOpEffectCode_RZ_IL_OP_BLK => OpCode::Blk,
            RzILOpEffectCode_RZ_IL_OP_REPEAT => OpCode::Repeat,
            RzILOpEffectCode_RZ_IL_OP_BRANCH => OpCode::Branch,
            _ => OpCode::Unkown,
        }
    }
}
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
macro_rules! abstruct_ctype {
    ($(#[$attr:meta])* $c_name:ident, $struct:ident, $dtor:ident) => {
        $(#[$attr])*
        pub struct $struct {
            rzil_sys: *mut librz_il_sys::$c_name,
            rc: Rc<Cell<usize>>,
        }
        impl $struct {
            unsafe fn wrap(rzil_sys:*mut $c_name)->Self
            where Self: Sized {
                // argument should be newly created for this call.
                // e.g. let x = <struct>::new_wrap(rz_il_<rzil_sys>_new());
                $struct {
                    rzil_sys: rzil_sys,
                    rc: Rc::new(Cell::new(1)),
                }
            }
        }
        impl Clone for $struct {
            fn clone(&self) -> Self {
                self.rc.set(self.rc.get() + 1);
                $struct {
                    rzil_sys: self.rzil_sys,
                    rc: self.rc.clone(),
                }
            }
        }
        impl Drop for $struct {
            fn drop(&mut self) {
                let rc = self.rc.get();
                if rc == 1 {
                    unsafe {
                        $dtor(self.rzil_sys);
                    }
                } else {
                    self.rc.set(rc - 1)
                }
            }
        }
    };
}
//abstruct_ctype!(RzILOpPure, OpPure, rz_il_op_pure_free);
//abstruct_ctype!(RzILOpEffect, OpEffect, rz_il_op_effect_free);
abstruct_ctype!(RzILMem, Mem, rz_il_mem_free);
abstruct_ctype!(RzILVar, Var, rz_il_variable_free);
abstruct_ctype!(RzILVal, Val, rz_il_value_free);
abstruct_ctype!(RzBitVector, BitVector, rz_bv_free);
type RzILOpPureArgs = rz_il_op_pure_t__bindgen_ty_1;
pub struct OpPure {
    op: Rc<*mut RzILOpPure>,
}
macro_rules! as_mut_ptr {
    ($ref: ident) => {
        $ref as *const _ as *mut _
    };
}
type Result<T> = std::result::Result<T, &'static str>;
impl OpPure {
    unsafe fn wrap(op_pure: *mut RzILOpPure) -> Result<Self> {
        if op_pure.is_null() {
            Err("failed")
        } else {
            Ok(OpPure {
                op: Rc::new(op_pure),
            })
        }
    }
    fn new_bool_and(x: &RzILOpBool, y: &RzILOpBool) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_bool_and(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_bool_or(x: &RzILOpBool, y: &RzILOpBool) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_bool_or(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_bool_xor(x: &RzILOpBool, y: &RzILOpBool) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_bool_xor(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_bool_inv(x: &RzILOpBool) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_bool_inv(as_mut_ptr!(x))) }
    }
    fn new_bitv(value: &RzBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_bitv(as_mut_ptr!(value))) }
    }
    fn new_bitv_from_u64(length: u32, number: u64) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_bitv_from_ut64(length, number)) }
    }
    fn new_bitv_from_i64(length: u32, number: i64) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_bitv_from_st64(length, number)) }
    }
    fn new_msb(bv: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_msb(as_mut_ptr!(bv))) }
    }
    fn new_lsb(bv: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_lsb(as_mut_ptr!(bv))) }
    }
    fn new_is_zero(bv: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_is_zero(as_mut_ptr!(bv))) }
    }
    fn new_non_zero(bv: &RzILOpPure) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_is_zero(as_mut_ptr!(bv))) }
    }
    fn new_eq(x: &RzILOpBitVector, y: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_eq(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_ule(x: &RzILOpBitVector, y: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_ule(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_sle(x: &RzILOpBitVector, y: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_sle(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_ult(x: &RzILOpBitVector, y: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_ult(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_slt(x: &RzILOpBitVector, y: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_slt(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_uge(x: &RzILOpBitVector, y: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_uge(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_sge(x: &RzILOpBitVector, y: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_sge(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_ugt(x: &RzILOpBitVector, y: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_ugt(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_sgt(x: &RzILOpBitVector, y: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_sgt(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_sle(x: &RzILOpBitVector, y: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_sle(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_sle(x: &RzILOpBitVector, y: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_sle(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_sle(x: &RzILOpBitVector, y: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_sle(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_sle(x: &RzILOpBitVector, y: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_sle(as_mut_ptr!(x), as_mut_ptr!(y))) }
    }
    fn new_sle(x: &RzILOpBitVector, y: &RzILOpBitVector) -> Result<Self> {
        unsafe { Self::wrap(rz_il_op_new_sle(as_mut_ptr!(x), as_mut_ptr!(y))) }
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
//    fn new(x: &RzILOpBool, y: &RzILOpBool) -> Result<Self> {
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
