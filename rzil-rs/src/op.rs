#![allow(non_upper_case_globals)]
#![allow(non_snake_case)]
use librz_il_sys::*;
use std::fmt;
use std::rc::Rc;
use std::cell::Cell;
pub trait OpArgs<T> {
    fn dummy() {}
}
impl OpArgs for RzILOpArgsEq {}

pub trait Op<T: OpArgs>: fmt::Debug {
    fn get_code(&self) -> OpCode;
    fn get_args(&self) -> T;
    unsafe fn wrap(code: OpCode, args: T) -> Self
    where
        Self: Sized;
}
struct Pure {
    code: OpCode,
    args: rz_il_op_pure_t__bindgen_ty_1,
}
/*
let op = rzil::op::Eq(bitv1, bitv2);
let op = rzil::op::Add(bitv1, bitv2)
let op = rzil::op::BoolAnd(bool1, bool2)
op = rzil::op::BoolAnd::from(op);
args = op.get_args(); // already typed
args.x
args.y
*/
macro_rules! abstruct {
//    ($(#[$attr:meta])* $c_name:ident, $cap_name:ident) => {
//        abs_type!($(#[$attr])* $c_name, $cap_name, PU);
//    };
    ($(#[$attr:meta])* $c_name:ident, $struct_name:ident, $free_fn:ident) => {
        $(#[$attr])*
        pub struct $struct_name<'a> {
            sys: *mut librz_il_sys::$c_name,
            rc: Rc<Cell<usize>>,
        }
        impl<'a> Clone for $struct_name<'a> {
            fn clone(&self) -> Self {
                self.rc.set(self.rc.get() + 1);
                $struct_name {
                    sys: self.sys,
                    rc: self.rc.clone(),
                }
            }
        }
        impl<'a> Drop for $struct_name<'a> {
            fn drop(&mut self) {
                let rc = self.rc.get();
                if rc == 1 {
                    unsafe {
                        $free_fn(self.sys);
                    }
                } else {
                    self.rc.set(rc - 1)
                }
            }
        }
    };
}
abstruct!(rz_il_op_pure_t, OpPure, rz_il_op_pure_free);
macro_rules! impl_op {
    ($(#[$attr:meta])* $op:ident ,$args:tt,$il_op:ident, $il_op_args:ident) => {
        $(#[$attr])*
        impl Op for $op {
            unsafe fn wrap(il_op: $il_op) -> Self
            where
                Self: Sized,
            {
                assert!(!il_op.is_null());
                Self {
                    code: il_op.code,
                    args: {
                        debug!("new op: id = {}, pointer = {:p}", il_op.code, il_op);
                        il_op.$args
                    },
                }
            }

            fn get_code(&self) -> OpCode {
                self.code
            }

            fn get_args(&self) -> $il_op_args {
                self.args
            }
        }
    };
}
impl_op!(Pure, )
pub enum OpCode {
    // pure
    Ite,
    Var,
    Let,
    BoolAnd,
    BoolOr,
    BoolXor,
    BoolInv,
    Bv,
    Msb,
    Lsb,
    IsZero,
    Eq_,
    Ule,
    Sle,
    Cast,
    Neg,
    LogNot,
    Add,
    Sub,
    Mul,
    Div,
    Sdiv,
    Smod,
    Mod,
    Logand,
    Logor,
    Logxor,
    ShiftLeft,
    ShiftRight,
    Append,
    Load,
    LoadW,
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
    FCastint,
    FCastsint,
    FCastfloat,
    FCastsfloat,
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
    Fmad,
    Fpow,
    Fpown,
    Frootn,
    Fcompound,
    Fhypot,
}

/*
#[cfg_attr(rustfmt,rustfmt_skip)]

enum Effect {
    // effect
    Set { v: String, is_local: bool, x: Rc<Pure> },
    Jmp { dst: i64},//BitVector },
    Goto { label: String },
    Seq {x: Rc<Effect>, y: Rc<Effect>},
    Blk {label: String, data_effect: Rc<Effect>, ctrl_effect: Rc<Effect> },
    Repeat {condition: Bool, data_effect: Rc<Effect>},
    Branch {condition: Bool, true_effect: Rc<Effect>, false_effect: Rc<Effect>},
    Store {mem: MemIndex, key: BitVector, value: BitVector},
    StoreW {},
}
struct MemIndex {
    x: i64,
}
struct BitVector {
    x: i64,
}
struct Bool {
    x: i64,
}
*/
