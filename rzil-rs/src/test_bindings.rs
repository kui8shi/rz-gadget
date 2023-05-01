#![deny(warnings)]
use librz_il_sys::*;

pub fn test_op_pure() -> *mut RzILOpPure {
    unsafe {
        let op: mut RzILOpPure = rz_il_op_new_b0();
        return op;
    }
}

fn main() {
    println!("{:?}", test_op_pure());
}
