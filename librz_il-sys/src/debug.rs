use crate::bindings::*;
impl std::fmt::Debug for RzILOpPure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        unsafe {
            match self.code {
                RzILOpPureCode::RZ_IL_OP_VAR => self.op.var.fmt(f),
                RzILOpPureCode::RZ_IL_OP_ITE => self.op.ite.fmt(f),
                RzILOpPureCode::RZ_IL_OP_LET => self.op.let_.fmt(f),
                RzILOpPureCode::RZ_IL_OP_B0 => f
                    .debug_struct("RzILOpPure")
                    .field("code", &self.code)
                    .finish(),
                RzILOpPureCode::RZ_IL_OP_B1 => f
                    .debug_struct("RzILOpPure")
                    .field("code", &self.code)
                    .finish(),
                RzILOpPureCode::RZ_IL_OP_INV => self.op.boolinv.fmt(f),
                RzILOpPureCode::RZ_IL_OP_AND => self.op.booland.fmt(f),
                RzILOpPureCode::RZ_IL_OP_OR => self.op.boolor.fmt(f),
                RzILOpPureCode::RZ_IL_OP_XOR => self.op.boolxor.fmt(f),
                RzILOpPureCode::RZ_IL_OP_BITV => self.op.bitv.fmt(f),
                RzILOpPureCode::RZ_IL_OP_MSB => self.op.msb.fmt(f),
                RzILOpPureCode::RZ_IL_OP_LSB => self.op.lsb.fmt(f),
                RzILOpPureCode::RZ_IL_OP_IS_ZERO => self.op.is_zero.fmt(f),
                RzILOpPureCode::RZ_IL_OP_NEG => self.op.neg.fmt(f),
                RzILOpPureCode::RZ_IL_OP_LOGNOT => self.op.lognot.fmt(f),
                RzILOpPureCode::RZ_IL_OP_ADD => self.op.add.fmt(f),
                RzILOpPureCode::RZ_IL_OP_SUB => self.op.sub.fmt(f),
                RzILOpPureCode::RZ_IL_OP_MUL => self.op.mul.fmt(f),
                RzILOpPureCode::RZ_IL_OP_DIV => self.op.div.fmt(f),
                RzILOpPureCode::RZ_IL_OP_SDIV => self.op.sdiv.fmt(f),
                RzILOpPureCode::RZ_IL_OP_MOD => self.op.mod_.fmt(f),
                RzILOpPureCode::RZ_IL_OP_SMOD => self.op.smod.fmt(f),
                RzILOpPureCode::RZ_IL_OP_LOGAND => self.op.logand.fmt(f),
                RzILOpPureCode::RZ_IL_OP_LOGOR => self.op.logor.fmt(f),
                RzILOpPureCode::RZ_IL_OP_LOGXOR => self.op.logxor.fmt(f),
                RzILOpPureCode::RZ_IL_OP_SHIFTR => self.op.shiftr.fmt(f),
                RzILOpPureCode::RZ_IL_OP_SHIFTL => self.op.shiftl.fmt(f),
                RzILOpPureCode::RZ_IL_OP_EQ => self.op.eq.fmt(f),
                RzILOpPureCode::RZ_IL_OP_SLE => self.op.sle.fmt(f),
                RzILOpPureCode::RZ_IL_OP_ULE => self.op.ule.fmt(f),
                RzILOpPureCode::RZ_IL_OP_CAST => self.op.cast.fmt(f),
                RzILOpPureCode::RZ_IL_OP_APPEND => self.op.append.fmt(f),
                RzILOpPureCode::RZ_IL_OP_LOAD => self.op.load.fmt(f),
                RzILOpPureCode::RZ_IL_OP_LOADW => self.op.loadw.fmt(f),
                _ => panic!("Parse RzILOpPure Error"),
            }
        }
    }
}
