#include "rz_analysis.h"
#include "rz_il/rz_il_opcodes.h"
#include <rz-gadget.h>
#include <rz_core.h>
#include <rz_il_opbuilder_begin.h>
#include <z3.h>

static const RzCmdDescArg cmd_rop_args[] = {
    {
        .name = "Flag or Address of basic block to translate to SMT-LIBv2",
        .type = RZ_CMD_ARG_TYPE_FLAG,
    },
    {0},
};

static const RzCmdDescHelp rop_usage = {
    .summary = "Rizin Extra Plugin to automate building ROP chain",
    .args = cmd_rop_args,
};

RZ_IPI RzCmdStatus rz_cmd_rop_handler(RzCore *core, int argc,
                                      const char **argv) {
  if (argc < 2) {
    return RZ_CMD_STATUS_WRONG_ARGS;
  }
  if (!rop_smtlib(core, argv[1])) {
    return RZ_CMD_STATUS_ERROR;
  }
  return RZ_CMD_STATUS_OK;
}

static bool rz_cmd_rop_init(RzCore *core) {
  RzCmd *rcmd = core->rcmd;
  RzCmdDesc *root_cd = rz_cmd_get_root(rcmd);
  if (!root_cd) {
    rz_warn_if_reached();
    return false;
  }

  RzCmdDesc *rop = rz_cmd_desc_argv_new(rcmd, root_cd, "rop",
                                        rz_cmd_rop_handler, &rop_usage);
  if (!rop) {
    rz_warn_if_reached();
    return false;
  }

  return true;
}

RzCorePlugin rz_core_plugin_rop = {
    .name = "rz-gadget",
    .desc = "Rizin Extra Plugin to automate building ROP chain",
    .license = "LGPL3",
    .version = "0.1.0",
    .author = "ez2take",
    .init = rz_cmd_rop_init,
};

#ifndef CORELIB
RZ_API RzLibStruct rizin_plugin = {
    .type = RZ_LIB_TYPE_CORE,
    .data = &rz_core_plugin_rop,
    .version = RZ_VERSION,
};
#endif

bool rop_smtlib(RzCore *core, const char *expr) {
  printf("[rop_smtlib]\n");
  ut64 addr = rz_num_math(core->num, expr);
  if (core->num->nc.errors) {
    RZ_LOG_ERROR("unknown address '%s'\n", core->num->nc.calc_buf);
    return false;
  }
  //  RzAnalysisBlock *block = rz_analysis_get_block_at(core->analysis, addr);
  RzAnalysisOp *op = rz_core_analysis_op(core, addr, RZ_ANALYSIS_OP_MASK_ALL);
  if (!op) {
    RZ_LOG_ERROR("no op at 0x%llx\n", addr);
    return false;
  }
  //  printf("basic block at 0x%llx ... %s\n", addr, block->op_bytes);
  RzStrBuf *buf = rz_strbuf_new("");
  rz_il_op_effect_stringify(op->il_op, buf);
  printf("op at 0x%llx ... \n\tmnemonic:%s\n\tRzIL:\n%s\n", addr, op->mnemonic,
         rz_strbuf_get(buf));
  return true;
}

// typedef bool (*OpPureToZ3FuncPtr)(RzILOpPure *op, Z3_context ctx, Z3_solver
// typedef bool (*OpEffectToZ3FuncPtr)(RzILOpEffect *op, Z3_context ctx,
// Z3_solver s);
HtUP /* RzILOpPureCode, OpPureToZ3FuncPtr*/ *trans_table_pure;
HtUP /* RzILOpEffectCode, OpEffectToZ3FuncPtr*/ *trans_table_effect;

//   RZ_IL_OP_VAR
bool add_z3_var(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_ITE
bool add_z3_ite(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_LET
bool add_z3_let(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_B0
bool add_z3_bool_false(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_B1
bool add_z3_bool_true(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_INV
bool add_z3_bool_inv(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_AND
bool add_z3_bool_and(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_OR
bool add_z3_bool_or(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_XOR
bool add_z3_bool_xor(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_BITV
bool add_z3_bitv(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_MSB
bool add_z3_msb(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_LSB
bool add_z3_lsb(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_IS_ZERO
bool add_z3_is_zero(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_NEG
bool add_z3_neg(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_LOGNOT
bool add_z3_lognot(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_ADD
bool add_z3_add(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_SUB
bool add_z3_sub(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_MUL
bool add_z3_mul(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_DIV
bool add_z3_div(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_SDIV
bool add_z3_sdiv(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_MOD
bool add_z3_mod(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_SMOD
bool add_z3_smod(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_LOGAND
bool add_z3_logand(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_LOGOR
bool add_z3_logor(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_LOGXOR
bool add_z3_logxor(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_SHIFTR
bool add_z3_shiftr(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_SHIFTL
bool add_z3_shiftl(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_EQ
bool add_z3_eq(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_SLE
bool add_z3_sle(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_ULE
bool add_z3_ule(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_CAST
bool add_z3_cast(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_APPEND
bool add_z3_append(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_LOAD
bool add_z3_load(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_LOADW
bool add_z3_loadw(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FLOAT
bool add_z3_float(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FBITS
bool add_z3_fbits(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_IS_FINITE
bool add_z3_is_finite(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_IS_NAN
bool add_z3_is_nan(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_IS_INF
bool add_z3_is_inf(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_IS_FZERO
bool add_z3_is_fzero(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_IS_FNEG
bool add_z3_is_fneg(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_IS_FPOS
bool add_z3_is_fpos(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FNEG
bool add_z3_fneg(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FABS
bool add_z3_fabs(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FCAST_INT
bool add_z3_fcast_int(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FCAST_SINT
bool add_z3_fcast_sint(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FCAST_FLOAT
bool add_z3_fcast_float(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FCAST_SFLOAT
bool add_z3_fcast_sfloat(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FCONVERT
bool add_z3_fconvert(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FREQUAL
bool add_z3_frequal(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FSUCC
bool add_z3_fsucc(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FPRED
bool add_z3_fpred(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FORDER
bool add_z3_forder(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FROUND
bool add_z3_fround(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FSQRT
bool add_z3_fsqrt(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FRSQRT
bool add_z3_frsqrt(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FADD
bool add_z3_fadd(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FSUB
bool add_z3_fsub(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FMUL
bool add_z3_fmul(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FDIV
bool add_z3_fdiv(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FMOD
bool add_z3_fmod(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FHYPOT
bool add_z3_fhypot(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FPOW
bool add_z3_fpow(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FMAD
bool add_z3_fmad(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FPOWN
bool add_z3_fpown(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FROOTN
bool add_z3_frootn(RzILOpPure *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_FCOMPOUND
bool add_z3_fcompound(RzILOpPure *op, Z3_context ctx, Z3_solver s);

//   RZ_IL_OP_EMPTY
bool add_z3_empty(RzILOpEffect *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_STORE
bool add_z3_store(RzILOpEffect *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_STOREW
bool add_z3_storew(RzILOpEffect *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_NOP
bool add_z3_nop(RzILOpEffect *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_SET
bool add_z3_set(RzILOpEffect *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_JMP
bool add_z3_jmp(RzILOpEffect *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_GOTO
bool add_z3_goto(RzILOpEffect *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_SEQ
bool add_z3_seq(RzILOpEffect *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_BLK
bool add_z3_blk(RzILOpEffect *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_REPEAT
bool add_z3_repeat(RzILOpEffect *op, Z3_context ctx, Z3_solver s);
//   RZ_IL_OP_BRANCH
bool add_z3_branch(RzILOpEffect *op, Z3_context ctx, Z3_solver s);

void init_translate_tables() {
  trans_table_pure = ht_up_new(NULL, NULL, NULL);
  trans_table_effect = ht_up_new(NULL, NULL, NULL);

  /* initialize translate_pure_table */
  ht_up_insert(trans_table_pure, RZ_IL_OP_VAR, add_z3_var);
  ht_up_insert(trans_table_pure, RZ_IL_OP_VAR, add_z3_var);
  ht_up_insert(trans_table_pure, RZ_IL_OP_ITE, add_z3_ite);
  ht_up_insert(trans_table_pure, RZ_IL_OP_LET, add_z3_let);
  ht_up_insert(trans_table_pure, RZ_IL_OP_B0, add_z3_bool_false);
  ht_up_insert(trans_table_pure, RZ_IL_OP_B1, add_z3_bool_true);
  ht_up_insert(trans_table_pure, RZ_IL_OP_INV, add_z3_bool_inv);
  ht_up_insert(trans_table_pure, RZ_IL_OP_AND, add_z3_bool_and);
  ht_up_insert(trans_table_pure, RZ_IL_OP_OR, add_z3_bool_or);
  ht_up_insert(trans_table_pure, RZ_IL_OP_XOR, add_z3_bool_xor);
  ht_up_insert(trans_table_pure, RZ_IL_OP_BITV, add_z3_bitv);
  ht_up_insert(trans_table_pure, RZ_IL_OP_MSB, add_z3_msb);
  ht_up_insert(trans_table_pure, RZ_IL_OP_LSB, add_z3_lsb);
  ht_up_insert(trans_table_pure, RZ_IL_OP_IS_ZERO, add_z3_is_zero);
  ht_up_insert(trans_table_pure, RZ_IL_OP_NEG, add_z3_neg);
  ht_up_insert(trans_table_pure, RZ_IL_OP_LOGNOT, add_z3_lognot);
  ht_up_insert(trans_table_pure, RZ_IL_OP_ADD, add_z3_add);
  ht_up_insert(trans_table_pure, RZ_IL_OP_SUB, add_z3_sub);
  ht_up_insert(trans_table_pure, RZ_IL_OP_MUL, add_z3_mul);
  ht_up_insert(trans_table_pure, RZ_IL_OP_DIV, add_z3_div);
  ht_up_insert(trans_table_pure, RZ_IL_OP_SDIV, add_z3_sdiv);
  ht_up_insert(trans_table_pure, RZ_IL_OP_MOD, add_z3_mod);
  ht_up_insert(trans_table_pure, RZ_IL_OP_SMOD, add_z3_smod);
  ht_up_insert(trans_table_pure, RZ_IL_OP_LOGAND, add_z3_logand);
  ht_up_insert(trans_table_pure, RZ_IL_OP_LOGOR, add_z3_logor);
  ht_up_insert(trans_table_pure, RZ_IL_OP_LOGXOR, add_z3_logxor);
  ht_up_insert(trans_table_pure, RZ_IL_OP_SHIFTR, add_z3_shiftr);
  ht_up_insert(trans_table_pure, RZ_IL_OP_SHIFTL, add_z3_shiftl);
  ht_up_insert(trans_table_pure, RZ_IL_OP_EQ, add_z3_eq);
  ht_up_insert(trans_table_pure, RZ_IL_OP_SLE, add_z3_sle);
  ht_up_insert(trans_table_pure, RZ_IL_OP_ULE, add_z3_ule);
  ht_up_insert(trans_table_pure, RZ_IL_OP_CAST, add_z3_cast);
  ht_up_insert(trans_table_pure, RZ_IL_OP_APPEND, add_z3_append);
  ht_up_insert(trans_table_pure, RZ_IL_OP_LOAD, add_z3_load);
  ht_up_insert(trans_table_pure, RZ_IL_OP_LOADW, add_z3_loadw);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FLOAT, add_z3_float);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FBITS, add_z3_fbits);
  ht_up_insert(trans_table_pure, RZ_IL_OP_IS_FINITE, add_z3_is_finite);
  ht_up_insert(trans_table_pure, RZ_IL_OP_IS_NAN, add_z3_is_nan);
  ht_up_insert(trans_table_pure, RZ_IL_OP_IS_INF, add_z3_is_inf);
  ht_up_insert(trans_table_pure, RZ_IL_OP_IS_FZERO, add_z3_is_fzero);
  ht_up_insert(trans_table_pure, RZ_IL_OP_IS_FNEG, add_z3_is_fneg);
  ht_up_insert(trans_table_pure, RZ_IL_OP_IS_FPOS, add_z3_is_fpos);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FNEG, add_z3_fneg);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FABS, add_z3_fabs);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FCAST_INT, add_z3_fcast_int);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FCAST_SINT, add_z3_fcast_sint);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FCAST_FLOAT, add_z3_fcast_float);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FCAST_SFLOAT, add_z3_fcast_sfloat);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FCONVERT, add_z3_fconvert);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FREQUAL, add_z3_frequal);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FSUCC, add_z3_fsucc);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FPRED, add_z3_fpred);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FORDER, add_z3_forder);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FROUND, add_z3_fround);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FSQRT, add_z3_fsqrt);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FRSQRT, add_z3_frsqrt);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FADD, add_z3_fadd);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FSUB, add_z3_fsub);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FMUL, add_z3_fmul);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FDIV, add_z3_fdiv);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FMOD, add_z3_fmod);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FHYPOT, add_z3_fhypot);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FPOW, add_z3_fpow);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FMAD, add_z3_fmad);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FPOWN, add_z3_fpown);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FROOTN, add_z3_frootn);
  ht_up_insert(trans_table_pure, RZ_IL_OP_FCOMPOUND, add_z3_fcompound);

  /* initialize translate_effect_table */
  ht_up_insert(trans_table_effect, RZ_IL_OP_EMPTY, add_z3_empty);
  ht_up_insert(trans_table_effect, RZ_IL_OP_STORE, add_z3_store);
  ht_up_insert(trans_table_effect, RZ_IL_OP_STOREW, add_z3_storew);
  ht_up_insert(trans_table_effect, RZ_IL_OP_NOP, add_z3_nop);
  ht_up_insert(trans_table_effect, RZ_IL_OP_SET, add_z3_set);
  ht_up_insert(trans_table_effect, RZ_IL_OP_JMP, add_z3_jmp);
  ht_up_insert(trans_table_effect, RZ_IL_OP_GOTO, add_z3_goto);
  ht_up_insert(trans_table_effect, RZ_IL_OP_SEQ, add_z3_seq);
  ht_up_insert(trans_table_effect, RZ_IL_OP_BLK, add_z3_blk);
  ht_up_insert(trans_table_effect, RZ_IL_OP_REPEAT, add_z3_repeat);
  ht_up_insert(trans_table_effect, RZ_IL_OP_BRANCH, add_z3_branch);
}

//   RZ_IL_OP_VAR
bool add_z3_var(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_ITE
bool add_z3_ite(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_LET
bool add_z3_let(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_B0
bool add_z3_bool_false(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  Z3_mk_false(ctx);
  Z3_solver_ return true;
}
//   RZ_IL_OP_B1
bool add_z3_bool_true(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_INV
bool add_z3_bool_inv(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_AND
bool add_z3_bool_and(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_OR
bool add_z3_bool_or(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_XOR
bool add_z3_bool_xor(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_BITV
bool add_z3_bitv(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_MSB
bool add_z3_msb(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_LSB
bool add_z3_lsb(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_IS_ZERO
bool add_z3_is_zero(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_NEG
bool add_z3_neg(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_LOGNOT
bool add_z3_lognot(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_ADD
bool add_z3_add(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_SUB
bool add_z3_sub(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_MUL
bool add_z3_mul(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_DIV
bool add_z3_div(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_SDIV
bool add_z3_sdiv(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_MOD
bool add_z3_mod(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_SMOD
bool add_z3_smod(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_LOGAND
bool add_z3_logand(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_LOGOR
bool add_z3_logor(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_LOGXOR
bool add_z3_logxor(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_SHIFTR
bool add_z3_shiftr(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_SHIFTL
bool add_z3_shiftl(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_EQ
bool add_z3_eq(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_SLE
bool add_z3_sle(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_ULE
bool add_z3_ule(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_CAST
bool add_z3_cast(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_APPEND
bool add_z3_append(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_LOAD
bool add_z3_load(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_LOADW
bool add_z3_loadw(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FLOAT
bool add_z3_float(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FBITS
bool add_z3_fbits(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_IS_FINITE
bool add_z3_is_finite(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_IS_NAN
bool add_z3_is_nan(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_IS_INF
bool add_z3_is_inf(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_IS_FZERO
bool add_z3_is_fzero(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_IS_FNEG
bool add_z3_is_fneg(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_IS_FPOS
bool add_z3_is_fpos(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_FNEG
bool add_z3_fneg(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FABS
bool add_z3_fabs(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FCAST_INT
bool add_z3_fcast_int(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_FCAST_SINT
bool add_z3_fcast_sint(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_FCAST_FLOAT
bool add_z3_fcast_float(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_FCAST_SFLOAT
bool add_z3_fcast_sfloat(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_FCONVERT
bool add_z3_fconvert(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_FREQUAL
bool add_z3_frequal(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_FSUCC
bool add_z3_fsucc(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FPRED
bool add_z3_fpred(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FORDER
bool add_z3_forder(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FROUND
bool add_z3_fround(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FSQRT
bool add_z3_fsqrt(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FRSQRT
bool add_z3_frsqrt(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FADD
bool add_z3_fadd(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FSUB
bool add_z3_fsub(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FMUL
bool add_z3_fmul(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FDIV
bool add_z3_fdiv(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FMOD
bool add_z3_fmod(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FHYPOT
bool add_z3_fhypot(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FPOW
bool add_z3_fpow(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FMAD
bool add_z3_fmad(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FPOWN
bool add_z3_fpown(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FROOTN
bool add_z3_frootn(RzILOpPure *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_FCOMPOUND
bool add_z3_fcompound(RzILOpPure *op, Z3_context ctx, Z3_solver s) {
  return true;
}

//   RZ_IL_OP_EMPTY
bool add_z3_empty(RzILOpEffect *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_STORE
bool add_z3_store(RzILOpEffect *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_STOREW
bool add_z3_storew(RzILOpEffect *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_NOP
bool add_z3_nop(RzILOpEffect *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_SET
bool add_z3_set(RzILOpEffect *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_JMP
bool add_z3_jmp(RzILOpEffect *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_GOTO
bool add_z3_goto(RzILOpEffect *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_SEQ
bool add_z3_seq(RzILOpEffect *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_BLK
bool add_z3_blk(RzILOpEffect *op, Z3_context ctx, Z3_solver s) { return true; }
//   RZ_IL_OP_REPEAT
bool add_z3_repeat(RzILOpEffect *op, Z3_context ctx, Z3_solver s) {
  return true;
}
//   RZ_IL_OP_BRANCH
bool add_z3_branch(RzILOpEffect *op, Z3_context ctx, Z3_solver s) {
  return true;
}
IL_TRUE;
