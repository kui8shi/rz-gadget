#include <rz-gadget.h>
#include <rz_core.h>
#include <z3.h>

struct rz_il_symbolic_vm_t;
typedef struct rz_il_symbolic_vm_t RzILSVM;
typedef Z3_ast (*OpPureSymbolicHandler)(RzILSVM *svm, RzILOpPure *op,
                                        RzILSortPure *sort);
typedef Z3_ast (*OpEffectSymbolicHandler)(RzILSVM *svm, RzILOpEffect *op,
                                          RzILTypeEffect *type);
struct rz_il_symbolic_vm_t {
  RzILVM vm;
  Z3_context ctx;
  Z3_solver slvr;
  OpPureSymbolicHandler *symbolic_op_handler_pure_table;
  OpEffectSymbolicHandler *symbolic_op_handler_effect_table;
};
typedef struct rz_il_symbolic_vm_t RzILSVM;

// Handler for core theory opcodes
void *rz_il_symbolic_handler_ite(RzILVM *vm, RzILOpPure *op,
                                 RzILTypePure *type);
void *rz_il_symbolic_handler_var(RzILVM *vm, RzILOpPure *op,
                                 RzILTypePure *type);
void *rz_il_symbolic_handler_let(RzILVM *vm, RzILOpPure *op,
                                 RzILTypePure *type);

void *rz_il_symbolic_handler_bitv(RzILVM *vm, RzILOpPure *op,
                                  RzILTypePure *type);
void *rz_il_symbolic_handler_msb(RzILVM *vm, RzILOpPure *op,
                                 RzILTypePure *type);
void *rz_il_symbolic_handler_lsb(RzILVM *vm, RzILOpPure *op,
                                 RzILTypePure *type);
void *rz_il_symbolic_handler_is_zero(RzILVM *vm, RzILOpPure *op,
                                     RzILTypePure *type);
void *rz_il_symbolic_handler_eq(RzILVM *vm, RzILOpPure *op, RzILTypePure *type);
void *rz_il_symbolic_handler_ule(RzILVM *vm, RzILOpPure *op,
                                 RzILTypePure *type);
void *rz_il_symbolic_handler_sle(RzILVM *vm, RzILOpPure *op,
                                 RzILTypePure *type);
void *rz_il_symbolic_handler_neg(RzILVM *vm, RzILOpPure *op,
                                 RzILTypePure *type);
void *rz_il_symbolic_handler_logical_not(RzILVM *vm, RzILOpPure *op,
                                         RzILTypePure *type);
void *rz_il_symbolic_handler_add(RzILVM *vm, RzILOpPure *op,
                                 RzILTypePure *type);
void *rz_il_symbolic_handler_sub(RzILVM *vm, RzILOpPure *op,
                                 RzILTypePure *type);
void *rz_il_symbolic_handler_mul(RzILVM *vm, RzILOpPure *op,
                                 RzILTypePure *type);
void *rz_il_symbolic_handler_div(RzILVM *vm, RzILOpPure *op,
                                 RzILTypePure *type);
void *rz_il_symbolic_handler_sdiv(RzILVM *vm, RzILOpPure *op,
                                  RzILTypePure *type);
void *rz_il_symbolic_handler_mod(RzILVM *vm, RzILOpPure *op,
                                 RzILTypePure *type);
void *rz_il_symbolic_handler_smod(RzILVM *vm, RzILOpPure *op,
                                  RzILTypePure *type);
void *rz_il_symbolic_handler_shiftl(RzILVM *vm, RzILOpPure *op,
                                    RzILTypePure *type);
void *rz_il_symbolic_handler_shiftr(RzILVM *vm, RzILOpPure *op,
                                    RzILTypePure *type);
void *rz_il_symbolic_handler_logical_and(RzILVM *vm, RzILOpPure *op,
                                         RzILTypePure *type);
void *rz_il_symbolic_handler_logical_or(RzILVM *vm, RzILOpPure *op,
                                        RzILTypePure *type);
void *rz_il_symbolic_handler_logical_xor(RzILVM *vm, RzILOpPure *op,
                                         RzILTypePure *type);

void *rz_il_symbolic_handler_bool_false(RzILVM *vm, RzILOpPure *op,
                                        RzILTypePure *type);
void *rz_il_symbolic_handler_bool_true(RzILVM *vm, RzILOpPure *op,
                                       RzILTypePure *type);
void *rz_il_symbolic_handler_bool_and(RzILVM *vm, RzILOpPure *op,
                                      RzILTypePure *type);
void *rz_il_symbolic_handler_bool_or(RzILVM *vm, RzILOpPure *op,
                                     RzILTypePure *type);
void *rz_il_symbolic_handler_bool_xor(RzILVM *vm, RzILOpPure *op,
                                      RzILTypePure *type);
void *rz_il_symbolic_handler_bool_inv(RzILVM *vm, RzILOpPure *op,
                                      RzILTypePure *type);

void *rz_il_symbolic_handler_cast(RzILVM *vm, RzILOpPure *op,
                                  RzILTypePure *type);
void *rz_il_symbolic_handler_append(RzILVM *vm, RzILOpPure *op,
                                    RzILTypePure *type);

bool rz_il_symbolic_handler_empty(RzILVM *vm, RzILOpEffect *op);
bool rz_il_symbolic_handler_nop(RzILVM *vm, RzILOpEffect *op);
bool rz_il_symbolic_handler_set(RzILVM *vm, RzILOpEffect *op);
bool rz_il_symbolic_handler_jmp(RzILVM *vm, RzILOpEffect *op);
bool rz_il_symbolic_handler_goto(RzILVM *vm, RzILOpEffect *op);
bool rz_il_symbolic_handler_seq(RzILVM *vm, RzILOpEffect *op);
bool rz_il_symbolic_handler_blk(RzILVM *vm, RzILOpEffect *op);
bool rz_il_symbolic_handler_repeat(RzILVM *vm, RzILOpEffect *op);
bool rz_il_symbolic_handler_branch(RzILVM *vm, RzILOpEffect *op);

void *rz_il_symbolic_handler_load(RzILVM *vm, RzILOpPure *op,
                                  RzILTypePure *type);
bool rz_il_symbolic_handler_store(RzILVM *vm, RzILOpEffect *op);
void *rz_il_symbolic_handler_loadw(RzILVM *vm, RzILOpPure *op,
                                   RzILTypePure *type);
bool rz_il_symbolic_handler_storew(RzILVM *vm, RzILOpEffect *op);

void *rz_il_symbolic_handler_float(RzILVM *vm, RzILOpPure *op,
                                   RzILTypePure *type);
void *rz_il_symbolic_handler_fbits(RzILVM *vm, RzILOpPure *op,
                                   RzILTypePure *type);
void *rz_il_symbolic_handler_is_finite(RzILVM *vm, RzILOpPure *op,
                                       RzILTypePure *type);
void *rz_il_symbolic_handler_is_nan(RzILVM *vm, RzILOpPure *op,
                                    RzILTypePure *type);
void *rz_il_symbolic_handler_is_inf(RzILVM *vm, RzILOpPure *op,
                                    RzILTypePure *type);
void *rz_il_symbolic_handler_is_fzero(RzILVM *vm, RzILOpPure *op,
                                      RzILTypePure *type);
void *rz_il_symbolic_handler_is_fneg(RzILVM *vm, RzILOpPure *op,
                                     RzILTypePure *type);
void *rz_il_symbolic_handler_is_fpos(RzILVM *vm, RzILOpPure *op,
                                     RzILTypePure *type);
void *rz_il_symbolic_handler_fneg(RzILVM *vm, RzILOpPure *op,
                                  RzILTypePure *type);
void *rz_il_symbolic_handler_fabs(RzILVM *vm, RzILOpPure *op,
                                  RzILTypePure *type);
void *rz_il_symbolic_handler_fcast_int(RzILVM *vm, RzILOpPure *op,
                                       RzILTypePure *type);
void *rz_il_symbolic_handler_fcast_sint(RzILVM *vm, RzILOpPure *op,
                                        RzILTypePure *type);
void *rz_il_symbolic_handler_fcast_float(RzILVM *vm, RzILOpPure *op,
                                         RzILTypePure *type);
void *rz_il_symbolic_handler_fcast_sfloat(RzILVM *vm, RzILOpPure *op,
                                          RzILTypePure *type);
void *rz_il_symbolic_handler_fconvert(RzILVM *vm, RzILOpPure *op,
                                      RzILTypePure *type);
void *rz_il_symbolic_handler_frequal(RzILVM *vm, RzILOpPure *op,
                                     RzILTypePure *type);
void *rz_il_symbolic_handler_fsucc(RzILVM *vm, RzILOpPure *op,
                                   RzILTypePure *type);
void *rz_il_symbolic_handler_fpred(RzILVM *vm, RzILOpPure *op,
                                   RzILTypePure *type);
void *rz_il_symbolic_handler_forder(RzILVM *vm, RzILOpPure *op,
                                    RzILTypePure *type);
void *rz_il_symbolic_handler_fround(RzILVM *vm, RzILOpPure *op,
                                    RzILTypePure *type);
void *rz_il_symbolic_handler_fsqrt(RzILVM *vm, RzILOpPure *op,
                                   RzILTypePure *type);
void *rz_il_symbolic_handler_frsqrt(RzILVM *vm, RzILOpPure *op,
                                    RzILTypePure *type);
void *rz_il_symbolic_handler_fadd(RzILVM *vm, RzILOpPure *op,
                                  RzILTypePure *type);
void *rz_il_symbolic_handler_fsub(RzILVM *vm, RzILOpPure *op,
                                  RzILTypePure *type);
void *rz_il_symbolic_handler_fdiv(RzILVM *vm, RzILOpPure *op,
                                  RzILTypePure *type);
void *rz_il_symbolic_handler_fmul(RzILVM *vm, RzILOpPure *op,
                                  RzILTypePure *type);
void *rz_il_symbolic_handler_fmod(RzILVM *vm, RzILOpPure *op,
                                  RzILTypePure *type);
void *rz_il_symbolic_handler_fhypot(RzILVM *vm, RzILOpPure *op,
                                    RzILTypePure *type);
void *rz_il_symbolic_handler_fpow(RzILVM *vm, RzILOpPure *op,
                                  RzILTypePure *type);
void *rz_il_symbolic_handler_fmad(RzILVM *vm, RzILOpPure *op,
                                  RzILTypePure *type);
void *rz_il_symbolic_handler_frootn(RzILVM *vm, RzILOpPure *op,
                                    RzILTypePure *type);
void *rz_il_symbolic_handler_fpown(RzILVM *vm, RzILOpPure *op,
                                   RzILTypePure *type);
void *rz_il_symbolic_handler_fcompound(RzILVM *vm, RzILOpPure *op,
                                       RzILTypePure *type);

// TODO: remove me when all the handlers are implemented
void *rz_il_symbolic_handler_pure_unimplemented(RzILVM *vm, RzILOpPure *op,
                                                RzILTypePure *type);
bool rz_il_symbolic_handler_effect_unimplemented(RzILVM *vm, RzILOpEffect *op);

RZ_IPI RzILOpPureHandler
    rz_il_op_handler_pure_table_default[RZ_IL_OP_PURE_MAX] = {
        [RZ_IL_OP_VAR] = rz_il_symbolic_handler_var,
        [RZ_IL_OP_ITE] = rz_il_symbolic_handler_ite,
        [RZ_IL_OP_LET] = rz_il_symbolic_handler_let,
        [RZ_IL_OP_B0] = rz_il_symbolic_handler_bool_false,
        [RZ_IL_OP_B1] = rz_il_symbolic_handler_bool_true,
        [RZ_IL_OP_INV] = rz_il_symbolic_handler_bool_inv,
        [RZ_IL_OP_AND] = rz_il_symbolic_handler_bool_and,
        [RZ_IL_OP_OR] = rz_il_symbolic_handler_bool_or,
        [RZ_IL_OP_XOR] = rz_il_symbolic_handler_bool_xor,
        [RZ_IL_OP_BITV] = rz_il_symbolic_handler_bitv,
        [RZ_IL_OP_MSB] = rz_il_symbolic_handler_msb,
        [RZ_IL_OP_LSB] = rz_il_symbolic_handler_lsb,
        [RZ_IL_OP_IS_ZERO] = rz_il_symbolic_handler_is_zero,
        [RZ_IL_OP_NEG] = rz_il_symbolic_handler_neg,
        [RZ_IL_OP_LOGNOT] = rz_il_symbolic_handler_logical_not,
        [RZ_IL_OP_ADD] = rz_il_symbolic_handler_add,
        [RZ_IL_OP_SUB] = rz_il_symbolic_handler_sub,
        [RZ_IL_OP_MUL] = rz_il_symbolic_handler_mul,
        [RZ_IL_OP_DIV] = rz_il_symbolic_handler_div,
        [RZ_IL_OP_MOD] = rz_il_symbolic_handler_mod,
        [RZ_IL_OP_SDIV] = rz_il_symbolic_handler_sdiv,
        [RZ_IL_OP_SMOD] = rz_il_symbolic_handler_smod,
        [RZ_IL_OP_LOGAND] = rz_il_symbolic_handler_logical_and,
        [RZ_IL_OP_LOGOR] = rz_il_symbolic_handler_logical_or,
        [RZ_IL_OP_LOGXOR] = rz_il_symbolic_handler_logical_xor,
        [RZ_IL_OP_SHIFTR] = rz_il_symbolic_handler_shiftr,
        [RZ_IL_OP_SHIFTL] = rz_il_symbolic_handler_shiftl,
        [RZ_IL_OP_EQ] = rz_il_symbolic_handler_eq,
        [RZ_IL_OP_SLE] = rz_il_symbolic_handler_sle,
        [RZ_IL_OP_ULE] = rz_il_symbolic_handler_ule,
        [RZ_IL_OP_CAST] = rz_il_symbolic_handler_cast,
        [RZ_IL_OP_APPEND] = rz_il_symbolic_handler_append,
        [RZ_IL_OP_LOAD] = rz_il_symbolic_handler_load,
        [RZ_IL_OP_LOADW] = rz_il_symbolic_handler_loadw,

        // Fbasic Theory
        [RZ_IL_OP_FLOAT] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FBITS] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_IS_FINITE] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_IS_NAN] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_IS_INF] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_IS_FZERO] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_IS_FNEG] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_IS_FPOS] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FNEG] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FABS] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FREQUAL] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FSUCC] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FPRED] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FORDER] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FROUND] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FSQRT] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FRSQRT] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FADD] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FSUB] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FMUL] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FDIV] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FMOD] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FMAD] = rz_il_symbolic_handler_pure_unimplemented,

        [RZ_IL_OP_FCAST_INT] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FCAST_SINT] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FCAST_FLOAT] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FCAST_SFLOAT] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FCONVERT] = rz_il_symbolic_handler_pure_unimplemented,

        // Float Theory
        // TODO : Implement other Float Theory operations
        [RZ_IL_OP_FHYPOT] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FPOW] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FROOTN] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FPOWN] = rz_il_symbolic_handler_pure_unimplemented,
        [RZ_IL_OP_FCOMPOUND] = rz_il_symbolic_handler_pure_unimplemented,
};

RZ_IPI RzILOpEffectSymbolicHandler
    symbolic_op_handler_effect_table_default[RZ_IL_OP_EFFECT_MAX] = {
        [RZ_IL_OP_EMPTY] = rz_il_symbolic_handler_empty,
        [RZ_IL_OP_STORE] = rz_il_symbolic_handler_store,
        [RZ_IL_OP_STOREW] = rz_il_symbolic_handler_storew,
        [RZ_IL_OP_NOP] = rz_il_symbolic_handler_nop,
        [RZ_IL_OP_SET] = rz_il_symbolic_handler_set,
        [RZ_IL_OP_JMP] = rz_il_symbolic_handler_jmp,
        [RZ_IL_OP_GOTO] = rz_il_symbolic_handler_goto,
        [RZ_IL_OP_SEQ] = rz_il_symbolic_handler_seq,
        [RZ_IL_OP_BLK] = rz_il_symbolic_handler_blk,
        [RZ_IL_OP_REPEAT] = rz_il_symbolic_handler_repeat,
        [RZ_IL_OP_BRANCH] = rz_il_symbolic_handler_branch,
};
