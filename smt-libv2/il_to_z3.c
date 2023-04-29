#include "rz_util/rz_assert.h"
#include <rz-gadget.h>

// Handler for symbolic evaluation
void *symbolic_handler_ite(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_var(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_let(RzILSVM *svm, RzILOpPure *op);

void *symbolic_handler_bitv(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_msb(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_lsb(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_is_zero(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_ule(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_sle(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_neg(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_logical_not(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_add(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_sub(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_mul(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_div(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_sdiv(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_mod(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_smod(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_shiftl(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_shiftr(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_eq(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_logical_and(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_logical_or(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_logical_xor(RzILSVM *svm, RzILOpPure *op);

void *symbolic_handler_bool_false(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_bool_true(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_bool_and(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_bool_or(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_bool_xor(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_bool_inv(RzILSVM *svm, RzILOpPure *op);

void *symbolic_handler_cast(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_append(RzILSVM *svm, RzILOpPure *op);

void *symbolic_handler_empty(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_nop(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_set(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_jmp(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_goto(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_seq(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_blk(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_repeat(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_branch(RzILSVM *svm, RzILOpEffect *op);

void *symbolic_handler_load(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_store(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_loadw(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_storew(RzILSVM *svm, RzILOpEffect *op);

void *symbolic_handler_float(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fbits(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_is_finite(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_is_nan(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_is_inf(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_is_fzero(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_is_fneg(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_is_fpos(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fneg(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fabs(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fcast_int(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fcast_sint(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fcast_float(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fcast_sfloat(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fconvert(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_frequal(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fsucc(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fpred(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_forder(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fround(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fsqrt(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_frsqrt(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fadd(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fsub(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fdiv(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fmul(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fmod(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fhypot(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fpow(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fmad(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_frootn(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fpown(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_fcompound(RzILSVM *svm, RzILOpPure *op);

// TODO: remove me when all the handlers are implemented
void *symbolic_handler_pure_unimplemented(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_effect_unimplemented(RzILSVM *svm, RzILOpEffect *op);

OpPureSymbolicHandler
	symbolic_op_handler_pure_table_default[RZ_IL_OP_PURE_MAX] = {
		[RZ_IL_OP_VAR] = symbolic_handler_pure_unimplemented, // symbolic_handler_var,
		[RZ_IL_OP_ITE] = symbolic_handler_pure_unimplemented, // symbolic_handler_ite,
		[RZ_IL_OP_LET] = symbolic_handler_pure_unimplemented, // symbolic_handler_let,
		[RZ_IL_OP_B0] = symbolic_handler_pure_unimplemented, // symbolic_handler_bool_false,
		[RZ_IL_OP_B1] = symbolic_handler_pure_unimplemented, // symbolic_handler_bool_true,
		[RZ_IL_OP_INV] = symbolic_handler_pure_unimplemented, // symbolic_handler_bool_inv,
		[RZ_IL_OP_AND] = symbolic_handler_pure_unimplemented, // symbolic_handler_bool_and,
		[RZ_IL_OP_OR] = symbolic_handler_pure_unimplemented, // symbolic_handler_bool_or,
		[RZ_IL_OP_XOR] = symbolic_handler_pure_unimplemented, // symbolic_handler_bool_xor,
		[RZ_IL_OP_BITV] = symbolic_handler_pure_unimplemented, // symbolic_handler_bitv,
		[RZ_IL_OP_MSB] = symbolic_handler_pure_unimplemented, // symbolic_handler_msb,
		[RZ_IL_OP_LSB] = symbolic_handler_pure_unimplemented, // symbolic_handler_lsb,
		[RZ_IL_OP_IS_ZERO] = symbolic_handler_pure_unimplemented, // symbolic_handler_is_zero,
		[RZ_IL_OP_NEG] = symbolic_handler_pure_unimplemented, // symbolic_handler_neg,
		[RZ_IL_OP_LOGNOT] = symbolic_handler_pure_unimplemented, // symbolic_handler_logical_not,
		[RZ_IL_OP_ADD] = symbolic_handler_pure_unimplemented, // symbolic_handler_add,
		[RZ_IL_OP_SUB] = symbolic_handler_pure_unimplemented, // symbolic_handler_sub,
		[RZ_IL_OP_MUL] = symbolic_handler_pure_unimplemented, // symbolic_handler_mul,
		[RZ_IL_OP_DIV] = symbolic_handler_pure_unimplemented, // symbolic_handler_div,
		[RZ_IL_OP_MOD] = symbolic_handler_pure_unimplemented, // symbolic_handler_mod,
		[RZ_IL_OP_SDIV] = symbolic_handler_pure_unimplemented, // symbolic_handler_sdiv,
		[RZ_IL_OP_SMOD] = symbolic_handler_pure_unimplemented, // symbolic_handler_smod,
		[RZ_IL_OP_LOGAND] = symbolic_handler_pure_unimplemented, // symbolic_handler_logical_and,
		[RZ_IL_OP_LOGOR] = symbolic_handler_pure_unimplemented, // symbolic_handler_pure_unimplemented,//symbolic_handler_logical_or,
		[RZ_IL_OP_LOGXOR] = symbolic_handler_pure_unimplemented, // symbolic_handler_logical_xor,
		[RZ_IL_OP_SHIFTR] = symbolic_handler_pure_unimplemented, // symbolic_handler_shiftr,
		[RZ_IL_OP_SHIFTL] = symbolic_handler_pure_unimplemented, // symbolic_handler_shiftl,
		[RZ_IL_OP_EQ] = symbolic_handler_pure_unimplemented, // symbolic_handler_eq,
		[RZ_IL_OP_SLE] = symbolic_handler_pure_unimplemented, // symbolic_handler_sle,
		[RZ_IL_OP_ULE] = symbolic_handler_pure_unimplemented, // symbolic_handler_ule,
		[RZ_IL_OP_CAST] = symbolic_handler_pure_unimplemented, // symbolic_handler_cast,
		[RZ_IL_OP_APPEND] = symbolic_handler_pure_unimplemented, // symbolic_handler_append,
		[RZ_IL_OP_LOAD] = symbolic_handler_pure_unimplemented, // symbolic_handler_load,
		[RZ_IL_OP_LOADW] = symbolic_handler_pure_unimplemented, // symbolic_handler_loadw,

		// Fbasic Theory
		[RZ_IL_OP_FLOAT] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FBITS] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_IS_FINITE] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_IS_NAN] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_IS_INF] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_IS_FZERO] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_IS_FNEG] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_IS_FPOS] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FNEG] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FABS] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FREQUAL] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FSUCC] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FPRED] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FORDER] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FROUND] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FSQRT] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FRSQRT] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FADD] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FSUB] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FMUL] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FDIV] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FMOD] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FMAD] = symbolic_handler_pure_unimplemented,

		[RZ_IL_OP_FCAST_INT] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FCAST_SINT] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FCAST_FLOAT] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FCAST_SFLOAT] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FCONVERT] = symbolic_handler_pure_unimplemented,

		// Float Theory
		// TODO : Implement other Float Theory operations
		[RZ_IL_OP_FHYPOT] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FPOW] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FROOTN] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FPOWN] = symbolic_handler_pure_unimplemented,
		[RZ_IL_OP_FCOMPOUND] = symbolic_handler_pure_unimplemented,
	};

OpEffectSymbolicHandler
	symbolic_op_handler_effect_table_default[RZ_IL_OP_EFFECT_MAX] = {
		[RZ_IL_OP_EMPTY] = symbolic_handler_effect_unimplemented, // symbolic_handler_empty,
		[RZ_IL_OP_STORE] = symbolic_handler_effect_unimplemented, // symbolic_handler_store,
		[RZ_IL_OP_STOREW] = symbolic_handler_effect_unimplemented, // symbolic_handler_storew,
		[RZ_IL_OP_NOP] = symbolic_handler_effect_unimplemented, // symbolic_handler_nop,
		[RZ_IL_OP_SET] = symbolic_handler_effect_unimplemented, // symbolic_handler_set,
		[RZ_IL_OP_JMP] = symbolic_handler_effect_unimplemented, // symbolic_handler_jmp,
		[RZ_IL_OP_GOTO] = symbolic_handler_effect_unimplemented, // symbolic_handler_goto,
		[RZ_IL_OP_SEQ] = symbolic_handler_effect_unimplemented, // symbolic_handler_seq,
		[RZ_IL_OP_BLK] = symbolic_handler_effect_unimplemented, // symbolic_handler_blk,
		[RZ_IL_OP_REPEAT] = symbolic_handler_effect_unimplemented, // symbolic_handler_repeat,
		[RZ_IL_OP_BRANCH] = symbolic_handler_effect_unimplemented, // symbolic_handler_branch,
	};

Z3_ast mk_var(Z3_context ctx, const char *name, Z3_sort ty) {
	Z3_symbol s = Z3_mk_string_symbol(ctx, name);
	return Z3_mk_const(ctx, s, ty);
}

bool z3_ast_is(Z3_context ctx, Z3_ast ast, Z3_ast_kind const kind) {
	return Z3_get_ast_kind(ctx, ast) == kind;
}
bool z3_sort_is(Z3_context ctx, Z3_ast ast, Z3_sort_kind const kind) {
	return Z3_get_sort_kind(ctx, Z3_get_sort(ctx, ast)) == kind;
}
const char *z3_ast_kind_to_str(Z3_ast_kind const kind) {
	switch (kind) {
	case Z3_NUMERAL_AST: return "Z3_NUMERAL_AST";
	case Z3_APP_AST: return "Z3_APP_AST";
	case Z3_VAR_AST: return "Z3_VAR_AST";
	case Z3_QUANTIFIER_AST: return "Z3_QUANTIFIER_AST";
	case Z3_SORT_AST: return "Z3_SORT_AST";
	case Z3_FUNC_DECL_AST: return "Z3_FUNC_DECL_AST";
	default: return "Z3_UNKNOWN_AST";
	};
	return "Z3_UNKNOWN_AST";
}
const char *z3_sort_kind_to_str(Z3_sort_kind kind) {
	switch (kind) {
	case Z3_UNINTERPRETED_SORT: return "Z3_UNINTERPRETED_SORT";
	case Z3_BOOL_SORT: return "Z3_BOOL_SORT";
	case Z3_INT_SORT: return "Z3_INT_SORT";
	case Z3_REAL_SORT: return "Z3_REAL_SORT";
	case Z3_BV_SORT: return "Z3_BV_SORT";
	case Z3_ARRAY_SORT: return "Z3_ARRAY_SORT";
	case Z3_DATATYPE_SORT: return "Z3_DATATYPE_SORT";
	case Z3_RELATION_SORT: return "Z3_RELATION_SORT";
	case Z3_FINITE_DOMAIN_SORT: return "Z3_FINITE_DOMAIN_SORT";
	case Z3_FLOATING_POINT_SORT: return "Z3_FLOATING_POINT_SORT";
	case Z3_ROUNDING_MODE_SORT: return "Z3_ROUNDING_MODE_SORT";
	case Z3_SEQ_SORT: return "Z3_SEQ_SORT";
	case Z3_RE_SORT: return "Z3_RE_SORT";
	case Z3_CHAR_SORT: return "Z3_CHAR_SORT";
	default: return "Z3_UNKNOWN_SORT";
	};

	return "Z3_UNKNOWN_SORT";
}
// Evaluation (Translate to Z3 ast)
Z3_ast eval_to_ast_bitv(RzILSVM *svm, RzILOpBitVector *op) {
	rz_return_val_if_fail(svm && op, NULL);
	Z3_ast bitvector = to_z3_pure(svm, op);
	if (!z3_sort_is(svm->ctx, bitvector, Z3_BV_SORT)) {
		RZ_LOG_ERROR("RzIL to Z3: type error: expected bitvector sort, got %s", z3_sort_kind_to_str(Z3_BV_SORT));
		return NULL;
	}
	return bitvector;
}
Z3_ast eval_to_ast_bool(RzILSVM *svm, RzILOpBool *op) {
	rz_return_val_if_fail(svm && op, NULL);
	Z3_ast boolean = to_z3_pure(svm, op);
	if (!z3_sort_is(svm->ctx, boolean, Z3_BOOL_SORT)) {
		RZ_LOG_ERROR("RzIL to Z3: type error: expected bool sort, got %s", z3_sort_kind_to_str(Z3_BOOL_SORT));
		return NULL;
	}
	return boolean;
}
Z3_ast eval_to_ast_float(RzILSVM *svm, RzILOpPure *op) {
	rz_return_val_if_fail(svm && op, NULL);
	Z3_ast floating_point = to_z3_pure(svm, op);
	if (!z3_sort_is(svm->ctx, floating_point, Z3_REAL_SORT)) {
		RZ_LOG_ERROR("RzIL to Z3: type error: expected float sort, got %s", z3_sort_kind_to_str(Z3_REAL_SORT));
		return NULL;
	}
	return floating_point;
	// unimplemented yet
	return NULL;
}
Z3_ast to_z3_evaluate_value(RzILSVM *svm, RzILOpPure *op) {
	rz_return_val_if_fail(svm && op, NULL);
	Z3_ast value = to_z3_pure(svm, op);
	if (!z3_sort_is(svm->ctx, value, Z3_BOOL_SORT)) {
		RZ_LOG_ERROR("RzIL to Z3: type error: expected bool sort, got %s", z3_sort_kind_to_str(Z3_BOOL_SORT));
		return NULL;
	}
	return boolean;
	return NULL;
}

void *to_z3_pure(RzILSVM *svm, RzILOpPure *op) {
	rz_return_val_if_fail(svm && op, NULL);
	OpPureSymbolicHandler handler = svm->symbolic_op_handler_pure_table[op->code];
	rz_return_val_if_fail(handler, NULL);
	return handler(svm, op);
}

void *to_z3_effect(RzILSVM *svm, RzILOpEffect *op) {
	rz_return_val_if_fail(svm && op, NULL);
	OpEffectSymbolicHandler handler = svm->symbolic_op_handler_effect_table[op->code];
	rz_return_val_if_fail(handler, NULL);
	return handler(svm, op);
}
// Handler for core theory opcodes
void *symbolic_handler_ite(RzILSVM *svm, RzILOpPure *op) {
	rz_return_val_if_fail(svm && op, NULL);
	Z3_ast condition, then_expr, else_expr;
	RzILOpArgsIte *args = &op->op.ite;
	condition = eval_to_ast_bool(svm, args->condition);
	then_expr = to_z3_pure(svm, args->x);
	else_expr = to_z3_pure(svm, args->y);
	rz_return_val_if_fail(condition && then_expr && else_expr, NULL);
	return Z3_mk_ite(svm->ctx, condition, then_expr, else_expr);
}

void *symbolic_handler_var(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_let(RzILSVM *svm, RzILOpPure *op);

void *symbolic_handler_bitv(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_msb(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_lsb(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_is_zero(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_ule(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_sle(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_neg(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_logical_not(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_add(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_sub(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_mul(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_div(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_sdiv(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_mod(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_smod(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_shiftl(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_shiftr(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_eq(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_logical_and(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_logical_or(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_logical_xor(RzILSVM *svm, RzILOpPure *op);

void *symbolic_handler_bool_false(RzILSVM *svm, RzILOpPure *op) {
	rz_return_val_if_fail(svm && op, NULL);
	return Z3_mk_false(svm->ctx);
}
void *symbolic_handler_bool_true(RZ_NONNULL RzILSVM *svm, RZ_NONNULL RzILOpPure *op) {
	rz_return_val_if_fail(svm && op, NULL);
	return Z3_mk_true(svm->ctx);
}
void *symbolic_handler_bool_and(RzILSVM *svm, RzILOpPure *op) {
	rz_return_val_if_fail(svm && op, NULL);
	Z3_ast booleans[2];
	RzILOpArgsBoolAnd *args = &op->op.booland;
	booleans[0] = eval_to_ast_bool(svm, args->x);
	booleans[1] = eval_to_ast_bool(svm, args->y);
	rz_return_val_if_fail(booleans[0] && booleans[1], NULL);
	return Z3_mk_and(svm->ctx, 2, booleans);
}
void *symbolic_handler_bool_or(RzILSVM *svm, RzILOpPure *op) {
	rz_return_val_if_fail(svm && op, NULL);
	Z3_ast booleans[2];
	RzILOpArgsBoolOr *args = &op->op.boolor;
	booleans[0] = eval_to_ast_bool(svm, args->x);
	booleans[1] = eval_to_ast_bool(svm, args->y);
	rz_return_val_if_fail(booleans[0] && booleans[1], NULL);
	return Z3_mk_or(svm->ctx, 2, booleans);
}
void *symbolic_handler_bool_xor(RzILSVM *svm, RzILOpPure *op) {
	rz_return_val_if_fail(svm && op, NULL);
	Z3_ast booleans[2];
	RzILOpArgsBoolXor *args = &op->op.boolxor;
	booleans[0] = eval_to_ast_bool(svm, args->x);
	booleans[1] = eval_to_ast_bool(svm, args->y);
	rz_return_val_if_fail(booleans[0] && booleans[1], NULL);
	return Z3_mk_xor(svm->ctx, booleans[0], booleans[1]);
}
void *symbolic_handler_bool_inv(RzILSVM *svm, RzILOpPure *op) {
	rz_return_val_if_fail(svm && op, NULL);
	Z3_ast boolean;
	RzILOpArgsBoolInv *args = &op->op.boolinv;
	boolean = eval_to_ast_bool(svm, args->x);
	rz_return_val_if_fail(boolean, NULL);
	return Z3_mk_not(svm->ctx, boolean);
}

void *symbolic_handler_cast(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_append(RzILSVM *svm, RzILOpPure *op);

void *symbolic_handler_empty(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_nop(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_set(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_jmp(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_goto(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_seq(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_blk(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_repeat(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_branch(RzILSVM *svm, RzILOpEffect *op);

void *symbolic_handler_load(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_store(RzILSVM *svm, RzILOpEffect *op);
void *symbolic_handler_loadw(RzILSVM *svm, RzILOpPure *op);
void *symbolic_handler_storew(RzILSVM *svm, RzILOpEffect *op);
