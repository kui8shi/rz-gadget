#include <rz-gadget.h>

extern OpPureSymbolicHandler symbolic_op_handler_pure_table_default[RZ_IL_OP_PURE_MAX];
extern OpEffectSymbolicHandler symbolic_op_handler_effect_table_default[RZ_IL_OP_EFFECT_MAX];
bool rz_il_svm_init(RzILSVM *svm) {
	Z3_config cfg = Z3_mk_config();
	svm->ctx = Z3_mk_context(cfg);
	svm->slvr = Z3_mk_solver(svm->ctx);
	Z3_solver_inc_ref(svm->ctx, svm->slvr);
	svm->symbolic_op_handler_effect_table = symbolic_op_handler_effect_table_default;
	svm->symbolic_op_handler_pure_table = symbolic_op_handler_pure_table_default;
	Z3_del_config(cfg);
	return true;
}
void rz_il_svm_fini(RzILSVM *svm) {
	svm->symbolic_op_handler_effect_table = NULL;
	svm->symbolic_op_handler_pure_table = NULL;
	Z3_solver_dec_ref(svm->ctx, svm->slvr);
	svm->slvr = NULL;
	Z3_del_context(svm->ctx);
	svm->ctx = NULL;
}

RzILSVM *rz_il_svm_new(ut64 start_addr, ut32 addr_size, bool big_endian) {
	RzILSVM *svm = realloc(rz_il_vm_new(start_addr, addr_size, big_endian), sizeof(RzILSVM));
	if (!svm) {
		return NULL;
	}
	rz_il_svm_init(svm);
	return svm;
}
void rz_il_svm_free(RzILSVM *svm) {
	rz_il_svm_fini(svm);
	rz_il_vm_fini((RzILVM *)svm);
	free(svm);
}
