#ifndef RZ_GADGET
#define RZ_GADGET
#include <rz_core.h>
#include <z3.h>

bool rop_smtlib(RzCore *core, const char *flag);

typedef struct rz_il_symbolic_vm_t RzILSVM;
typedef void *(*OpPureSymbolicHandler)(RzILSVM *svm, RzILOpPure *op);
typedef void *(*OpEffectSymbolicHandler)(RzILSVM *svm, RzILOpEffect *op);
struct rz_il_symbolic_vm_t {
	RzILVM vm;
	Z3_context ctx;
	Z3_solver slvr;
	OpPureSymbolicHandler *symbolic_op_handler_pure_table;
	OpEffectSymbolicHandler *symbolic_op_handler_effect_table;
};

Z3_ast to_z3_bitv(RzILSVM *svm, RzILOpBitVector *op);
Z3_ast to_z3_bool(RzILSVM *svm, RzILOpBool *op);
Z3_ast to_z3_float(RzILSVM *svm, RzILOpPure *op);
Z3_ast to_z3_val(RzILSVM *svm, RzILOpPure *op);
void *to_z3_pure(RzILSVM *svm, RzILOpPure *op);
void *to_z3_effect(RzILSVM *svm, RzILOpEffect *op);

#endif
