#[repr(C)]
enum RzILOpPureCode{
	// Init
	RZ_IL_OP_VAR,
	RZ_IL_OP_ITE,
	RZ_IL_OP_LET,

	// RzILBool
	RZ_IL_OP_B0,
	RZ_IL_OP_B1,
	RZ_IL_OP_INV,
	RZ_IL_OP_AND,
	RZ_IL_OP_OR,
	RZ_IL_OP_XOR,

	// RzBitVector
	RZ_IL_OP_BITV,
	RZ_IL_OP_MSB,
	RZ_IL_OP_LSB,
	RZ_IL_OP_IS_ZERO,
	RZ_IL_OP_NEG,
	RZ_IL_OP_LOGNOT,
	RZ_IL_OP_ADD,
	RZ_IL_OP_SUB,
	RZ_IL_OP_MUL,
	RZ_IL_OP_DIV,
	RZ_IL_OP_SDIV,
	RZ_IL_OP_MOD,
	RZ_IL_OP_SMOD,
	RZ_IL_OP_LOGAND,
	RZ_IL_OP_LOGOR,
	RZ_IL_OP_LOGXOR,
	RZ_IL_OP_SHIFTR,
	RZ_IL_OP_SHIFTL,
	RZ_IL_OP_EQ,
	RZ_IL_OP_SLE,
	RZ_IL_OP_ULE,
	RZ_IL_OP_CAST,
	RZ_IL_OP_APPEND,

	// RzILFloat
	RZ_IL_OP_FLOAT,
	RZ_IL_OP_FBITS,
	RZ_IL_OP_IS_FINITE,
	RZ_IL_OP_IS_NAN,
	RZ_IL_OP_IS_INF,
	RZ_IL_OP_IS_FZERO,
	RZ_IL_OP_IS_FNEG,
	RZ_IL_OP_IS_FPOS,
	RZ_IL_OP_FNEG,
	RZ_IL_OP_FABS,
	RZ_IL_OP_FCAST_INT,
	RZ_IL_OP_FCAST_SINT,
	RZ_IL_OP_FCAST_FLOAT,
	RZ_IL_OP_FCAST_SFLOAT,
	RZ_IL_OP_FCONVERT,
	RZ_IL_OP_FREQUAL,
	RZ_IL_OP_FSUCC,
	RZ_IL_OP_FPRED,
	RZ_IL_OP_FORDER,
	RZ_IL_OP_FROUND,
	RZ_IL_OP_FSQRT,
	RZ_IL_OP_FRSQRT,
	RZ_IL_OP_FADD,
	RZ_IL_OP_FSUB,
	RZ_IL_OP_FMUL,
	RZ_IL_OP_FDIV,
	RZ_IL_OP_FMOD,
	RZ_IL_OP_FHYPOT,
	RZ_IL_OP_FPOW,
	RZ_IL_OP_FMAD,
	RZ_IL_OP_FROOTN,
	RZ_IL_OP_FPOWN,
	RZ_IL_OP_FCOMPOUND,
	// ...

	// Memory
	RZ_IL_OP_LOAD,
	RZ_IL_OP_LOADW,

	RZ_IL_OP_PURE_MAX
}


/**
 * \brief value is a bitvector constant.
 *
 * In BAP: `int : 's Bitv.t Value.sort -> word -> 's bitv`
 */
struct rz_il_op_args_bv_t {
	RzBitVector *value; ///< value of bitvector
} 
type RzILOpArgsBv = rz_il_op_args_bv_t;

/**
 *  \brief op structure for `'s bitv -> bool`
 *  [MSB] msb x is the most significant bit of x.
 *  [LSB] lsb x is the least significant bit of x.
 *  [IS_ZERO] is_zero x holds if x is a bitvector of all zeros.
 */
struct rz_il_op_args_un_bv_b_t {
	RzILOpBitVector *bv;
};

type RzILOpArgsMsb = rz_il_op_args_un_bv_b_t;
type RzILOpArgsLsb = rz_il_op_args_un_bv_b_t;
type RzILOpArgsIsZero = rz_il_op_args_un_bv_b_t;

/**
 * op structure for
 * `not` ('s bitv -> 's bitv)
 *   not x is one-complement negation.
 * `neg` ('s bitv -> 's bitv)
 *   neg x is two-complement unary minus
 */
struct rz_il_op_args_bv_unop_t {
	RzILOpBitVector *bv; ///< unary operand
};

type RzILOpArgsLogNot = rz_il_op_args_bv_unop_t;
type RzILOpArgsNeg = rz_il_op_args_bv_unop_t;

/**
 *  \brief op structure for two-operand algorithm and logical operations ('s bitv -> 's bitv -> 's bitv)
 *
 *  [ADD] add x y addition modulo 2^'s
 *  [SUB] sub x y subtraction modulo 2^'s
 *  [MUL] mul x y multiplication modulo 2^'s
 *  [DIV] div x y unsigned division modulo 2^'s truncating towards 0. The division by zero is defined to be a vector of all ones of size 's.
 *  [MOD] modulo x y is the remainder of div x y modulo 2^'s.
 *  [SDIV] sdiv x y is signed division of x by y modulo 2^'s.
 *  [SMOD] smodulo x y is the signed remainder of div x y modulo 2^'s.
 *  [LOGAND] logand x y is a bitwise logical and of x and y.
 *  [LOGOR] logor x y is a bitwise logical or of x and y.
 *  [LOGXOR] logxor x y is a bitwise logical xor of x and y.
 */
struct rz_il_op_args_alg_log_operations_t {
	RzILOpBitVector *x; ///< left operand
	RzILOpBitVector *y; ///< right operand
};

type RzILOpArgsAdd = rz_il_op_args_alg_log_operations_t;
type RzILOpArgsSub = rz_il_op_args_alg_log_operations_t;
type RzILOpArgsMul = rz_il_op_args_alg_log_operations_t;
type RzILOpArgsDiv = rz_il_op_args_alg_log_operations_t;
type RzILOpArgsSdiv = rz_il_op_args_alg_log_operations_t;
type RzILOpArgsMod = rz_il_op_args_alg_log_operations_t;
type RzILOpArgsSmod = rz_il_op_args_alg_log_operations_t;
type RzILOpArgsLogand = rz_il_op_args_alg_log_operations_t;
type RzILOpArgsLogor = rz_il_op_args_alg_log_operations_t;
type RzILOpArgsLogxor = rz_il_op_args_alg_log_operations_t;

/**
 *  \brief op structure for binary comparison ops ('a bitv -> 'a bitv -> bool)
 *
 *  [EQ] eq x y binary predicate for bitwise equality
 *  [SLE] sle x y binary predicate for singed less than or equal
 *  [ULE] ule x y binary predicate for unsigned less than or equal
 */
struct rz_il_op_args_cmp_t {
	RzILOpBitVector *x; ///< index of operand 1
	RzILOpBitVector *y; ///< index of operand 2
};

type RzILOpArgsEq = rz_il_op_args_cmp_t;
type RzILOpArgsSle = rz_il_op_args_cmp_t;
type RzILOpArgsUle = rz_il_op_args_cmp_t;

/**
 *  \brief op structure for casting bitv
 */
struct rz_il_op_args_cast_t {
	ut32 length; ///< new bits length
	RzILOpBool *fill; ///< If m = size val - length > 0 then m fill-bits are prepended to the most significant part of the vector.
	RzILOpBitVector *val; ///< value to cast
} RzILOpArgsCast;

/**
 *  \struct rz_il_op_args_append_t
 *  \brief op structure for appending 2 bitv: MSB:LSB high:low
 */
struct rz_il_op_args_append_t {
	RzILOpBitVector *high; ///< bitvector occupying the most significant bits
	RzILOpBitVector *low; ///< bitvector occupying the least significant bits
} RzILOpArgsAppend;

/**
 *  \brief op structure for lshift and rshift (bool -> 's bitv -> 'b bitv -> 's bitv)
 *
 *  [LSHIFT] shiftl s x m shifts x left by m bits filling with s.
 *  [RSHIFT] shiftr s x m shifts x right by m bits filling with s.
 */
struct rz_il_op_args_shift_t {
	RzILOpBool *fill_bit; ///< index of fill bit
	RzILOpBitVector *x; ///< index of operand 1
	RzILOpBitVector *y; ///< index of operand 2
};

type RzILOpArgsShiftLeft = rz_il_op_args_shift_t;
type RzILOpArgsShiftRight = rz_il_op_args_shift_t;

/**
 * \brief op structure for `set` ('a var -> 'a pure -> data eff)
 *
 * set v x changes the value stored in v to the value of x.
 */
struct rz_il_op_args_set_t {
	const char *v; ///< name of variable, const one
	bool is_local; ///< whether a global variable should be set or a local optionally created and set
	RzILOpPure *x; ///< value to set the variable to
} RzILOpArgsSet;

/**
 * \brief op structure for `let_ : 'a var -> 'a pure -> 'b pure -> 'b pure`
 *
 * `let_ v exp body` binds the value of exp to v body.
 */
struct rz_il_op_args_let_t {
	const char *name; ///< name of variable
	RzILOpPure *exp; ///< value/expression to bind the variable to
	RzILOpPure *body; ///< body in which the variable will be bound and that produces the result
} RzILOpArgsLet;

/**
 *  \brief op structure for `jmp` (_ bitv -> ctrl eff)
 *
 *  jmp dst passes the control to a program located at dst.
 */
struct rz_il_op_args_jmp_t {
	RzILOpBitVector *dst; ///< index of destination address
} 
type RzILOpArgsJmp = rz_il_op_args_jmp_t;

/**
 *  \brief op structure for `goto` (label -> ctrl eff)
 *
 *  goto lbl passes the control to a program labeled with lbl.
 */
struct rz_il_op_args_goto_t {
	const char *lbl; ///< name of the label, const one
} 
type RzILOpArgsGoto = rz_il_op_args_goto_t;

/**
 *  \brief op structure for `Seq` ('a eff -> 'a eff -> 'a eff)
 *
 *  seq x y performs effect x, after that perform effect y. Pack two effects into one.
 */
struct rz_il_op_args_seq_t {
	RzILOpEffect *x; ///< perform this first
	RzILOpEffect *y; ///< perform this second
} RzILOpArgsSeq;

/**
 *  \brief op structure for `blk` (label -> data eff -> ctrl eff -> unit eff)
 *
 *  blk lbl data ctrl a labeled sequence of effects.
 */
struct rzil_op_blk_t {
	const char *label; ///< name of the label, const one
	RzILOpEffect *data_eff; ///< index of data_eff
	RzILOpEffect *ctrl_eff; ///< index of ctrl_eff
} RzILOpArgsBlk;

/**
 *  \brief op structure for `repeat` (bool -> data eff -> data eff)
 *
 *  repeat c data repeats data effects till the condition c holds.
 */
struct rzil_op_repeat_t {
	RzILOpBool *condition; ///< index of BOOL condition
	RzILOpEffect *data_eff; ///< index of data effect
} RzILOpArgsRepeat;

/**
 *  \brief op structure for `branch` (bool -> 'a eff -> 'a eff -> 'a eff)
 *
 *  branch c lhs rhs if c holds then performs lhs else rhs.
 */
struct rz_il_op_args_branch_t {
	RzILOpBool *condition;
	RZ_NONNULL RzILOpEffect *true_eff; ///< effect for when condition evaluates to true
	RZ_NONNULL RzILOpEffect *false_eff; ///< effect for when condition evaluates to false
} RzILOpArgsBranch;

/**
 *  \brief op structure for `ite` (bool -> 'a pure -> 'a pure -> 'a pure)
 *
 *  ite c x y is x if c evaluates to b1 else y.
 */
struct rz_il_op_args_ite_t {
	RzILOpBool *condition; ///< index of BOOL condition
	RzILOpPure *x; ///< index of RzILVal operand 1
	RzILOpPure *y; ///< index of RzILVal operand 2
} RzILOpArgsIte;

/**
 *  \brief op structure for `var` ('a var -> 'a pure)
 *
 *  var v is the value of the variable v.
 */
struct rz_il_op_args_var_t {
	const char *v; ///< name of variable, const one
	RzILVarKind kind; ///< set of variables to pick from
} RzILOpArgsVar;

/**
 *  \brief op structure for `and`, `or` and `xor` (bool -> bool -> bool)
 *
 *  BAP equivalent:
 *    val and_ : bool -> bool -> bool
 *    val or_ : bool -> bool -> bool
 *  and(x, y) is a conjunction of x and y.
 *  or(x, y)  is a conjunction of x or y.
 *  xor(x, y) is a conjunction of x xor y.
 */
struct rz_il_op_args_bool_operation_t {
	RzILOpBool *x; ///< left operand
	RzILOpBool *y; ///< right operand
};

type RzILOpArgsBoolAnd = rz_il_op_args_bool_operation_t;
type RzILOpArgsBoolOr = rz_il_op_args_bool_operation_t;
type RzILOpArgsBoolXor = rz_il_op_args_bool_operation_t;

/**
 *  \brief op structure for `inv` (!bool -> bool)
 *
 *	BAP equivalent:
 *	  val inv : bool -> bool
 *  inv(x) inverts x (also known as not operation).
 */
struct rz_il_op_args_bool_inv_t {
	RzILOpBool *x; ///< single operand
};

type RzILOpArgsBoolInv = rz_il_op_args_bool_inv_t;

/**
 *  \brief op structure for `load` (('a, 'b) mem -> 'a bitv -> 'b bitv)
 *
 *  load m k is the value associated with the key k in the memory m.
 */
struct rz_il_op_args_load_t {
	RzILMemIndex mem; ///< index of the mem inside the vm to use
	RzILOpBitVector *key; ///< index of the cell (address) in mem, must have exactly the size of a key in the memory
} RzILOpArgsLoad;

/**
 *  \brief op structure for `store` (('a, 'b) mem -> 'a bitv -> 'b bitv -> ('a, 'b) mem)
 *
 *  store m k x a memory m in which the key k is associated with the word x.
 */
struct rz_il_op_args_store_t {
	RzILMemIndex mem; ///< index of memory in the vm to use
	RzILOpBitVector *key; ///< address where to store to, must have exactly the size of a key in the memory
	RzILOpBitVector *value; ///< value to store, must have exactly the size of a memory cell
} RzILOpArgsStore;

/**
 * \brief Load an entire word of arbitrary bit size from a memory
 *
 * Endianness is determined by the vm
 */
struct rz_il_op_args_loadw_t {
	RzILMemIndex mem; ///< index of the mem inside the vm to use
	RzILOpBitVector *key; ///< memory index of the RzBitVector key (address)
	ut32 n_bits; ///< n of bits to read, and of the resulting bitvector
} RzILOpArgsLoadW;

/**
 * \brief Store an entire word of arbitrary bit size into a memory
 *
 * Endianness is determined by the vm
 */
struct rz_il_op_args_storew_t {
	RzILMemIndex mem; ///< index of memory in the vm to use
	RzILOpBitVector *key; ///< address where to store to
	RzILOpBitVector *value; ///< value to store, arbitrary size
} RzILOpArgsStoreW;

/**
 * \brief value for a float constant
 * `float s x` interprets x as a floating-point number in format s.
 * In BAP : `( 'r, 's ) format Float.t Value.sort -> 's bitv -> ( 'r, 's ) format float
 */
struct rz_il_op_args_float_t {
	RzFloatFormat r;
	RzILOpBitVector *bv;
} RzILOpArgsFloat;

/**
 * \brief op structure for unary without rmode
 */
struct rz_il_op_args_float_unary_t {
	RzILOpFloat *f;
};

/**
 * \brief opstructure for fbits : ( 'r, 's ) format float -> 's bitv
 * fbits x is a bitvector representation of the floating-point number x.
 */
type RzILOpArgsFbits = rz_il_op_args_float_unary_t;

/**
 * \brief op structure for 'f float -> bool
 * [IS_FINITE] is_finite x holds if x represents a finite number.
 * [IS_NAN] is_nan x holds if x represents a not-a-number (NaN).
 * [IS_INF] is_inf x holds if x represents an infinite number.
 * [IS_FZERO] is_fzero x holds if x represents a zero.
 * [IS_FNEG] is_fpos x holds if x represents a positive number.
 * [IS_FPOS] is_fneg x hold if x represents a negative number.
 */
type RzILOpArgsIsFinite = rz_il_op_args_float_unary_t;
type RzILOpArgsIsNan = rz_il_op_args_float_unary_t;
type RzILOpArgsIsInf = rz_il_op_args_float_unary_t;
type RzILOpArgsIsFzero = rz_il_op_args_float_unary_t;
type RzILOpArgsIsFpos = rz_il_op_args_float_unary_t;
type RzILOpArgsIsFneg = rz_il_op_args_float_unary_t;

/**
 * op structure for 'f float -> float
 * [FNEG] fneg x is -x
 * [FABS] fabs x is absolute value of x (|x|)
 * [FSUCC] fsucc x is the least floating-point number representable in (sort x) that is greater than x.
 * [FPRED] fpred x is the greatest floating-point number representable in (sort x) that is less than x.
 */
type RzILOpArgsFneg = rz_il_op_args_float_unary_t;
type RzILOpArgsFabs = rz_il_op_args_float_unary_t;
type RzILOpArgsFsucc = rz_il_op_args_float_unary_t;
type RzILOpArgsFpred = rz_il_op_args_float_unary_t;

/**
 * \brief op structure for cast to bv from float
 * [FCAST_INT] `f_cast_int s rm x` returns an integer closest to x.
 * [FCAST_SINT] `f_cast_sint s rm x` returns an integer closest to x.
 */
struct rz_il_op_args_float_cast_int_t {
	ut32 length;
	RzFloatRMode mode;
	RzILOpFloat *f;
};

type RzILOpArgsFCastint = rz_il_op_args_float_cast_int_t;
type RzILOpArgsFCastsint = rz_il_op_args_float_cast_int_t;

/**
 * \brief for cast to float from bv
 * 'f Float.t Value.sort -> rmode -> 'a bitv -> 'f float
 * [FCAST_FLOAT] `cast_float s rm x` is the closest to x floating-point number of sort x.
 * 	note that : The bitvector x is interpreted as an unsigned integer in the two-complement form.
 * [FCAST_SFLOAT] `cast_sfloat s rm x` is the closest to x floating-point number of sort x.
 * 	note that : The bitvector x is interpreted as a signed integer in the two-complement form.
 */
struct rz_il_op_args_float_cast_float_t {
	RzFloatFormat format;
	RzFloatRMode mode;
	RzILOpBitVector *bv;
};

type RzILOpArgsFCastfloat = rz_il_op_args_float_cast_float_t;
type RzILOpArgsFCastsfloat = rz_il_op_args_float_cast_float_t;

/**
 * \brief convert between different float format
 * 'f Float.t Value.sort -> rmode -> _ float -> 'f float
 * [FCONVERT] `fconvert f r x` is the closest to x floating number in format f.
 */
struct rz_il_op_args_float_fconvert_t {
	RzFloatFormat format;
	RzFloatRMode mode;
	RzILOpFloat *f;
};
type RzILOpArgsFconvert = rz_il_op_args_float_fconvert_t;

/**
 * \brief op structure of requal
 *  rmode -> rmode -> bool
 * requal x y holds if rounding modes are equal.
 */
struct rz_il_op_args_float_requal_t {
	RzFloatRMode x;
	RzFloatRMode y;
} RzILOpArgsFrequal;

/**
 * \brief op structure of binary op without rmode
 * ('float -> 'flaat -> bool)
 * forder x y holds if floating-point number x is less than y.
 */
struct rz_il_op_args_float_binop_t {
	RzILOpFloat *x;
	RzILOpFloat *y;
} RzILOpArgsForder;

/**
 * \brief op structure for float operation (unary op with rmode)
 * `rmode -> 'f float -> 'f float`
 * [FROUND]
 * [FSQRT] fsqrt m x returns the closest floating-point number to r, where r is such number that r*r is equal to x.
 * [FRSQRT] reverse sqrt, rsqrt m x is the closest floating-point number to 1 / sqrt x.
 */
struct rz_il_op_args_float_alg_unop_t {
	RzFloatRMode rmode;
	RzILOpFloat *f;
};
type RzILOpArgsFround = rz_il_op_args_float_alg_unop_t;
type RzILOpArgsFsqrt = rz_il_op_args_float_alg_unop_t;
type RzILOpArgsFrsqrt = rz_il_op_args_float_alg_unop_t;

/**
 * \brief op structure for float basic arithmetic operations (binary op with rmode)
 * rmode -> 'f float -> 'f float -> 'f float
 * [FADD]
 */
struct rz_il_op_args_float_alg_binop_t {
	RzFloatRMode rmode;
	RzILOpFloat *x;
	RzILOpFloat *y;
};

type RzILOpArgsFadd = rz_il_op_args_float_alg_binop_t;
type RzILOpArgsFsub = rz_il_op_args_float_alg_binop_t;
type RzILOpArgsFmul = rz_il_op_args_float_alg_binop_t;
type RzILOpArgsFdiv = rz_il_op_args_float_alg_binop_t;
type RzILOpArgsFmod = rz_il_op_args_float_alg_binop_t;
type RzILOpArgsFhypot = rz_il_op_args_float_alg_binop_t;
type RzILOpArgsFpow = rz_il_op_args_float_alg_binop_t;

/**
 * \brief op structure of ternary op in float
 * rmode -> 'f float -> 'f float -> 'f float -> 'f float
 */
struct rz_il_op_args_float_alg_terop_t {
	RzFloatRMode rmode;
	RzILOpFloat *x;
	RzILOpFloat *y;
	RzILOpFloat *z;
};
type RzILOpArgsFmad = rz_il_op_args_float_alg_terop_t;

/**
 * \brief op structure for some float binary op requiring `int`
 * rmode -> 'f float -> 'a bitv -> 'f float
 */
struct rz_il_op_args_float_alg_hybrid_binop_t {
	RzFloatRMode rmode;
	RzILOpFloat *f;
	RzILOpBitVector *n;
};

type RzILOpArgsFrootn = rz_il_op_args_float_alg_hybrid_binop_t;
type RzILOpArgsFpown = rz_il_op_args_float_alg_hybrid_binop_t;
type RzILOpArgsFcompound = rz_il_op_args_float_alg_hybrid_binop_t;

#[repr(C)]
pub struct RzILOpPure {
	code: RzILOpPureCode,
	union op {
		ite : RzILOpArgsIte,
		var : RzILOpArgsVar,
		_let : RzILOpArgsLet,

		booland : RzILOpArgsBoolAnd,
		boolor : RzILOpArgsBoolOr,
		boolxor : RzILOpArgsBoolXor,
		boolinv : RzILOpArgsBoolInv,

		bitv : RzILOpArgsBv,
		msb : RzILOpArgsMsb,
		lsb : RzILOpArgsLsb,
		is_zero : RzILOpArgsIsZero,
		eq : RzILOpArgsEq,
		ule : RzILOpArgsUle,
		sle : RzILOpArgsSle,
		cast : RzILOpArgsCast,
		neg : RzILOpArgsNeg,
		lognot : RzILOpArgsLogNot,
		add : RzILOpArgsAdd,
		sub : RzILOpArgsSub,
		mul : RzILOpArgsMul,
		div : RzILOpArgsDiv,
		sdiv : RzILOpArgsSdiv,
		smod : RzILOpArgsSmod,
		_mod : RzILOpArgsMod,
		logand : RzILOpArgsLogand,
		logor : RzILOpArgsLogor,
		logxor : RzILOpArgsLogxor,
		shiftl : RzILOpArgsShiftLeft,
		shiftr : RzILOpArgsShiftRight,
		append : RzILOpArgsAppend,

		load : RzILOpArgsLoad,
		loadw : RzILOpArgsLoadW,

		float_ : RzILOpArgsFloat,
		fbits : RzILOpArgsFbits,
		is_finite : RzILOpArgsIsFinite,
		is_nan : RzILOpArgsIsNan,
		is_inf : RzILOpArgsIsInf,
		is_fzero : RzILOpArgsIsFzero,
		is_fneg : RzILOpArgsIsFneg,
		is_fpos : RzILOpArgsIsFpos,
		fneg : RzILOpArgsFneg,
		fabs : RzILOpArgsFabs,
		fcast_int : RzILOpArgsFCastint,
		fcast_sint : RzILOpArgsFCastsint,
		fcast_float : RzILOpArgsFCastfloat,
		fcast_sfloat : RzILOpArgsFCastsfloat,
		fconvert : RzILOpArgsFconvert,
		frequal : RzILOpArgsFrequal,
		fsucc : RzILOpArgsFsucc,
		fpred : RzILOpArgsFpred,
		forder : RzILOpArgsForder,
		fround : RzILOpArgsFround,
		fsqrt : RzILOpArgsFsqrt,
		frsqrt : RzILOpArgsFrsqrt,
		fadd : RzILOpArgsFadd,
		fsub : RzILOpArgsFsub,
		fmul : RzILOpArgsFmul,
		fdiv : RzILOpArgsFdiv,
		fmod : RzILOpArgsFmod,
		fmad : RzILOpArgsFmad,
		fpow : RzILOpArgsFpow,
		fpown : RzILOpArgsFpown,
		frootn : RzILOpArgsFrootn,
		fcompound : RzILOpArgsFcompound,
		fhypot : RzILOpArgsFhypot,
	}
}
