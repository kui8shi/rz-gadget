pub trait Core {
    //type Ast<'ctx> where Self: 'ctx;
    type Ast: Sized;
    fn const_bool(&self, val: bool) -> Self::Ast;
    fn not(&self, op: &Self::Ast) -> Self::Ast;
    fn and(&self, op: [&Self::Ast; 2]) -> Self::Ast;
    fn or(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn xor(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn implies(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn distinct(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn ite(&self, ops: [&Self::Ast; 3]) -> Self::Ast;
}

pub trait BitVec {
    type Ast: Sized;
    fn const_bv(&self, val: u64, size: usize) -> Self::Ast;
    fn concat(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvnot(&self, op: &Self::Ast) -> Self::Ast;
    fn bvneg(&self, op: &Self::Ast) -> Self::Ast;
    fn bvand(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvor(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvadd(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvmul(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvsmul_no_overflow(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvsmul_no_underflow(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvumul_no_overflow(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvudiv(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvurem(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvnand(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvnor(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvxor(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvxnor(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bveq(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvsub(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvsdiv(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvsrem(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvsmod(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvshl(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvlshr(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvashr(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    // parameterized functions
    fn extract(&self, op: &Self::Ast, high: usize, low: usize) -> Self::Ast;
    fn repeat(&self, op: &Self::Ast, rep: usize) -> Self::Ast;
    fn zero_ext(&self, op: &Self::Ast, size: usize) -> Self::Ast;
    fn sign_ext(&self, op: &Self::Ast, size: usize) -> Self::Ast;
    fn bvrotl(&self, op: &Self::Ast, shift: usize) -> Self::Ast;
    fn bvrotr(&self, op: &Self::Ast, shift: usize) -> Self::Ast;
    // logical functions
    fn bvult(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvule(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvugt(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvuge(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvslt(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvsle(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvsgt(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
    fn bvsge(&self, ops: [&Self::Ast; 2]) -> Self::Ast;
}

pub trait ArrayEx {
    type Ast: Sized;
    fn select(&self, key: &Self::Ast, size: usize) -> Self::Ast;
    fn store(&self, key: &Self::Ast, value: &Self::Ast, size: usize);
}
