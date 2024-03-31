use super::{
    ast::{Effect, Pure, PureCode, PureRef, Scope, Sort},
    error::{Result, RzILError},
};
use crate::variables::VarId;
use quick_cache::sync::Cache;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct RzILCache {
    pure_cache: Rc<Cache<u64, PureRef>>, // cache pure ops by their semantics
                                         //uniq_var_id: Cell<u64>,              // unique variable id
}

impl RzILCache {
    pub fn new() -> Self {
        const DEFAULT_CACHE_SIZE: usize = 100;
        RzILCache {
            pure_cache: Rc::new(Cache::new(DEFAULT_CACHE_SIZE)),
        }
    }
}

impl RzILBuilder for RzILCache {
    fn new_pure(
        &self,
        code: PureCode,
        args: Vec<PureRef>,
        symbolized: bool,
        sort: Sort,
        eval: u64,
    ) -> PureRef {
        let op: PureRef = Pure::new(code, args, symbolized, sort, eval).into();
        if let Some(cached) = self.pure_cache.get(&op.get_hash()) {
            cached.clone()
        } else {
            self.pure_cache.insert(op.get_hash(), op.clone());
            op
        }
    }
}

pub trait RzILBuilder {
    // ======== required methods ========
    fn new_pure(
        &self,
        code: PureCode,
        args: Vec<PureRef>,
        symbolized: bool,
        sort: Sort,
        eval: u64,
    ) -> PureRef;

    // ======== provided methods ========
    fn new_const(&self, sort: Sort, val: u64) -> PureRef {
        match sort {
            Sort::Bitv(_) => self.new_pure(PureCode::Bitv, vec![], false, sort, val),
            Sort::Bool => self.new_pure(PureCode::Bool, vec![], false, sort, val),
        }
    }

    fn new_pure_maybe_const(
        &self,
        code: PureCode,
        args: Vec<PureRef>,
        symbolized: bool,
        sort: Sort,
        eval: u64,
    ) -> PureRef {
        if !symbolized {
            self.new_const(sort, eval)
        } else {
            self.new_pure(code, args, symbolized, sort, eval)
        }
    }

    fn new_var(&self, scope: Scope, name: &str, val: &Pure) -> PureRef {
        self.new_pure(
            PureCode::Var(scope, VarId::new(name)),
            vec![],
            val.is_symbolized(),
            val.get_sort(),
            val.evaluate(),
        )
    }

    fn new_let_var(&self, name: &str, val: PureRef) -> PureRef {
        let symbolized = val.is_symbolized();
        let sort = val.get_sort();
        let eval = val.evaluate();
        self.new_pure(
            PureCode::Var(Scope::Let, VarId::new(name)),
            vec![val], // will be unwrapped
            symbolized,
            sort,
            eval,
        )
    }

    fn new_unconstrained(&self, sort: Sort, name: &str) -> PureRef {
        self.new_pure(
            PureCode::Var(Scope::Global, VarId::new(name)),
            vec![],
            false,
            sort,
            0,
        )
    }

    fn convert_bool_to_bitv(&self, op: PureRef) -> Result<PureRef> {
        if op.is_bool() {
            // convert op's sort from Bool to Bitv(1)
            if op.is_concretized() {
                Ok(self.new_const(Sort::Bitv(1), op.evaluate()))
            } else {
                let one = self.new_const(Sort::Bitv(1), 1);
                let zero = self.new_const(Sort::Bitv(1), 0);
                self.new_ite(op, one, zero)
            }
        } else if let Sort::Bitv(1) = op.get_sort() {
            // leave as it is
            Ok(op)
        } else {
            // an expresion unable to convert to Bitv(1)
            Err(RzILError::UnexpectedSort(Sort::Bitv(1), op.get_sort()))
        }
    }

    // Pure
    fn new_ite(&self, condition: PureRef, then: PureRef, otherwise: PureRef) -> Result<PureRef> {
        condition.expect_bool()?;
        then.expect_same_sort_with(&otherwise)?;

        let symbolized = condition.is_symbolized()
            || (condition.evaluate() != 0 && then.is_symbolized())
            || (condition.evaluate() == 0 && otherwise.is_symbolized());
        let sort = then.get_sort();
        let eval = if condition.evaluate() != 0 {
            then.evaluate()
        } else {
            otherwise.evaluate()
        };

        if condition.is_concretized() {
            if condition.evaluate_bool() {
                Ok(then)
            } else {
                Ok(otherwise)
            }
        } else {
            Ok(self.new_pure(
                PureCode::Ite,
                vec![condition, then, otherwise],
                symbolized,
                sort,
                eval,
            ))
        }
    }

    fn new_boolinv(&self, x: PureRef) -> Result<PureRef> {
        x.expect_bool()?;

        let symbolized = x.is_symbolized();
        let sort = Sort::Bool;
        let eval = (x.evaluate() == 0) as u64;

        Ok(self.new_pure_maybe_const(PureCode::BoolInv, vec![x], symbolized, sort, eval))
    }

    fn new_booland(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bool()?;
        y.expect_bool()?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bool;
        let eval = x.evaluate() & y.evaluate();

        Ok(self.new_pure_maybe_const(PureCode::BoolAnd, vec![x, y], symbolized, sort, eval))
    }

    fn new_boolor(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bool()?;
        y.expect_bool()?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bool;
        let eval = x.evaluate() | y.evaluate();

        Ok(self.new_pure_maybe_const(PureCode::BoolOr, vec![x, y], symbolized, sort, eval))
    }

    fn new_boolxor(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bool()?;
        y.expect_bool()?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bool;
        let eval = x.evaluate() ^ y.evaluate();

        Ok(self.new_pure_maybe_const(PureCode::BoolXor, vec![x, y], symbolized, sort, eval))
    }

    fn new_msb(&self, bitv: PureRef) -> Result<PureRef> {
        bitv.expect_bitv()?;

        let symbolized = bitv.is_symbolized();
        let sort = Sort::Bool;
        let eval = (bitv.evaluate() >> (bitv.get_size() - 1)) & 0x1;

        Ok(self.new_pure_maybe_const(PureCode::Msb, vec![bitv], symbolized, sort, eval))
    }

    fn new_lsb(&self, bitv: PureRef) -> Result<PureRef> {
        bitv.expect_bitv()?;

        let symbolized = bitv.is_symbolized();
        let sort = Sort::Bool;
        let eval = bitv.evaluate() & 0x1;

        Ok(self.new_pure_maybe_const(PureCode::Lsb, vec![bitv], symbolized, sort, eval))
    }

    fn new_is_zero(&self, bitv: PureRef) -> Result<PureRef> {
        bitv.expect_bitv()?;

        let symbolized = bitv.is_symbolized();
        let sort = Sort::Bool;
        let eval = if bitv.is_zero() { 1 } else { 0 };

        Ok(self.new_pure_maybe_const(PureCode::IsZero, vec![bitv], symbolized, sort, eval))
    }

    fn new_neg(&self, bitv: PureRef) -> Result<PureRef> {
        bitv.expect_bitv()?;

        let symbolized = bitv.is_symbolized();
        let sort = Sort::Bool;
        let eval = -(bitv.evaluate() as i64) as u64;

        Ok(self.new_pure_maybe_const(PureCode::Neg, vec![bitv], symbolized, sort, eval))
    }

    fn new_lognot(&self, bitv: PureRef) -> Result<PureRef> {
        bitv.expect_bitv()?;

        let symbolized = bitv.is_symbolized();
        let sort = Sort::Bool;
        let eval = !bitv.evaluate();

        Ok(self.new_pure_maybe_const(PureCode::LogNot, vec![bitv], symbolized, sort, eval))
    }

    fn new_bvadd(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bitv()?;
        y.expect_bitv()?;

        x.expect_same_sort_with(&y)?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bitv(x.get_size());
        let eval = if !symbolized {
            (x.evaluate() + y.evaluate()) & x.get_bitmask()
        } else {
            0
        };

        if x.is_zero() {
            Ok(y)
        } else if y.is_zero() {
            Ok(x)
        } else {
            Ok(self.new_pure_maybe_const(PureCode::Add, vec![x, y], symbolized, sort, eval))
        }
    }

    fn new_bvsub(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bitv()?;
        y.expect_bitv()?;

        x.expect_same_sort_with(&y)?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bitv(x.get_size());
        let eval = if !symbolized {
            (x.evaluate() - y.evaluate()) & x.get_bitmask()
        } else {
            0
        };

        if x.is_zero() {
            self.new_neg(y)
        } else if y.is_zero() {
            Ok(x)
        } else {
            Ok(self.new_pure_maybe_const(PureCode::Sub, vec![x, y], symbolized, sort, eval))
        }
    }

    fn new_bvmul(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bitv()?;
        y.expect_bitv()?;

        x.expect_same_sort_with(&y)?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bitv(x.get_size());
        let eval = if !symbolized {
            (x.evaluate() * y.evaluate()) & x.get_bitmask()
        } else {
            0
        };

        if x.is_zero() || y.is_zero() {
            Ok(self.new_const(sort, 0))
        } else if x.evaluate() == 1 {
            Ok(y)
        } else if y.evaluate() == 1 {
            Ok(x)
        } else {
            Ok(self.new_pure_maybe_const(PureCode::Mul, vec![x, y], symbolized, sort, eval))
        }
    }

    fn new_bvdiv(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bitv()?;
        y.expect_bitv()?;

        x.expect_same_sort_with(&y)?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bitv(x.get_size());
        let eval = if !symbolized {
            assert!(y.evaluate() != 0);
            (x.evaluate() / y.evaluate()) & x.get_bitmask()
        } else {
            0
        };

        if x.is_zero() {
            Ok(self.new_const(sort, 0))
        } else if y.evaluate() == 1 {
            Ok(x)
        } else {
            Ok(self.new_pure_maybe_const(PureCode::Div, vec![x, y], symbolized, sort, eval))
        }
    }

    fn new_bvsdiv(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bitv()?;
        y.expect_bitv()?;

        x.expect_same_sort_with(&y)?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bitv(x.get_size());
        let eval = if !symbolized {
            assert!(y.evaluate() != 0);
            ((x.evaluate() as i64 / y.evaluate() as i64) as u64) & x.get_bitmask()
        } else {
            0
        };

        if x.is_zero() {
            Ok(self.new_const(sort, 0))
        } else if y.evaluate() == 1 {
            Ok(x)
        } else {
            Ok(self.new_pure_maybe_const(PureCode::Sdiv, vec![x, y], symbolized, sort, eval))
        }
    }

    fn new_bvmod(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bitv()?;
        y.expect_bitv()?;

        x.expect_same_sort_with(&y)?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bitv(x.get_size());
        let eval = if !symbolized {
            assert!(y.evaluate() != 0);
            (x.evaluate() % y.evaluate()) & x.get_bitmask()
        } else {
            0
        };

        if x.is_zero() {
            Ok(self.new_const(sort, 0))
        } else if y.evaluate() == 1 {
            Ok(x)
        } else {
            Ok(self.new_pure_maybe_const(PureCode::Mod, vec![x, y], symbolized, sort, eval))
        }
    }

    fn new_bvsmod(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bitv()?;
        y.expect_bitv()?;

        x.expect_same_sort_with(&y)?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bitv(x.get_size());
        let eval = if !symbolized {
            assert!(y.evaluate() != 0);
            ((x.evaluate() as i64 % y.evaluate() as i64) as u64) & x.get_bitmask()
        } else {
            0
        };

        if x.is_zero() {
            Ok(self.new_const(sort, 0))
        } else if y.evaluate() == 1 {
            Ok(x)
        } else {
            Ok(self.new_pure_maybe_const(PureCode::Smod, vec![x, y], symbolized, sort, eval))
        }
    }

    fn new_logand(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bitv()?;
        y.expect_bitv()?;

        x.expect_same_sort_with(&y)?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bitv(x.get_size());
        let eval = if !symbolized {
            x.evaluate() | y.evaluate()
        } else {
            0
        };

        if x.is_zero() {
            Ok(y)
        } else if y.is_zero() {
            Ok(x)
        } else {
            Ok(self.new_pure_maybe_const(PureCode::LogOr, vec![x, y], symbolized, sort, eval))
        }
    }

    fn new_logor(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bitv()?;
        y.expect_bitv()?;

        x.expect_same_sort_with(&y)?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bitv(x.get_size());
        let eval = if !symbolized {
            x.evaluate() & y.evaluate()
        } else {
            0
        };

        if x.is_zero() || y.is_zero() {
            Ok(self.new_const(sort, 0))
        } else {
            Ok(self.new_pure_maybe_const(PureCode::LogOr, vec![x, y], symbolized, sort, eval))
        }
    }

    fn new_logxor(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bitv()?;
        y.expect_bitv()?;

        x.expect_same_sort_with(&y)?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bitv(x.get_size());
        let eval = if !symbolized {
            x.evaluate() ^ y.evaluate()
        } else {
            0
        };

        if x.is_zero() {
            Ok(y)
        } else if y.is_zero() {
            Ok(x)
        } else if x.get_hash() == y.get_hash() {
            // xor eax, eax
            Ok(self.new_const(sort, 0))
        } else {
            Ok(self.new_pure_maybe_const(PureCode::LogXor, vec![x, y], symbolized, sort, eval))
        }
    }

    fn new_bvshr(&self, fill_bit: PureRef, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bitv()?;
        y.expect_bitv()?;

        x.expect_same_sort_with(&y)?;

        let fill_bit = self.convert_bool_to_bitv(fill_bit)?;
        let symbolized = fill_bit.is_symbolized() | x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bitv(x.get_size());
        let eval = if !symbolized {
            ((x.evaluate() >> y.evaluate())
                | if fill_bit.evaluate() != 0 {
                    u64::MAX & !(x.get_bitmask() >> y.evaluate())
                } else {
                    0
                })
                & x.get_bitmask()
        } else {
            0
        };

        Ok(self.new_pure_maybe_const(
            PureCode::ShiftRight,
            vec![fill_bit, x, y],
            symbolized,
            sort,
            eval,
        ))
    }

    fn new_bvshl(&self, fill_bit: PureRef, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bitv()?;
        y.expect_bitv()?;

        x.expect_same_sort_with(&y)?;

        let fill_bit = self.convert_bool_to_bitv(fill_bit)?;
        let symbolized = fill_bit.is_symbolized() | x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bitv(x.get_size());
        let eval = if !symbolized {
            ((x.evaluate() << y.evaluate())
                | if fill_bit.evaluate() != 0 {
                    (1 << y.evaluate()) - 1
                } else {
                    0
                })
                & x.get_bitmask()
        } else {
            0
        };

        Ok(self.new_pure_maybe_const(
            PureCode::ShiftLeft,
            vec![fill_bit, x, y],
            symbolized,
            sort,
            eval,
        ))
    }

    fn new_eq(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bitv()?;
        y.expect_bitv()?;

        x.expect_same_sort_with(&y)?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bool;
        let eval = if !symbolized {
            if x.evaluate() == y.evaluate() {
                1
            } else {
                0
            }
        } else {
            0
        };

        Ok(self.new_pure_maybe_const(PureCode::Equal, vec![x, y], symbolized, sort, eval))
    }

    fn new_sle(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bitv()?;
        y.expect_bitv()?;

        x.expect_same_sort_with(&y)?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bool;
        let eval = if !symbolized && (x.evaluate() as i64) >= (y.evaluate() as i64) {
            1
        } else {
            0
        };

        Ok(self.new_pure_maybe_const(PureCode::Sle, vec![x, y], symbolized, sort, eval))
    }

    fn new_ule(&self, x: PureRef, y: PureRef) -> Result<PureRef> {
        x.expect_bitv()?;
        y.expect_bitv()?;

        x.expect_same_sort_with(&y)?;

        let symbolized = x.is_symbolized() | y.is_symbolized();
        let sort = Sort::Bool;
        let eval = if !symbolized && x.evaluate() >= y.evaluate() {
            1
        } else {
            0
        };

        Ok(self.new_pure_maybe_const(PureCode::Ule, vec![x, y], symbolized, sort, eval))
    }

    fn new_cast(&self, fill_bit: PureRef, value: PureRef, length: usize) -> Result<PureRef> {
        value.expect_bitv()?;

        let fill_bit = self.convert_bool_to_bitv(fill_bit)?;
        let symbolized =
            fill_bit.is_symbolized() | value.is_symbolized() | fill_bit.is_symbolized();
        let sort = Sort::Bitv(length);
        let eval = if !symbolized {
            if value.get_size() >= length {
                value.evaluate() & (1 << (length - 1))
            } else {
                let fill_bits = (fill_bit.evaluate() << (length - 1)) & !value.get_bitmask();
                value.evaluate() | fill_bits
            }
        } else {
            0
        };

        if value.get_sort() == sort {
            Ok(value)
        } else {
            let expand = value.get_size() < sort.get_size();
            Ok(self.new_pure_maybe_const(
                PureCode::Cast(expand),
                vec![fill_bit, value],
                symbolized,
                sort,
                eval,
            ))
        }
    }

    fn new_append(&self, high: PureRef, low: PureRef) -> Result<PureRef> {
        high.expect_bitv()?;
        low.expect_bitv()?;

        let symbolized = high.is_symbolized() | low.is_symbolized();
        let sort = Sort::Bitv(high.get_size() + low.get_size());
        let eval = if !symbolized {
            (high.evaluate() << low.get_size()) | low.evaluate()
        } else {
            0
        };

        Ok(self.new_pure_maybe_const(PureCode::Append, vec![high, low], symbolized, sort, eval))
    }

    // extract bits of high down to low from value
    fn new_extract(&self, value: PureRef, high: u32, low: u32) -> Result<PureRef> {
        value.expect_bitv()?;

        assert!(high >= low);
        let symbolized = value.is_symbolized();
        let sort = Sort::Bitv((high - low + 1) as usize);
        let eval = {
            let outer_left = u64::BITS - high - 1;
            let outer_right = low;
            value.evaluate() << outer_left >> outer_left >> outer_right
        };

        Ok(self.new_pure_maybe_const(
            PureCode::Extract(high, low),
            vec![value],
            symbolized,
            sort,
            eval,
        ))
    }

    fn new_load(&self, key: PureRef) -> Result<PureRef> {
        key.expect_bitv()?;

        let symbolized = true;
        let sort = Sort::Bitv(u8::BITS as usize);
        let eval = 0;
        Ok(self.new_pure(PureCode::Load, vec![key], symbolized, sort, eval))
    }

    fn new_loadw(&self, key: PureRef, bits: u64) -> Result<PureRef> {
        key.expect_bitv()?;

        let symbolized = true;
        let sort = Sort::Bitv(bits as usize);
        let eval = 0;
        Ok(self.new_pure(PureCode::Load, vec![key], symbolized, sort, eval))
    }

    // Effect
    fn new_effect(&self, effect: Effect) -> Rc<Effect> {
        Rc::new(effect)
    }

    fn new_nop(&self) -> Rc<Effect> {
        self.new_effect(Effect::Nop)
    }

    fn new_seq(&self, args: Vec<Rc<Effect>>) -> Rc<Effect> {
        self.new_effect(Effect::Seq { args })
    }
    fn new_jmp(&self, dst: PureRef) -> Result<Rc<Effect>> {
        dst.expect_bitv()?;
        Ok(self.new_effect(Effect::Jmp { dst }))
    }

    fn new_branch(
        &self,
        condition: PureRef,
        then: Rc<Effect>,
        otherwise: Rc<Effect>,
    ) -> Result<Rc<Effect>> {
        condition.expect_bool()?;
        if condition.is_concretized() {
            if condition.evaluate_bool() {
                Ok(then)
            } else {
                Ok(otherwise)
            }
        } else {
            Ok(self.new_effect(Effect::Branch {
                condition,
                then,
                otherwise,
            }))
        }
    }

    fn new_store(&self, key: PureRef, value: PureRef) -> Result<Rc<Effect>> {
        key.expect_bitv()?;
        value.expect_bitv()?;
        Ok(self.new_effect(Effect::Store { key, value }))
    }
}
