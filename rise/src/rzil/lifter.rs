use super::{
    ast::{Effect, PureCode, PureRef, Scope, Sort},
    builder::RzILBuilder,
    error::{Result, RzILError},
};
use crate::variables::Variables;
use bitflags::bitflags;
use rzapi::structs::RzILInfo;
use std::{collections::HashMap, rc::Rc};

bitflags! {
    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct RzILConfig: u64 {
        const ConvertBranchSetToSetIte      = 0b00000001;
        const AnalyzeDependencies = 0b00000010;
        const All = Self::ConvertBranchSetToSetIte.bits() | Self::AnalyzeDependencies.bits();
    }
}

#[derive(Clone, Debug)]
struct SetInBranchEntry {
    condition: PureRef,
    then: Option<PureRef>,
    otherwise: Option<PureRef>,
    default: PureRef,
}

#[derive(Clone, Debug)]
struct BranchSetToSetIte {
    conditions: Vec<PureRef>,
    taken: Vec<bool>,
    entries: HashMap<String, SetInBranchEntry>,
}

impl BranchSetToSetIte {
    fn clear(&mut self) {
        self.conditions.clear();
        self.taken.clear();
        self.entries.clear();
    }

    fn inside_branch(&self) -> bool {
        self.conditions.len() > 0
    }

    fn begin(&mut self, condition: PureRef) {
        self.conditions.push(condition);
        self.taken.push(true);
    }

    fn otherwise(&mut self) {
        self.taken.pop();
        self.taken.push(false);
    }

    fn end(&mut self) {
        self.taken.pop();
        self.conditions.pop();
    }

    fn connect_condition(&self, rzil: &impl RzILBuilder) -> Result<PureRef> {
        let len = self.conditions.len();
        let mut x = self.conditions.last().unwrap().clone();
        for i in (0..len - 1).rev() {
            if !x.is_bool() {
                return Err(RzILError::UnexpectedSort(Sort::Bool, x.get_sort()));
            }
            if i < len - 2 && !self.taken[i + 1] {
                x = rzil.new_boolinv(x)?;
            }
            let y = self.conditions[i].clone();
            x = rzil.new_booland(x, y)?;
        }
        Ok(x)
    }

    fn add_entry(
        &mut self,
        rzil: &impl RzILBuilder,
        vars: &mut Variables,
        name: &str,
        src: PureRef,
    ) -> Result<()> {
        let condition = self.connect_condition(rzil)?;
        let taken = *self.taken.first().unwrap();
        let mut entry = self.entries.get_mut(name);
        if let Some(Scope::Let) = vars.get_scope(name) {
            // let var is immutable
            return Err(RzILError::ImmutableVariable(name.to_string()));
        }
        match entry {
            Some(e) => {
                if taken {
                    if e.then.is_some() {
                        return Err(RzILError::DoubleSetInBranch);
                    }
                    e.then = Some(src);
                } else {
                    if e.otherwise.is_some() {
                        return Err(RzILError::DoubleSetInBranch);
                    }
                    e.otherwise = Some(src);
                }
            }
            None => {
                let default = match vars.get_var(name) {
                    Some(var) => var.clone(),
                    None => {
                        // this constant value won't be used.
                        rzil.new_const(src.get_sort(), 0)
                    }
                };
                let (mut then, mut otherwise) = (None, None);
                if taken {
                    then = Some(src);
                } else {
                    otherwise = Some(src);
                }
                self.entries.insert(
                    name.to_string(),
                    SetInBranchEntry {
                        condition,
                        then,
                        otherwise,
                        default,
                    },
                );
            }
        };
        Ok(())
    }

    fn drain(&mut self, rzil: &impl RzILBuilder, vars: &mut Variables) -> Result<Vec<Rc<Effect>>> {
        let entries: Vec<(String, SetInBranchEntry)> = self.entries.drain().collect();
        let mut set_ite_ops = Vec::new();
        self.clear();
        for (name, e) in entries {
            let condition = e.condition.clone();
            let then = e.then.clone().unwrap_or(e.default.clone());
            let otherwise = e.otherwise.clone().unwrap_or(e.default.clone());
            let ite = rzil.new_ite(condition, then, otherwise)?;
            let set_op = rzil.new_set(vars, &name, ite)?;
            set_ite_ops.push(set_op);
        }
        Ok(set_ite_ops)
    }
}

/*
 * Refactoring Plan:
 *
 *      create Pure/Effect
 *  RzIL <-  Lifter
 *               | ask if a referenced variable is global(register) or local
 *  Storage <---
 *      save global/local var (maybe per context)
 *
 */
#[derive(Clone, Debug)]
pub struct RzILLifter {
    //option: RzILConfig,
    bs_to_si: BranchSetToSetIte,
    tmp_vars: HashMap<String, PureRef>, // store LocalPure vars
}

impl RzILLifter {
    pub fn new() -> Self {
        RzILLifter {
            bs_to_si: BranchSetToSetIte {
                conditions: Vec::new(),
                taken: Vec::new(),
                entries: HashMap::new(),
            },
            tmp_vars: HashMap::new(),
        }
    }

    fn parse_pure(
        &mut self,
        rzil: &impl RzILBuilder,
        vars: &mut Variables,
        op: &RzILInfo,
    ) -> Result<PureRef> {
        match op {
            RzILInfo::Var { value } => match vars.get_var(value) {
                Some(var) => {
                    if var.is_concretized() {
                        Ok(rzil.new_const(var.get_sort(), var.evaluate()))
                    } else {
                        Ok(var)
                    }
                }
                None => match self.tmp_vars.get(value) {
                    Some(var) => {
                        if var.is_concretized() {
                            Ok(rzil.new_const(var.get_sort(), var.evaluate()))
                        } else {
                            Ok(var.clone())
                        }
                    }
                    None => Err(RzILError::UndefinedVariableReferenced(value.clone())),
                },
            },
            RzILInfo::Ite { condition, x, y } => {
                let condition = self.parse_pure(rzil, vars, condition)?;
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_ite(condition, x, y)
            }
            RzILInfo::Let { dst, exp, body } => {
                let name = dst.to_string();
                let exp = self.parse_pure(rzil, vars, exp)?;
                let dst = rzil.new_let_var(vars.get_uniq_id(dst), exp);
                if self.tmp_vars.insert(name.clone(), dst.clone()).is_some() {
                    return Err(RzILError::ImmutableVariable(name));
                }
                let body = self.parse_pure(rzil, vars, body)?;
                self.tmp_vars.remove(&name);
                rzil.new_let(dst, body)
            }
            RzILInfo::Bool { value } => {
                let sort = Sort::Bool;
                let val = if *value { 1 } else { 0 };
                Ok(rzil.new_const(sort, val))
            }
            RzILInfo::BoolInv { x } => {
                let x = self.parse_pure(rzil, vars, x)?;
                rzil.new_boolinv(x)
            }
            RzILInfo::BoolAnd { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_booland(x, y)
            }
            RzILInfo::BoolOr { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_boolor(x, y)
            }
            RzILInfo::BoolXor { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_boolxor(x, y)
            }
            RzILInfo::Bitv { bits, len } => {
                let sort = Sort::Bitv(*len);
                let val = u64::from_str_radix(&bits[2..], 16)?;
                Ok(rzil.new_const(sort, val))
            }
            RzILInfo::Msb { bv } => {
                let bitv = self.parse_pure(rzil, vars, bv)?;
                rzil.new_msb(bitv)
            }
            RzILInfo::Lsb { bv } => {
                let bitv = self.parse_pure(rzil, vars, bv)?;
                rzil.new_lsb(bitv)
            }
            RzILInfo::IsZero { bv } => {
                let bitv = self.parse_pure(rzil, vars, bv)?;
                rzil.new_is_zero(bitv)
            }
            RzILInfo::Neg { bv } => {
                let bitv = self.parse_pure(rzil, vars, bv)?;
                rzil.new_neg(bitv)
            }
            RzILInfo::LogNot { bv } => {
                let bitv = self.parse_pure(rzil, vars, bv)?;
                rzil.new_lognot(bitv)
            }
            RzILInfo::Add { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_bvadd(x, y)
            }
            RzILInfo::Sub { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_bvsub(x, y)
            }
            RzILInfo::Mul { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_bvmul(x, y)
            }
            RzILInfo::Div { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_bvdiv(x, y)
            }
            RzILInfo::Sdiv { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_bvsdiv(x, y)
            }
            RzILInfo::Mod { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_bvmod(x, y)
            }
            RzILInfo::Smod { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_bvsmod(x, y)
            }
            RzILInfo::LogAnd { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_logand(x, y)
            }
            RzILInfo::LogOr { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_logor(x, y)
            }
            RzILInfo::LogXor { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_logxor(x, y)
            }
            RzILInfo::ShiftRight { fill_bit, x, y } => {
                let fill_bit = self.parse_pure(rzil, vars, fill_bit)?;
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_bvshr(fill_bit, x, y)
            }
            RzILInfo::ShiftLeft { fill_bit, x, y } => {
                let fill_bit = self.parse_pure(rzil, vars, fill_bit)?;
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_bvshl(fill_bit, x, y)
            }
            RzILInfo::Equal { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_eq(x, y)
            }
            RzILInfo::Sle { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_sle(x, y)
            }
            RzILInfo::Ule { x, y } => {
                let x = self.parse_pure(rzil, vars, x)?;
                let y = self.parse_pure(rzil, vars, y)?;
                rzil.new_ule(x, y)
            }
            RzILInfo::Cast {
                value,
                length,
                fill,
            } => {
                let fill_bit = self.parse_pure(rzil, vars, fill)?;
                let value = self.parse_pure(rzil, vars, value)?;
                rzil.new_cast(fill_bit, value, *length)
            }
            RzILInfo::Append { high, low } => {
                let high = self.parse_pure(rzil, vars, high)?;
                let low = self.parse_pure(rzil, vars, low)?;
                rzil.new_append(high, low)
            }
            RzILInfo::Load { mem: _, key } => {
                let key = self.parse_pure(rzil, vars, key)?;
                rzil.new_load(key)
            }
            RzILInfo::Loadw { mem: _, key, bits } => {
                let key = self.parse_pure(rzil, vars, key)?;
                rzil.new_loadw(key, *bits)
            }
            RzILInfo::Float { format: _, bv: _ } => {
                Err(RzILError::UnimplementedRzILPure(PureCode::Float))
            }
            RzILInfo::Fbits { f: _ } => Err(RzILError::UnimplementedRzILPure(PureCode::Fbits)),
            RzILInfo::IsFinite { f: _ } => {
                Err(RzILError::UnimplementedRzILPure(PureCode::IsFinite))
            }
            RzILInfo::IsNan { f: _ } => Err(RzILError::UnimplementedRzILPure(PureCode::IsNan)),
            RzILInfo::IsInf { f: _ } => Err(RzILError::UnimplementedRzILPure(PureCode::IsInf)),
            RzILInfo::IsFzero { f: _ } => Err(RzILError::UnimplementedRzILPure(PureCode::IsFzero)),
            RzILInfo::IsFneg { f: _ } => Err(RzILError::UnimplementedRzILPure(PureCode::IsFneg)),
            RzILInfo::IsFpos { f: _ } => Err(RzILError::UnimplementedRzILPure(PureCode::IsFpos)),
            RzILInfo::Fneg { f: _ } => Err(RzILError::UnimplementedRzILPure(PureCode::Fneg)),
            RzILInfo::Fpos { f: _ } => Err(RzILError::UnimplementedRzILPure(PureCode::Fneg)),
            RzILInfo::FcastInt {
                length: _,
                rmode: _,
                value: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::FcastInt)),
            RzILInfo::FcastSint {
                length: _,
                rmode: _,
                value: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::FcastSint)),
            RzILInfo::FcastFloat {
                format: _,
                rmode: _,
                value: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::FcastFloat)),
            RzILInfo::FcastSfloat {
                format: _,
                rmode: _,
                value: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::FcastSfloat)),
            RzILInfo::Fconvert {
                format: _,
                rmode: _,
                value: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fconvert)),
            RzILInfo::Fround { rmode: _, value: _ } => {
                Err(RzILError::UnimplementedRzILPure(PureCode::Fround))
            }
            RzILInfo::Frequal {
                rmode_x: _,
                rmode_y: _,
                value: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Frequal)),
            RzILInfo::Fsucc { f: _ } => Err(RzILError::UnimplementedRzILPure(PureCode::Fsucc)),
            RzILInfo::Fpred { f: _ } => Err(RzILError::UnimplementedRzILPure(PureCode::Fpred)),
            RzILInfo::Forder { x: _, y: _ } => {
                Err(RzILError::UnimplementedRzILPure(PureCode::Forder))
            }
            RzILInfo::Fsqrt { rmode: _, f: _ } => {
                Err(RzILError::UnimplementedRzILPure(PureCode::Fsqrt))
            }
            RzILInfo::Frsqrt { rmode: _, f: _ } => {
                Err(RzILError::UnimplementedRzILPure(PureCode::Frsqrt))
            }
            RzILInfo::Fadd {
                rmode: _,
                x: _,
                y: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fadd)),
            RzILInfo::Fsub {
                rmode: _,
                x: _,
                y: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fsub)),
            RzILInfo::Fmul {
                rmode: _,
                x: _,
                y: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fmul)),
            RzILInfo::Fdiv {
                rmode: _,
                x: _,
                y: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fdiv)),
            RzILInfo::Fmod {
                rmode: _,
                x: _,
                y: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fmod)),
            RzILInfo::Hypot {
                rmode: _,
                x: _,
                y: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Hypot)),
            RzILInfo::Pow {
                rmode: _,
                x: _,
                y: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Pow)),
            RzILInfo::Fmad {
                rmode: _,
                x: _,
                y: _,
                z: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fmad)),
            RzILInfo::Fpown {
                rmode: _,
                f: _,
                n: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fpown)),
            RzILInfo::Frootn {
                rmode: _,
                f: _,
                n: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Frootn)),
            RzILInfo::Fcompound {
                rmode: _,
                f: _,
                n: _,
            } => Err(RzILError::UnimplementedRzILPure(PureCode::Fcompound)),
            _ => Err(RzILError::UnkownPure),
        }
    }

    pub fn parse_effect(
        &mut self,
        rzil: &impl RzILBuilder,
        vars: &mut Variables,
        op: &RzILInfo,
    ) -> Result<Rc<Effect>> {
        match op {
            RzILInfo::Nop => Ok(rzil.new_nop()),
            RzILInfo::Set { dst, src } => {
                let name = dst;
                let src = self.parse_pure(rzil, vars, src)?;
                if self.bs_to_si.inside_branch() {
                    self.bs_to_si.add_entry(rzil, vars, name, src)?;
                    return Err(RzILError::None);
                }
                rzil.new_set(vars, name, src)
            }

            RzILInfo::Jmp { dst } => {
                let dst = self.parse_pure(rzil, vars, dst)?;
                rzil.new_jmp(dst)
            }

            RzILInfo::Goto { label } => Ok(rzil.new_effect(Effect::Goto {
                label: label.to_owned(),
            })),
            RzILInfo::Seq { x, y } => {
                let mut args = Vec::new();
                self.parse_seq_arg(rzil, vars, x, &mut args)?;
                self.parse_seq_arg(rzil, vars, y, &mut args)?;
                if args.is_empty() {
                    return Ok(rzil.new_nop());
                } else if args.len() == 1 {
                    return Ok(args.pop().unwrap());
                }
                Ok(rzil.new_effect(Effect::Seq { args }))
            }
            RzILInfo::Branch {
                condition,
                true_eff,
                false_eff,
            } => {
                let condition = self.parse_pure(rzil, vars, condition)?;

                // === Branch Begin ===
                self.bs_to_si.begin(condition.clone());

                // Then
                let then = self.parse_effect(rzil, vars, true_eff)?;

                // Otherwise
                self.bs_to_si.otherwise();
                let otherwise = self.parse_effect(rzil, vars, false_eff)?;

                self.bs_to_si.end();
                // ==== Branch End ====

                let mut args = if !self.bs_to_si.inside_branch() {
                    self.bs_to_si.drain(rzil, vars)?
                } else {
                    // in case of nested branches
                    Vec::new()
                };
                let need_branch = !then.is_nop() || !otherwise.is_nop();
                if need_branch {
                    // if all child ops are opt-outed, we don't need a branch op
                    args.push(rzil.new_branch(condition, then, otherwise)?);
                }
                if args.is_empty() {
                    Ok(rzil.new_nop())
                } else if args.len() == 1 {
                    Ok(args.pop().unwrap())
                } else {
                    Ok(rzil.new_seq(args))
                }
            }
            RzILInfo::Store { mem: _, key, value } => {
                let key = self.parse_pure(rzil, vars, key)?;
                let value = self.parse_pure(rzil, vars, value)?;
                rzil.new_store(key, value)
            }
            RzILInfo::Storew { mem: _, key, value } => {
                let key = self.parse_pure(rzil, vars, key)?;
                let value = self.parse_pure(rzil, vars, value)?;
                rzil.new_store(key, value)
            }
            RzILInfo::Blk {
                label: _,
                data: _,
                ctrl: _,
            } => Err(RzILError::UnimplementedRzILEffect("Blk".to_string())),
            RzILInfo::Repeat {
                condition: _,
                data_eff: _,
            } => Err(RzILError::UnimplementedRzILEffect("Repeat".to_string())), //Ok(rzil.nop()), //
            RzILInfo::Empty => Err(RzILError::Empty),
            _ => Err(RzILError::UnkownPure),
        }
    }

    fn parse_seq_arg(
        &mut self,
        rzil: &impl RzILBuilder,
        vars: &mut Variables,
        seq_arg: &RzILInfo,
        vec: &mut Vec<Rc<Effect>>,
    ) -> Result<()> {
        match seq_arg {
            RzILInfo::Seq { x, y } => {
                // nested Seq
                self.parse_seq_arg(rzil, vars, x, vec)?;
                self.parse_seq_arg(rzil, vars, y, vec)?;
            }
            _ => {
                match self.parse_effect(rzil, vars, seq_arg) {
                    Ok(ret) => match &*ret {
                        Effect::Seq { args } => {
                            // later generated Seq
                            vec.extend_from_slice(args);
                        }
                        _ => vec.push(ret),
                    },
                    Err(RzILError::None) => (),
                    Err(err) => return Err(err),
                };
            }
        };
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use std::{fs, rc::Rc};

    use rzapi::{api::RzApi, structs::RzILInfo};

    use super::RzILLifter;
    use crate::{
        registers::bind_registers,
        rzil::{ast::Effect, builder::RzILCache},
        variables::Variables,
    };
    /*
    fn init(rzapi: &mut RzApi) -> (RiseContext, Variables) {
        let s = Z3Solver::new();
        let r = RzILCache::new();
        let mut ctx = RiseContext::new(s, r);
        let mut vars = Variables::new();
        bind_registers(rzapi, &mut vars).unwrap();
        (ctx, vars)
    }
    */

    fn read_rzil_info(path: &str) -> RzILInfo {
        let file = fs::File::open(path).expect("unable to open file");
        serde_json::from_reader(file).expect("invalid json file")
    }

    fn parse_ops(target: Vec<&str>) -> Vec<Rc<Effect>> {
        let rzil = RzILCache::new();
        let mut lifter = RzILLifter::new();
        let mut rzapi = RzApi::new(Some("test/sample")).unwrap();
        let mut vars = Variables::new();
        bind_registers(&mut rzapi, &mut vars, &rzil).unwrap();
        let mut ret = Vec::new();
        for t in target {
            let path = format!("test/{}.json", t);
            let op_info = read_rzil_info(&path);
            ret.push(lifter.parse_effect(&rzil, &mut vars, &op_info).unwrap());
            vars.partial_clear();
        }
        ret
    }

    fn parse_op(target: &str) -> Rc<Effect> {
        parse_ops(vec![target]).pop().unwrap()
    }

    #[test]
    fn xor() {
        dbg!(parse_op("xor_eax_eax"));
    }

    #[test]
    fn sub() {
        dbg!(parse_op("sub_al_0x20"));
    }

    #[test]
    fn ret() {
        dbg!(parse_op("ret"));
    }

    #[test]
    fn shr() {
        dbg!(parse_op("shr_rsi_0x3f"));
    }

    #[test]
    fn push() {
        dbg!(parse_op("push_qword_rip+0x2fa2"));
    }

    #[test]
    fn and() {
        dbg!(parse_op("and_rsp_-0xf"));
    }

    #[test]
    fn multiple() {
        let ops = parse_ops(vec![
            "sub_al_0x20",
            "shr_rsi_0x3f",
            "xor_eax_eax",
            "push_qword_rip+0x2fa2",
            "and_rsp_-0xf",
            "ret",
        ]);
        for op in ops {
            dbg!(op);
        }
    }
}
