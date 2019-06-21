// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use std::rc::Rc;

use crate::ast::*;
use crate::value::*;

mod i64_aux {
    pub fn checked_exp(base: i64, exponent: i64) -> Option<i64> {
        let mut exponent = exponent;
        if exponent < 0 {
            None
        } else {
            let mut base_opt = Some(base);
            let mut res = 1;
            while exponent > 0 {
                let base = base_opt?;
                if exponent & 1 == 1 {
                    res = i64::checked_mul(res, base)?;
                }
                base_opt = i64::checked_mul(base, base);
                exponent >>= 1;
            }
            Some(res)
        }
    }
}

impl Builtin {
    pub fn arity(self) -> usize {
        use self::Builtin::*;
        match self {
            EqualBool => 2,

            AddInt64 => 2,
            SubInt64 => 2,
            MulInt64 => 2,
            DivInt64 => 2,
            ModInt64 => 2,
            ExpInt64 => 2,

            EqualInt64 => 2,
            LeqInt64 => 2,
            GeqInt64 => 2,
            LessInt64 => 2,
            GreaterInt64 => 2,

            AppendText => 2,
            ImplodeText => 1,
            ExplodeText => 1,
            Sha256Text => 1,
            TextToCodePoints => 1,
            TextFromCodePoints => 1,

            EqualText => 2,
            LeqText => 2,
            GeqText => 2,
            LessText => 2,
            GreaterText => 2,

            EqualParty => 2,
            LeqParty => 2,
            GeqParty => 2,
            LessParty => 2,
            GreaterParty => 2,

            EqualContractId => 2,
            CoerceContractId => 1,

            EqualTime => 2,
            LeqTime => 2,
            GeqTime => 2,
            LessTime => 2,
            GreaterTime => 2,

            TimeToMicrosSinceEpoch => 1,
            TimeFromMicrosSinceEpoch => 1,

            EqualDate => 2,
            LeqDate => 2,
            GeqDate => 2,
            LessDate => 2,
            GreaterDate => 2,

            DateToDaysSinceEpoch => 1,
            DateFromDaysSinceEpoch => 1,

            Int64ToText => 1,
            TextToText => 1,
            PartyToText => 1,
            PartyToQuotedText => 1,
            TimeToText => 1,
            DateToText => 1,

            Int64FromText => 1,
            PartyFromText => 1,
            GetParty => 1,

            Cons => 2,
            Foldr => 3,
            Foldl => 3,
            EqualList => 3,

            Some => 1,
            Error => 1,

            MapInsert => 3,
            MapLookup => 2,
            MapDelete => 2,
            MapToList => 1,
            MapSize => 1,

            Unsupported(x) => panic!("Builtin::Unsupported {:?}", x),
        }
    }

    fn binop_int64<'a, F>(f: F, op: &str, args: &[Rc<Value<'a>>]) -> Result<Value<'a>, String>
    where
        F: FnOnce(i64, i64) -> Option<i64>,
    {
        let x = args[0].as_i64();
        let y = args[1].as_i64();
        match f(x, y) {
            None => Err(format!("Arithmetic error: {} {} {}", x, op, y)),
            Some(n) => Ok(Value::Int64(n)),
        }
    }

    pub fn interpret<'a>(
        self,
        args: &[Rc<Value<'a>>],
        world: &'a World,
    ) -> Result<Value<'a>, String> {
        use self::Builtin::*;
        assert!(
            args.len() == self.arity(),
            "Bad arity for builtin {:?}: {}",
            self,
            args.len()
        );
        match self {
            EqualBool => Ok(Value::Bool(args[0].as_bool() == args[1].as_bool())),

            AddInt64 => Self::binop_int64(i64::checked_add, "+", args),
            SubInt64 => Self::binop_int64(i64::checked_sub, "-", args),
            MulInt64 => Self::binop_int64(i64::checked_mul, "*", args),
            DivInt64 => Self::binop_int64(i64::checked_div, "/", args),
            ModInt64 => Self::binop_int64(i64::checked_rem, "%", args),
            ExpInt64 => Self::binop_int64(i64_aux::checked_exp, "^", args),

            EqualInt64 => Ok(Value::Bool(args[0].as_i64() == args[1].as_i64())),
            LeqInt64 => Ok(Value::Bool(args[0].as_i64() <= args[1].as_i64())),
            GeqInt64 => Ok(Value::Bool(args[0].as_i64() >= args[1].as_i64())),
            LessInt64 => Ok(Value::Bool(args[0].as_i64() < args[1].as_i64())),
            GreaterInt64 => Ok(Value::Bool(args[0].as_i64() > args[1].as_i64())),

            AppendText => {
                let mut res = args[0].as_string().clone();
                res.push_str(args[1].as_string());
                Ok(Value::Text(res))
            }
            ImplodeText => {
                let mut res = String::new();
                for val in args[0].as_list() {
                    res.push_str(val.as_string());
                }
                Ok(Value::Text(res))
            }
            ExplodeText => Ok(Value::from_list(
                args[0]
                    .as_string()
                    .chars()
                    .map(|c| Rc::new(Value::Text(c.to_string()))),
            )),
            Sha256Text => {
                use sha2::Digest;
                let arg = args[0].as_string();
                let hash = sha2::Sha256::digest(arg.as_bytes());
                Ok(Value::Text(hex::encode(hash)))
            }
            // FIXME(MH): Both primitives do not operate with unicode code points
            // but with unicode scalar values. This is against the DAML-LF spec.
            TextToCodePoints => Ok(Value::from_list(
                args[0]
                    .as_string()
                    .chars()
                    .map(|c| Rc::new(Value::Int64(c as i64))),
            )),
            TextFromCodePoints => {
                use std::convert::TryFrom;
                let t: Result<String, i64> = args[0]
                    .as_list()
                    .map(|v| {
                        let i = v.as_i64();
                        let w = u32::try_from(i).map_err(|_| i)?;
                        let c = char::try_from(w).map_err(|_| i)?;
                        Ok(c)
                    })
                    .collect();
                let t = t.map_err(|i| format!("invalid code point: {}", i))?;
                Ok(Value::Text(t))
            }

            EqualText => Ok(Value::Bool(args[0].as_string() == args[1].as_string())),
            LeqText => Ok(Value::Bool(args[0].as_string() <= args[1].as_string())),
            GeqText => Ok(Value::Bool(args[0].as_string() >= args[1].as_string())),
            LessText => Ok(Value::Bool(args[0].as_string() < args[1].as_string())),
            GreaterText => Ok(Value::Bool(args[0].as_string() > args[1].as_string())),

            EqualParty => Ok(Value::Bool(args[0].as_party() == args[1].as_party())),
            LeqParty => Ok(Value::Bool(args[0].as_party() <= args[1].as_party())),
            GeqParty => Ok(Value::Bool(args[0].as_party() >= args[1].as_party())),
            LessParty => Ok(Value::Bool(args[0].as_party() < args[1].as_party())),
            GreaterParty => Ok(Value::Bool(args[0].as_party() > args[1].as_party())),

            EqualContractId => Ok(Value::Bool(
                args[0].as_contract_id() == args[1].as_contract_id(),
            )),
            CoerceContractId => Ok(Value::ContractId(args[0].as_contract_id().clone())),

            EqualTime => Ok(Value::Bool(args[0].as_time() == args[1].as_time())),
            LeqTime => Ok(Value::Bool(args[0].as_time() <= args[1].as_time())),
            GeqTime => Ok(Value::Bool(args[0].as_time() >= args[1].as_time())),
            LessTime => Ok(Value::Bool(args[0].as_time() < args[1].as_time())),
            GreaterTime => Ok(Value::Bool(args[0].as_time() > args[1].as_time())),

            TimeToMicrosSinceEpoch => Ok(Value::Int64(args[0].as_time().to_micros_since_epoch())),
            TimeFromMicrosSinceEpoch => {
                Ok(Value::Time(Time::from_micros_since_epoch(args[0].as_i64())))
            }

            EqualDate => Ok(Value::Bool(args[0].as_date() == args[1].as_date())),
            LeqDate => Ok(Value::Bool(args[0].as_date() <= args[1].as_date())),
            GeqDate => Ok(Value::Bool(args[0].as_date() >= args[1].as_date())),
            LessDate => Ok(Value::Bool(args[0].as_date() < args[1].as_date())),
            GreaterDate => Ok(Value::Bool(args[0].as_date() > args[1].as_date())),

            DateToDaysSinceEpoch => Ok(Value::Int64(args[0].as_date().to_days_since_epoch())),
            DateFromDaysSinceEpoch => {
                Ok(Value::Date(Date::from_days_since_epoch(args[0].as_i64())))
            }

            Int64ToText => Ok(Value::Text(args[0].as_i64().to_string())),
            // NOTE(MH): We handle `TextToText` special to avoid cloning.
            TextToText => panic!("Builtin::TextToText is handled in step"),
            PartyToText => Ok(Value::Text(args[0].as_party().to_string())),
            PartyToQuotedText => Ok(Value::Text(format!("'{}'", args[0].as_party()))),
            TimeToText => Ok(Value::Text(args[0].as_time().to_string())),
            DateToText => Ok(Value::Text(args[0].as_date().to_string())),

            Int64FromText => Ok(match args[0].as_string().parse() {
                Err(_) => Value::None,
                Ok(n) => Value::Some(Rc::new(Value::Int64(n))),
            }),
            PartyFromText => Ok(match args[0].as_string().parse() {
                Err(_) => Value::None,
                Ok(p) => Value::Some(Rc::new(Value::Party(p))),
            }),
            GetParty => args[0].as_string().parse().map(Value::Party),

            Cons => {
                let head = Rc::clone(&args[0]);
                let tail = Rc::clone(&args[1]);
                Ok(Value::Cons(head, tail))
            }
            Foldr => panic!("Builtin::Foldr is handled in step"),
            Foldl => panic!("Builtin::Foldl is handled in step"),
            EqualList => panic!("Builtin::EqualLit is handled in step"),

            Some => {
                let body = Rc::clone(&args[0]);
                Ok(Value::Some(body))
            }
            Error => Err(args[0].as_string().to_owned()),

            MapInsert => {
                let mut map = args[2].as_map().clone();
                map.insert(args[0].as_string().clone(), Rc::clone(&args[1]));
                Ok(Value::Map(map))
            }
            MapLookup => {
                let map = args[1].as_map();
                Ok(match map.get(args[0].as_string()) {
                    Option::None => Value::None,
                    Option::Some(v) => Value::Some(Rc::clone(v)),
                })
            }
            MapDelete => {
                let mut map = args[1].as_map().clone();
                map.remove(args[0].as_string());
                Ok(Value::Map(map))
            }
            MapToList => Ok(Value::from_list(args[0].as_map().iter().map(|(k, v)| {
                Rc::new(Value::TupleCon(
                    &world.map_entry_fields,
                    vec![Rc::new(Value::Text(k.to_string())), Rc::clone(v)],
                ))
            }))),
            MapSize => Ok(Value::Int64(args[0].as_map().len() as i64)),

            Unsupported(x) => panic!("Builtin::Unsupported {:?}", x),
        }
    }
}
