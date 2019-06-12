// Copyright (c) 2019 Digital Asset (Switzerland) GmbH and/or its affiliates. All rights reserved.
// SPDX-License-Identifier: Apache-2.0
use std::borrow::Borrow;
use std::rc::Rc;

use crate::ast::Builtin;
use crate::value::Value;

mod i64_aux {
    pub fn checked_exp(base: i64, exponent: i64) -> i64 {
        let mut exponent = exponent;
        if exponent < 0 {
            panic!("checked_exp: negavtive exponent");
        } else {
            let mut base_opt = Some(base);
            let mut res = 1;
            while exponent > 0 {
                let base = base_opt.expect("checked_exp: overflow");
                if exponent & 1 == 1 {
                    res = i64::checked_mul(res, base).expect("checked_exp: overflow");
                }
                base_opt = i64::checked_mul(base, base);
                exponent >>= 1;
            }
            res
        }
    }
}

pub fn arity(builtin: Builtin) -> usize {
    use self::Builtin::*;
    match builtin {
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

        Int64ToText => 1,
        TextToText => 1,
        PartyToText => 1,
        PartyToQuotedText => 1,

        Int64FromText => 1,
        PartyFromText => 1,

        Cons => 2,
        Foldr => 3,
        Foldl => 3,
        EqualList => 3,

        Some => 1,
        Error => 1,

        Unsupported(x) => panic!("Builtin::Unsupported {:?}", x),
    }
}

pub fn interpret<'a>(builtin: Builtin, args: &[Rc<Value<'a>>]) -> Value<'a> {
    use self::Builtin::*;
    assert!(
        args.len() == arity(builtin),
        "Bad arity for builtin {:?}: {}",
        builtin,
        args.len()
    );
    match builtin {
        EqualBool => Value::Bool(args[0].as_bool() == args[1].as_bool()),

        AddInt64 => Value::Int64(i64::checked_add(args[0].as_i64(), args[1].as_i64()).unwrap()),
        SubInt64 => Value::Int64(i64::checked_sub(args[0].as_i64(), args[1].as_i64()).unwrap()),
        MulInt64 => Value::Int64(i64::checked_mul(args[0].as_i64(), args[1].as_i64()).unwrap()),
        DivInt64 => Value::Int64(i64::checked_div(args[0].as_i64(), args[1].as_i64()).unwrap()),
        ModInt64 => Value::Int64(i64::checked_rem(args[0].as_i64(), args[1].as_i64()).unwrap()),
        ExpInt64 => Value::Int64(i64_aux::checked_exp(args[0].as_i64(), args[1].as_i64())),

        EqualInt64 => Value::Bool(args[0].as_i64() == args[1].as_i64()),
        LeqInt64 => Value::Bool(args[0].as_i64() <= args[1].as_i64()),
        GeqInt64 => Value::Bool(args[0].as_i64() >= args[1].as_i64()),
        LessInt64 => Value::Bool(args[0].as_i64() < args[1].as_i64()),
        GreaterInt64 => Value::Bool(args[0].as_i64() > args[1].as_i64()),

        AppendText => {
            let mut res = args[0].as_string().clone();
            res.push_str(args[1].as_string());
            Value::Text(res)
        }
        ImplodeText => {
            let mut res = String::new();
            for val in Value::make_list_iter(&args[0]) {
                res.push_str(val.as_string());
            }
            Value::Text(res)
        }
        ExplodeText => {
            let arg: &Value = args[0].borrow();
            let mut res = Value::Nil;
            for c in arg.as_string().chars().rev() {
                res = Value::Cons(Rc::new(Value::Text(c.to_string())), Rc::new(res));
            }
            res
        }

        EqualText => Value::Bool(args[0].as_string() == args[1].as_string()),
        LeqText => Value::Bool(args[0].as_string() <= args[1].as_string()),
        GeqText => Value::Bool(args[0].as_string() >= args[1].as_string()),
        LessText => Value::Bool(args[0].as_string() < args[1].as_string()),
        GreaterText => Value::Bool(args[0].as_string() > args[1].as_string()),

        EqualParty => Value::Bool(args[0].as_party() == args[1].as_party()),
        LeqParty => Value::Bool(args[0].as_party() <= args[1].as_party()),
        GeqParty => Value::Bool(args[0].as_party() >= args[1].as_party()),
        LessParty => Value::Bool(args[0].as_party() < args[1].as_party()),
        GreaterParty => Value::Bool(args[0].as_party() > args[1].as_party()),

        Int64ToText => Value::Text(args[0].as_i64().to_string()),
        // NOTE(MH): We handle `TextToText` special to avoid cloning.
        TextToText => panic!("Builtin::TextToText is handled in step"),
        PartyToText => Value::Text(args[0].as_party().to_string()),
        PartyToQuotedText => Value::Text(format!("'{}'", args[0].as_party())),

        Int64FromText => match args[0].as_string().parse() {
            Err(_) => Value::None,
            Ok(n) => Value::Some(Rc::new(Value::Int64(n))),
        },
        PartyFromText => match args[0].as_string().parse() {
            Err(_) => Value::None,
            Ok(p) => Value::Some(Rc::new(Value::Party(p))),
        },

        Cons => {
            let head = Rc::clone(args[0].borrow());
            let tail = Rc::clone(args[1].borrow());
            Value::Cons(head, tail)
        }
        Foldr => panic!("Builtin::Foldr is handled in step"),
        Foldl => panic!("Builtin::Foldl is handled in step"),
        EqualList => panic!("Builtin::EqualLit is handled in step"),

        Some => {
            let body = Rc::clone(args[0].borrow());
            Value::Some(body)
        }
        Error => {
            let msg = args[0].as_string();
            panic!("User error: {}", msg)
        }

        Unsupported(x) => panic!("Builtin::Unsupported {:?}", x),
    }
}
