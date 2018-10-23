#![allow(dead_code)]
#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate serde_json;
extern crate protobuf;

use std::borrow::Borrow;
use std::collections::HashMap;
use std::env;
use std::fmt;
use std::io::Read;
use std::rc::Rc;

mod ast;
use ast::{Altn, Defn, Expr, Lambda, Module, Name};

mod daml_lf;
mod daml_lf_1;
mod lf;

#[derive(Clone)]
struct External {
  arity: usize,
  run: fn(Vec<Rc<Value>>) -> Rc<Value>,
}

impl fmt::Debug for External {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "External {{ arity: {}, run: .. }}", self.arity)
  }
}

type Externals = HashMap<Name, External>;

#[derive(Debug, Copy, Clone)]
enum Prim<'a> {
  Global(&'a Name, &'a Lambda),
  External(&'a Name, &'a External),
  Pack(usize, usize),
}

#[derive(Debug, Clone)]
enum Value<'a> {
  Num(i64),
  Pack(usize, Vec<Rc<Value<'a>>>),
  PAP(Prim<'a>, Vec<Rc<Value<'a>>>, usize),
}

#[derive(Debug)]
enum Ctrl<'a> {
  Evaluating,
  Expr(&'a Expr),
  Value(Rc<Value<'a>>),
}

#[derive(Debug)]
struct Env<'a> {
  stack: Vec<Rc<Value<'a>>>,
}

impl<'a> Env<'a> {
  fn new() -> Self {
    Env { stack: Vec::new() }
  }

  fn get(&self, idx: usize) -> &Rc<Value<'a>> {
    self
      .stack
      .get(self.stack.len() - idx)
      .expect("Bad de Bruijn index")
  }

  fn push(&mut self, value: Rc<Value<'a>>) {
    self.stack.push(value);
  }

  fn push_many(&mut self, args: &Vec<Rc<Value<'a>>>) {
    self.stack.extend_from_slice(args);
  }

  fn pop(&mut self, count: usize) {
    let new_len = self.stack.len() - count;
    self.stack.truncate(new_len);
  }
}

#[derive(Debug)]
enum Kont<'a> {
  Dump(Env<'a>),
  Pop(usize),
  Args(&'a [Expr]),
  Fun(Prim<'a>, Vec<Rc<Value<'a>>>, usize),
  Match(&'a Vec<Altn>),
  Let(&'a Name, &'a Expr),
}

#[derive(Debug)]
struct State<'a> {
  ctrl: Ctrl<'a>,
  env: Env<'a>,
  kont: Vec<Kont<'a>>,
}

impl<'a> Value<'a> {
  fn mk_unit() -> Rc<Self> {
    Rc::new(Value::Pack(0, Vec::new()))
  }

  fn mk_bool(b: bool) -> Rc<Self> {
    Rc::new(Value::Pack(b.into(), Vec::new()))
  }

  fn mk_num(n: i64) -> Rc<Self> {
    Rc::new(Value::Num(n))
  }
}

impl<'a> Ctrl<'a> {
  fn from_prim(prim: Prim<'a>, arity: usize) -> Self {
    Ctrl::Value(Rc::new(Value::PAP(prim, Vec::new(), arity)))
  }
}

impl<'a> State<'a> {
  fn from_expr(expr: &'a Expr) -> Self {
    State {
      ctrl: Ctrl::Expr(expr),
      env: Env::new(),
      kont: Vec::new(),
    }
  }

  fn step(&mut self, module: &'a Module, externals: &'a Externals) {
    let old_ctrl = std::mem::replace(&mut self.ctrl, Ctrl::Evaluating);

    let new_ctrl = match old_ctrl {
      Ctrl::Evaluating => panic!("Control was not update after last step"),

      Ctrl::Expr(Expr::Local { idx, .. }) => {
        let v = self.env.get(*idx);
        Ctrl::Value(Rc::clone(&v))
      }
      Ctrl::Expr(Expr::Global { name }) => {
        let lam = module
          .get(name)
          .expect(&format!("Unknown global: {}", name));
        Ctrl::from_prim(Prim::Global(name, lam), lam.binds.len())
      }
      Ctrl::Expr(Expr::External { name }) => {
        let ext = externals
          .get(name)
          .expect(&format!("Unknown external: {}", name));
        Ctrl::from_prim(Prim::External(name, ext), ext.arity)
      }
      Ctrl::Expr(&Expr::Pack { tag, arity }) => Ctrl::from_prim(Prim::Pack(tag, arity), arity),
      Ctrl::Expr(&Expr::Num { int }) => Ctrl::Value(Value::mk_num(int)),
      Ctrl::Expr(Expr::Ap { fun, args }) => {
        self.kont.push(Kont::Args(args));
        Ctrl::Expr(fun)
      }
      Ctrl::Expr(Expr::Let { defn, body }) => {
        let Defn { lhs, rhs } = defn.borrow();
        self.kont.push(Kont::Let(lhs, body));
        Ctrl::Expr(rhs)
      }
      Ctrl::Expr(Expr::Match { expr, altns }) => {
        self.kont.push(Kont::Match(altns));
        Ctrl::Expr(expr)
      }

      Ctrl::Value(v) => match v.borrow() {
        Value::PAP(prim, args, 0) => match prim {
          Prim::Global(_name, lam) => {
            let Lambda { body, .. } = lam;
            let mut new_env = Env::new();
            new_env.push_many(args);
            let old_env = std::mem::replace(&mut self.env, new_env);
            self.kont.push(Kont::Dump(old_env));
            Ctrl::Expr(body)
          }
          Prim::External(_name, ext) => Ctrl::Value((ext.run)(args.clone())),
          Prim::Pack(tag, _arity) => Ctrl::Value(Rc::new(Value::Pack(*tag, args.clone()))),
        },

        _ => match self.kont.pop().expect("Step on final state") {
          Kont::Dump(env) => {
            self.env = env;
            Ctrl::Value(Rc::clone(&v))
          }
          Kont::Pop(count) => {
            self.env.pop(count);
            Ctrl::Value(Rc::clone(&v))
          }
          Kont::Args(next_args) => match v.borrow() {
            Value::PAP(prim, args, missing) => {
              let (next_arg, next_args) = next_args.split_first().expect("Empty Args");
              if !next_args.is_empty() {
                self.kont.push(Kont::Args(next_args));
              }
              self.kont.push(Kont::Fun(*prim, args.clone(), *missing));
              Ctrl::Expr(next_arg)
            }
            _ => panic!("Applying value"),
          },
          Kont::Fun(prim2, mut args2, missing2) => {
            args2.push(Rc::clone(&v));
            Ctrl::Value(Rc::new(Value::PAP(prim2, args2, missing2 - 1)))
          }
          Kont::Match(altns) => match v.borrow() {
            Value::Pack(tag, args) => {
              let Altn { rhs, .. } = &altns[*tag];
              self.kont.push(Kont::Pop(args.len()));
              self.env.push_many(&args);
              Ctrl::Expr(rhs)
            }
            _ => panic!("Pattern match on non-data value"),
          },
          Kont::Let(_name, body) => {
            self.kont.push(Kont::Pop(1));
            self.env.push(Rc::clone(&v));
            Ctrl::Expr(body)
          }
        },
      },
    };

    self.ctrl = new_ctrl
  }

  fn is_final(&self) -> bool {
    match self.ctrl.borrow() {
      Ctrl::Value(v) => match v.borrow() {
        Value::Num(_) | Value::Pack(_, _) => self.kont.is_empty(),
        _ => false,
      },
      _ => false,
    }
  }
}

fn args_i64(args: Vec<Rc<Value>>) -> i64 {
  let msg: &str = "Type mismatch in args_i64";
  if args.len() != 1 {
    panic!(msg);
  } else {
    match *args[0] {
      Value::Num(x) => x,
      _ => panic!(msg),
    }
  }
}

fn args_i64_i64(args: Vec<Rc<Value>>) -> (i64, i64) {
  let msg: &str = "Type mismatch in args_i64_i64";
  if args.len() != 2 {
    panic!(msg);
  } else {
    match (args[0].borrow(), args[1].borrow()) {
      (&Value::Num(x), &Value::Num(y)) => (x, y),
      _ => panic!(msg),
    }
  }
}

fn externals() -> Externals {
  let add_ext = External {
    arity: 2,
    run: |args| {
      let (x, y) = args_i64_i64(args);
      Value::mk_num(x + y)
    },
  };
  let sub_ext = External {
    arity: 2,
    run: |args| {
      let (x, y) = args_i64_i64(args);
      Value::mk_num(x - y)
    },
  };
  let mul_ext = External {
    arity: 2,
    run: |args| {
      let (x, y) = args_i64_i64(args);
      Value::mk_num(x * y)
    },
  };
  let neg_ext = External {
    arity: 1,
    run: |args| Value::mk_num(-args_i64(args)),
  };
  let eq_ext = External {
    arity: 2,
    run: |args| {
      let (x, y) = args_i64_i64(args);
      Value::mk_bool(x == y)
    },
  };
  let le_ext = External {
    arity: 2,
    run: |args| {
      let (x, y) = args_i64_i64(args);
      Value::mk_bool(x <= y)
    },
  };
  let lt_ext = External {
    arity: 2,
    run: |args| {
      let (x, y) = args_i64_i64(args);
      Value::mk_bool(x < y)
    },
  };
  let gt_ext = External {
    arity: 2,
    run: |args| {
      let (x, y) = args_i64_i64(args);
      Value::mk_bool(x > y)
    },
  };
  let ge_ext = External {
    arity: 2,
    run: |args| {
      let (x, y) = args_i64_i64(args);
      Value::mk_bool(x >= y)
    },
  };
  let chr_ext = External {
    arity: 1,
    run: |args| Value::mk_num(args_i64(args) & 0xFF),
  };
  let ord_ext = External {
    arity: 1,
    run: |args| Value::mk_num(args_i64(args)),
  };
  let puti_ext = External {
    arity: 1,
    run: |args| {
      println!("{}", args_i64(args));
      Value::mk_unit()
    },
  };
  let putc_ext = External {
    arity: 1,
    run: |args| {
      print!("{}", args_i64(args) as u8 as char);
      Value::mk_unit()
    },
  };
  let geti_ext = External {
    arity: 1,
    run: |args| {
      if args.len() != 1 {
        panic!("Arity mismatch for geti");
      } else {
        let mut input = String::new();
        std::io::stdin()
          .read_line(&mut input)
          .expect("Failed to read line");
        Value::mk_num(input.trim().parse().expect("Input not a number!"))
      }
    },
  };
  let getc_ext = External {
    arity: 1,
    run: |args| {
      if args.len() != 1 {
        panic!("Arity mismatch for getc");
      } else {
        let mut input = [0];
        let n = match std::io::stdin().read_exact(&mut input) {
          Ok(()) => input[0] as i64,
          Err(_) => -1,
        };
        Value::mk_num(n)
      }
    },
  };
  let seq_ext = External {
    arity: 2,
    run: |mut args| {
      if args.len() != 2 {
        panic!("Type mismatch for seq");
      } else {
        args.pop().unwrap()
      }
    },
  };
  [
    (String::from("add"), add_ext),
    (String::from("sub"), sub_ext),
    (String::from("mul"), mul_ext),
    (String::from("neg"), neg_ext),
    (String::from("eq"), eq_ext),
    (String::from("le"), le_ext),
    (String::from("lt"), lt_ext),
    (String::from("gt"), gt_ext),
    (String::from("ge"), ge_ext),
    (String::from("chr"), chr_ext),
    (String::from("ord"), ord_ext),
    (String::from("puti"), puti_ext),
    (String::from("putc"), putc_ext),
    (String::from("geti"), geti_ext),
    (String::from("getc"), getc_ext),
    (String::from("seq"), seq_ext),
  ]
    .iter()
    .cloned()
    .collect()
}

fn _main() -> std::io::Result<()> {
  let debug = false;
  let args: Vec<String> = env::args().collect();
  let filename = &args[1];

  let entry_point = Expr::entry_point();
  let module: Module = ast::load_module(filename)?;
  let externals = externals();
  eprintln!("Loaded!");

  // let mut state = State::enter_caf(String::from("test"));
  let mut state = State::from_expr(&entry_point);
  let mut count = 0;
  if debug {
    eprintln!("State 0: {:?}", state);
  }
  while !state.is_final() {
    state.step(&module, &externals);
    count += 1;
    if debug {
      eprintln!("State {}: {:?}", count, state);
    }
  }
  eprintln!("==========\nSteps: {}", count);

  if debug {
    eprintln!("==========\nResult: {:?}", state.ctrl);
  }

  Ok(())
}

fn main() -> std::io::Result<()> {
  let args: Vec<String> = env::args().collect();
  let filename = &args[1];
  let package = lf::Package::load(filename)?;
  println!("{:?}", package);
  Ok(())
}
