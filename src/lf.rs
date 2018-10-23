use std;
use std::collections::HashMap;
use protobuf;

use daml_lf;
use daml_lf_1;

pub mod debruijn {
  use std::collections::HashMap;
  use super::Var;

  pub struct Env {
    rev_indices: HashMap<super::Var, Vec<usize>>,
    depth: usize,
  }

  impl Env {
    pub fn new() -> Self {
      Env { rev_indices: HashMap::new(), depth: 0 }
    }

    pub fn get(&self, var: &Var) -> usize {
      self.depth - self.rev_indices.get(var).and_then(|v| v.last()).unwrap()
    }

    // TODO(MH): Don't clone `var`.
    pub fn push(&mut self, var: &Var) {
      self.rev_indices.entry(var.clone()).or_insert(Vec::new()).push(self.depth);
      self.depth += 1;
    }

    // TODO(MH): Use iterators.
    pub fn push_many(&mut self, vars: &Vec<&Var>) {
      for var in vars {
        self.push(var);
      }
    }

    pub fn pop(&mut self, var: &Var) {
      self.rev_indices.get_mut(var).and_then(|v| v.pop()).unwrap();
      self.depth -= 1;
    }

    pub fn pop_many(&mut self, vars: &Vec<&Var>) {
      for var in vars {
        self.pop(var);
      }
    }
  }
}

use self::debruijn::Env;

type Var = String;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct DottedName {
  segments: Vec<String>,
}

impl DottedName {
  fn from_proto(proto: daml_lf_1::DottedName) -> DottedName {
    DottedName {
      segments: proto.segments.into_vec(),
    }
  }
}

#[derive (Debug)]
struct ModuleRef {
  module_name: DottedName,
}

impl ModuleRef {
  fn from_proto(proto: daml_lf_1::ModuleRef) -> Self {
    let module_name = DottedName::from_proto(proto.module_name.unwrap());
    ModuleRef { module_name }
  }
}

#[derive (Debug)]
struct TypeCon {
  module_ref: ModuleRef,
  name: DottedName,
}

impl TypeCon {
  fn from_proto(proto: daml_lf_1::Type_Con) -> TypeCon {
    let tycon = proto.tycon.unwrap();
    let module_ref = ModuleRef::from_proto(tycon.module.unwrap());
    let name = DottedName::from_proto(tycon.name.unwrap());
    TypeCon { module_ref, name }
  }
}

type Builtin = daml_lf_1::BuiltinFunction;

#[derive (Debug)]
enum PrimCon {
  Unit,
  False,
  True,
}

impl PrimCon {
  fn from_proto(proto: daml_lf_1::PrimCon) -> PrimCon {
    use daml_lf_1::PrimCon::*;
    use lf::PrimCon::*;
    match proto {
      CON_UNIT => Unit,
      CON_FALSE => False,
      CON_TRUE => True,
    }
  }
}

#[derive (Debug)]
enum PrimLit {
  Int64(i64),
  Text(String),
  Party(String),
  Unsupported(&'static str),
}

impl PrimLit {
  fn from_proto(proto: daml_lf_1::PrimLit) -> PrimLit {
    use daml_lf_1::PrimLit_oneof_Sum::*;
    match proto.Sum.unwrap() {
      int64(x) => PrimLit::Int64(x),
      decimal(_) => PrimLit::Unsupported("PrimLit::Decimal"),
      text(x) => PrimLit::Text(x),
      timestamp(_) => PrimLit::Unsupported("PrimLit::Timestamp"),
      party(x) => PrimLit::Party(x),
      date(_) => PrimLit::Unsupported("PrimLit::Date"),
    }
  }
}

#[derive (Debug)]
enum Pat {
  Default,
  Variant(String, Var),
  PrimCon(PrimCon),
  Nil,
  Cons(Var, Var),
}

impl Pat {
  fn from_proto(proto: daml_lf_1::CaseAlt_oneof_Sum) -> Self {
    use daml_lf_1::CaseAlt_oneof_Sum::*;
    match proto {
      default(_) => Pat::Default,
      variant(x) => Pat::Variant(x.variant, x.binder),
      prim_con(x) => Pat::PrimCon(PrimCon::from_proto(x)),
      nil(_) => Pat::Nil,
      cons(x) => Pat::Cons(x.var_head, x.var_tail),
    }
  }

  fn binders(&self) -> Vec<&Var> {
    match self {
      Pat::Default => vec![],
      Pat::Variant(_, x) => vec![x],
      Pat::PrimCon(_) => vec![],
      Pat::Nil => vec![],
      Pat::Cons(x, y) => vec![x, y],
    }
  }
}

#[derive (Debug)]
struct Alt {
  pattern: Pat,
  body: Box<Expr>,
}

impl Alt {
  fn from_proto(env: &mut Env, proto: daml_lf_1::CaseAlt) -> Self {
    let pattern = Pat::from_proto(proto.Sum.unwrap());
    let body = {
      let binders = pattern.binders();
      env.push_many(&binders);
      let body = Expr::from_proto_ptr(env, proto.body);
      env.pop_many(&binders);
      body
    };
    Alt { pattern, body }
  }
}

#[derive (Debug)]
enum Expr {
  Var {
    name: Var,
    index: usize,
  },
  Val {
    module_ref: ModuleRef,
    name: String,
  },
  Builtin(Builtin),
  PrimCon(PrimCon),
  PrimLit(PrimLit),
  RecCon {
    tycon: TypeCon,
    fields: Vec<(String, Expr)>,
  },
  RecProj {
    tycon: TypeCon,
    field: String,
    record: Box<Expr>,
  },
  VariantCon {
    tycon: TypeCon,
    con: String,
    arg: Box<Expr>,
  },
  App {
    fun: Box<Expr>,
    args: Vec<Expr>,
  },
  Lam {
    params: Vec<Var>,
    body: Box<Expr>,
  },
  Case {
    scrut: Box<Expr>,
    alts: Vec<Alt>,
  },
  Let {
    binder: Var,
    bound: Box<Expr>,
    body: Box<Expr>,
  },
  Nil,
  Cons {
    head: Box<Expr>,
    tail: Box<Expr>,
  },

  Unsupported(&'static str),
}

impl Expr {
  fn from_proto(env: &mut Env, proto: daml_lf_1::Expr) -> Expr {
    use daml_lf_1::Expr_oneof_Sum::*;
    match proto.Sum.unwrap() {
      var(x) => {
        let index = env.get(&x);
        let name = x;
        Expr::Var { name, index }
      }
      val(x) => {
        let module_ref = ModuleRef::from_proto(x.module.unwrap());
        let name = x.name;
        Expr::Val { module_ref, name }
      }
      builtin(x) => Expr::Builtin(x),
      prim_con(x) => Expr::PrimCon(PrimCon::from_proto(x)),
      prim_lit(x) => Expr::PrimLit(PrimLit::from_proto(x)),
      rec_con(x) => {
        let tycon = TypeCon::from_proto(x.tycon.unwrap());
        let fields = x
          .fields
          .into_iter()
          .map(|fx| (fx.field, Self::from_proto(env, fx.expr.unwrap())))
          .collect();
        Expr::RecCon { tycon, fields }
      }
      rec_proj(x) => {
        let tycon = TypeCon::from_proto(x.tycon.unwrap());
        let field = x.field;
        let record = Self::from_proto_ptr(env, x.record);
        Expr::RecProj {
          tycon,
          field,
          record,
        }
      }
      variant_con(x) => {
        let tycon = TypeCon::from_proto(x.tycon.unwrap());
        let con = x.variant_con;
        let arg = Self::from_proto_ptr(env, x.variant_arg);
        Expr::VariantCon { tycon, con, arg }
      }
      tuple_con(_) => Expr::Unsupported("Expr::TupleCon"),
      tuple_proj(_) => Expr::Unsupported("Expr::TupleProj"),
      app(x) => {
        let fun = Self::from_proto_ptr(env, x.fun);
        let args = x.args.into_iter().map(|y| Self::from_proto(env, y)).collect();
        Expr::App { fun, args }
      }
      ty_app(x) => Self::from_proto(env, x.expr.unwrap()),
      abs(x) => {
        let params: Vec<Var> = x.param.into_iter().map(|x| x.var).collect();
        let body = {
          // TODO(MH): Remove this abomination.
          let binders = params.iter().collect();
          env.push_many(&binders);
          let body = Self::from_proto_ptr(env, x.body);
          env.pop_many(&binders);
          body
        };
        Expr::Lam { params, body }
      }
      ty_abs(x) => Self::from_proto(env, x.body.unwrap()),
      case(x) => {
        let scrut = Self::from_proto_ptr(env, x.scrut);
        let alts = x.alts.into_iter().map(|y| Alt::from_proto(env, y)).collect();
        Expr::Case { scrut, alts }
      }
      field_let(x) => {
        let mut bindings = Vec::new();
        bindings.reserve(x.bindings.len());
        for binding in x.bindings.into_vec() {
          let binder = binding.binder.unwrap().var;
          let bound = Self::from_proto_ptr(env, binding.bound);
          env.push(&binder);
          bindings.push((binder, bound));
        }
        let body = Self::from_proto(env, x.body.unwrap());
        for (binder, _) in bindings.iter() {
          env.pop(binder);
        }
        bindings.into_iter().rev().fold(body, |body, (binder, bound)| {
          Expr::Let {
            binder,
            bound,
            body: Box::new(body),
          }
        })
      }
      nil(_) => Expr::Nil,
      cons(x) => {
        let tail = Self::from_proto(env, x.tail.unwrap());
        x.front.into_iter().rev().fold(tail, |tail, elem| {
          let head = Box::new(Self::from_proto(env, elem));
          Expr::Cons {
            head,
            tail: Box::new(tail),
          }
        })
      }
      update(_) => Expr::Unsupported("Expr::Update"),
      scenario(_) => Expr::Unsupported("Expr::Scenario"),
      rec_upd(_) => Expr::Unsupported("Expr::RecUpd"),
      tuple_upd(_) => Expr::Unsupported("Expr::TupleUpd"),
    }
  }

  fn from_proto_ptr(env: &mut Env, proto: ::protobuf::SingularPtrField<daml_lf_1::Expr>) -> Box<Expr> {
    Box::new(Expr::from_proto(env, proto.unwrap()))
  }
}

#[derive (Debug)]
struct DefValue {
  name: String,
  expr: Expr,
}

impl DefValue {
  fn from_proto(proto: daml_lf_1::DefValue) -> Self {
    let mut env = Env::new();
    let name = proto.var.unwrap().var;
    let expr = Expr::from_proto(&mut env, proto.expr.unwrap());
    DefValue { name, expr }
  }
}

#[derive (Debug)]
struct Module {
  name: DottedName,
  values: HashMap<String, DefValue>,
}

impl Module {
  fn from_proto(proto: daml_lf_1::Module) -> Self {
    let name = DottedName::from_proto(proto.name.unwrap());
    let values = proto
      .values
      .into_iter()
      .map(|x| {
        let y = DefValue::from_proto(x);
        (y.name.clone(), y)
      }).collect();
    Module { name, values }
  }
}

#[derive (Debug)]
pub struct Package {
  modules: HashMap<DottedName, Module>,
}

impl Package {
  fn from_proto(proto: daml_lf::Archive) -> Self {
    let payload = proto
      .payload
      .unwrap()
      .Sum
      .unwrap();
    let modules = match payload {
      daml_lf::ArchivePayload_oneof_Sum::daml_lf_1(proto) =>
        proto
          .modules
          .into_iter()
          .map(|x| {
            let y = Module::from_proto(x);
            (y.name.clone(), y)
          }).collect()
    };
    Package { modules }
  }

  pub fn load<P: AsRef<std::path::Path>>(path: P) -> std::io::Result<Package> {
    use std::fs::File;
    let mut file: File = File::open(path)?;
    let proto = protobuf::parse_from_reader(&mut file)?;
    let package = Package::from_proto(proto);
    Ok(package)
  }
}
