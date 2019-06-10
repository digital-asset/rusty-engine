use fnv::FnvHashMap;

use crate::protos::daml_lf as daml_lf;
use crate::protos::daml_lf_1 as daml_lf_1;

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

pub type Var = String;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct DottedName {
  pub segments: Vec<String>,
}

impl DottedName {
  fn from_proto(proto: daml_lf_1::DottedName) -> DottedName {
    DottedName {
      segments: proto.segments.into_vec(),
    }
  }
}

#[derive (Debug)]
pub struct ModuleRef {
  pub module_name: DottedName,
}

impl ModuleRef {
  fn from_proto(proto: daml_lf_1::ModuleRef) -> Self {
    let module_name = DottedName::from_proto(proto.module_name.unwrap());
    ModuleRef { module_name }
  }
}

#[derive (Debug)]
pub struct TypeCon {
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

#[derive (Debug, Clone, Copy)]
pub enum Builtin {
  // Boolean comparis
  EqualBool,

  // Integer arithmetic
  AddInt64,
  SubInt64,
  MulInt64,
  DivInt64,
  ModInt64,
  ExpInt64,

  // Integer comparison
  EqualInt64,
  LeqInt64,
  GeqInt64,
  LessInt64,
  GreaterInt64,

  // Text operations
  AppendText,
  ImplodeText,
  ExplodeText,

  // Text comparison
  EqualText,
  LeqText,
  GeqText,
  LessText,
  GreaterText,

  // Conversion to text
  Int64ToText,
  TextToText,

  // List operations
  Cons,
  Foldr,
  Foldl,
  EqualList,

  // Misc
  Error,

  Unsupported(daml_lf_1::BuiltinFunction),
}

impl Builtin {
  fn from_proto(proto: daml_lf_1::BuiltinFunction) -> Builtin {
    use daml_lf_1::BuiltinFunction::*;
    use self::Builtin::*;
    match proto {
      EQUAL_BOOL => EqualBool,

      ADD_INT64 => AddInt64,
      SUB_INT64 => SubInt64,
      MUL_INT64 => MulInt64,
      DIV_INT64 => DivInt64,
      MOD_INT64 => ModInt64,
      EXP_INT64 => ExpInt64,

      EQUAL_INT64 => EqualInt64,
      LEQ_INT64 => LeqInt64,
      GEQ_INT64 => GeqInt64,
      LESS_INT64 => LessInt64,
      GREATER_INT64 => GreaterInt64,

      APPEND_TEXT => AppendText,
      IMPLODE_TEXT => ImplodeText,
      EXPLODE_TEXT => ExplodeText,

      EQUAL_TEXT => EqualText,
      LEQ_TEXT => LeqText,
      GEQ_TEXT => GeqText,
      LESS_TEXT => LessText,
      GREATER_TEXT => GreaterText,

      TO_TEXT_INT64 => Int64ToText,
      TO_TEXT_TEXT => TextToText,

      FOLDR => Foldr,
      FOLDL => Foldl,
      EQUAL_LIST => EqualList,

      ERROR => Error,

      _ => Unsupported(proto),
    }
  }
}

#[derive (Clone, Debug)]
pub enum PrimLit {
  Unit,
  Bool(bool),
  Nil,
  Int64(i64),
  Text(String),
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
      party(_) => PrimLit::Unsupported("PrimLit::Party"),
      date(_) => PrimLit::Unsupported("PrimLit::Date"),
    }
  }
}

#[derive (Debug)]
pub enum Pat {
  Default,
  Variant(String, Var),
  Unit,
  Bool(bool),
  Nil,
  Cons(Var, Var),
}

impl Pat {
  fn from_proto(proto: daml_lf_1::CaseAlt_oneof_Sum) -> Self {
    use daml_lf_1::CaseAlt_oneof_Sum::*;
    use daml_lf_1::PrimCon::*;
    match proto {
      default(_) => Pat::Default,
      variant(x) => Pat::Variant(x.variant, x.binder),
      prim_con(x) => match x {
        CON_UNIT => Pat::Unit,
        CON_FALSE => Pat::Bool(false),
        CON_TRUE => Pat::Bool(true),
      }
      nil(_) => Pat::Nil,
      cons(x) => Pat::Cons(x.var_head, x.var_tail),
    }
  }

  fn binders(&self) -> Vec<&Var> {
    match self {
      Pat::Default => vec![],
      Pat::Variant(_, x) => vec![x],
      Pat::Unit => vec![],
      Pat::Bool(_) => vec![],
      Pat::Nil => vec![],
      Pat::Cons(x, y) => vec![x, y],
    }
  }
}

#[derive (Debug)]
pub struct Alt {
  pub pattern: Pat,
  pub body: Expr,
}

impl Alt {
  fn from_proto(env: &mut Env, proto: daml_lf_1::CaseAlt) -> Self {
    let pattern = Pat::from_proto(proto.Sum.unwrap());
    let body = {
      let binders = pattern.binders();
      env.push_many(&binders);
      let body = Expr::from_proto(env, proto.body.unwrap());
      env.pop_many(&binders);
      body
    };
    Alt { pattern, body }
  }
}

#[derive (Debug)]
pub enum Expr {
  Var {
    name: Var,
    index: usize,
  },
  Val {
    module_ref: ModuleRef,
    name: String,
  },
  Builtin(Builtin),
  PrimLit(PrimLit),
  RecCon {
    tycon: TypeCon,
    fields: Vec<String>,
    exprs: Vec<Expr>,
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

  Unsupported(&'static str),
}

impl Expr {
  fn from_proto(env: &mut Env, proto: daml_lf_1::Expr) -> Expr {
    use daml_lf_1::Expr_oneof_Sum::*;
    use daml_lf_1::PrimCon::*;
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
      builtin(x) => Expr::Builtin(Builtin::from_proto(x)),
      prim_con(x) => Expr::PrimLit(match x {
        CON_UNIT => PrimLit::Unit,
        CON_FALSE => PrimLit::Bool(false),
        CON_TRUE => PrimLit::Bool(true),
      }),
      prim_lit(x) => Expr::PrimLit(PrimLit::from_proto(x)),
      rec_con(x) => {
        let tycon = TypeCon::from_proto(x.tycon.unwrap());
        let mut fields = Vec::new();
        fields.reserve(x.fields.len());
        let mut exprs = Vec::new();
        exprs.reserve(x.fields.len());
        for fx in x.fields.into_vec() {
          fields.push(fx.field);
          exprs.push(Self::from_proto(env, fx.expr.unwrap()));
        }
        Expr::RecCon { tycon, fields, exprs }
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
        Expr::make_case(scrut, alts)
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
      nil(_) => Expr::PrimLit(PrimLit::Nil),
      cons(x) => {
        let tail = Self::from_proto(env, x.tail.unwrap());
        x.front.into_iter().rev().fold(tail, |tail, elem| {
          let head = Self::from_proto(env, elem);
          Expr::App {
            fun: Box::new(Expr::Builtin(Builtin::Cons)),
            args: vec![head, tail],
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

  fn make_case(scrut: Box<Expr>, mut alts: Vec<Alt>) -> Self {
    assert!(!alts.is_empty(), "Empty case expression");
    match alts[0].pattern {
      Pat::Default => panic!("Case starting with default pattern"),
      Pat::Bool(b0) => {
        let pos = alts.iter().position(|alt| match &alt.pattern {
          Pat::Bool(b) => b0 != *b,
          Pat::Default => true,
          _ => false,
        }).expect("Incomplete boolean pattern match");
        alts.swap(1, pos);
        alts.truncate(2);
        if b0 {
          alts.swap(0, 1);
        }
      }
      Pat::Nil => {
        let pos = alts.iter().position(|alt| match &alt.pattern {
          Pat::Cons(..) | Pat::Default => true,
          _ => false,
        }).expect("Incomplete list pattern match");
        alts.swap(1, pos);
        alts.truncate(2);
      }
      Pat::Cons(..) => {
        let pos = alts.iter().position(|alt| match &alt.pattern {
          Pat::Nil | Pat::Default => true,
          _ => false,
        }).expect("Incomplete list pattern match");
        alts.swap(1, pos);
        alts.truncate(2);
        alts.swap(0, 1);
      }
      _ => (),
    };
    Expr::Case { scrut, alts }
  }
}

#[derive (Debug)]
pub struct DefValue {
  pub name: String,
  pub expr: Expr,
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
pub struct Module {
  name: DottedName,
  values: FnvHashMap<String, DefValue>,
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
  modules: FnvHashMap<DottedName, Module>,
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

  pub fn get_value(&self, module_ref: &ModuleRef, name: &String) -> &DefValue {
    self.modules.get(&module_ref.module_name).unwrap().values.get(name).unwrap()
  }
}
