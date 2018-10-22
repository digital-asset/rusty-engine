use daml_lf_1;
use std::collections::HashMap;

type Var = String;

#[derive(Clone, Eq, Hash, PartialEq)]
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

struct ModuleRef {
  module_name: DottedName,
}

impl ModuleRef {
  fn from_proto(proto: daml_lf_1::ModuleRef) -> Self {
    let module_name = DottedName::from_proto(proto.module_name.unwrap());
    ModuleRef { module_name }
  }
}

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
}

struct Alt {
  pattern: Pat,
  body: Box<Expr>,
}

impl Alt {
  fn from_proto(proto: daml_lf_1::CaseAlt) -> Self {
    let pattern = Pat::from_proto(proto.Sum.unwrap());
    let body = Box::new(Expr::from_proto(proto.body.unwrap()));
    Alt { pattern, body }
  }
}

enum Expr {
  Var {
    name: Var,
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
  fn from_proto(proto: daml_lf_1::Expr) -> Expr {
    use daml_lf_1::Expr_oneof_Sum::*;
    match proto.Sum.unwrap() {
      var(x) => Expr::Var { name: x },
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
          .map(|fx| (fx.field, Expr::from_proto(fx.expr.unwrap())))
          .collect();
        Expr::RecCon { tycon, fields }
      }
      rec_proj(x) => {
        let tycon = TypeCon::from_proto(x.tycon.unwrap());
        let field = x.field;
        let record = Box::new(Expr::from_proto(x.record.unwrap()));
        Expr::RecProj {
          tycon,
          field,
          record,
        }
      }
      variant_con(x) => {
        let tycon = TypeCon::from_proto(x.tycon.unwrap());
        let con = x.variant_con;
        let arg = Box::new(Expr::from_proto(x.variant_arg.unwrap()));
        Expr::VariantCon { tycon, con, arg }
      }
      tuple_con(_) => Expr::Unsupported("Expr::TupleCon"),
      tuple_proj(_) => Expr::Unsupported("Expr::TupleProj"),
      app(x) => {
        let fun = Box::new(Expr::from_proto(x.fun.unwrap()));
        let args = x.args.into_iter().map(Expr::from_proto).collect();
        Expr::App { fun, args }
      }
      ty_app(x) => Self::from_proto(x.expr.unwrap()),
      abs(x) => {
        let params = x.param.into_iter().map(|x| x.var).collect();
        let body = Box::new(Expr::from_proto(x.body.unwrap()));
        Expr::Lam { params, body }
      }
      ty_abs(x) => Self::from_proto(x.body.unwrap()),
      case(x) => {
        let scrut = Self::from_proto_ptr(x.scrut);
        let alts = x.alts.into_iter().map(Alt::from_proto).collect();
        Expr::Case { scrut, alts }
      }
      field_let(x) => {
        let body = Self::from_proto(x.body.unwrap());
        x.bindings.into_iter().rev().fold(body, |body, binding| {
          let binder = binding.binder.unwrap().var;
          let bound = Self::from_proto_ptr(binding.bound);
          Expr::Let {
            binder,
            bound,
            body: Box::new(body),
          }
        })
      }
      nil(_) => Expr::Nil,
      cons(x) => {
        let tail = Self::from_proto(x.tail.unwrap());
        x.front.into_iter().rev().fold(tail, |tail, elem| {
          let head = Box::new(Self::from_proto(elem));
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

  fn from_proto_ptr(proto: ::protobuf::SingularPtrField<daml_lf_1::Expr>) -> Box<Expr> {
    Box::new(Expr::from_proto(proto.unwrap()))
  }
}

struct DefValue {
  name: String,
  expr: Expr,
}

impl DefValue {
  fn from_proto(proto: daml_lf_1::DefValue) -> Self {
    let name = proto.var.unwrap().var;
    let expr = Expr::from_proto(proto.expr.unwrap());
    DefValue { name, expr }
  }
}

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

struct Package {
  modules: HashMap<DottedName, Module>,
}

impl Package {
  fn from_proto(proto: daml_lf_1::Package) -> Self {
    let modules = proto
      .modules
      .into_iter()
      .map(|x| {
        let y = Module::from_proto(x);
        (y.name.clone(), y)
      }).collect();
    Package { modules }
  }
}
