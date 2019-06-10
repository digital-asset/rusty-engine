use fnv::FnvHashMap;
use std::io::*;

use crate::protos::da::daml_lf;
use crate::protos::da::daml_lf_1;

mod debruijn {
    use super::{PackageId, Var};
    use std::collections::HashMap;

    pub struct Env {
        pub self_package_id: PackageId,
        rev_indices: HashMap<super::Var, Vec<usize>>,
        depth: usize,
    }

    impl Env {
        pub fn new(self_package_id: PackageId) -> Self {
            Env {
                self_package_id,
                rev_indices: HashMap::new(),
                depth: 0,
            }
        }

        pub fn get(&self, var: &Var) -> usize {
            self.depth - self.rev_indices.get(var).and_then(|v| v.last()).unwrap()
        }

        // TODO(MH): Don't clone `var`.
        pub fn push(&mut self, var: &Var) {
            self.rev_indices
                .entry(var.clone())
                .or_insert(Vec::new())
                .push(self.depth);
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

pub type PackageId = String;

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

#[derive(Debug)]
pub struct ModuleRef {
    pub package_id: PackageId,
    pub module_name: DottedName,
}

impl ModuleRef {
    fn from_proto(proto: daml_lf_1::ModuleRef, self_package_id: PackageId) -> Self {
        use daml_lf_1::PackageRef_oneof_Sum;
        let package_ref: daml_lf_1::PackageRef = proto.package_ref.unwrap();
        let package_id = match package_ref.Sum.unwrap() {
            PackageRef_oneof_Sum::field_self(_) => self_package_id,
            PackageRef_oneof_Sum::package_id(id) => id,
        };
        let module_name = DottedName::from_proto(proto.module_name.unwrap());
        ModuleRef {
            package_id,
            module_name,
        }
    }
}

#[derive(Debug)]
pub struct TypeCon {
    module_ref: ModuleRef,
    name: DottedName,
}

impl TypeCon {
    fn from_proto(proto: daml_lf_1::Type_Con, self_package_id: PackageId) -> TypeCon {
        let tycon = proto.tycon.unwrap();
        let module_ref = ModuleRef::from_proto(tycon.module.unwrap(), self_package_id);
        let name = DottedName::from_proto(tycon.name.unwrap());
        TypeCon { module_ref, name }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Builtin {
    // Boolean comparison
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

    // Conversion from text
    Int64FromText,

    // List operations
    Cons,
    Foldr,
    Foldl,
    EqualList,

    // Misc
    Some,
    Error,

    Unsupported(daml_lf_1::BuiltinFunction),
}

impl Builtin {
    fn from_proto(proto: daml_lf_1::BuiltinFunction) -> Builtin {
        use self::Builtin::*;
        use daml_lf_1::BuiltinFunction::*;
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

            FROM_TEXT_INT64 => Int64FromText,

            FOLDR => Foldr,
            FOLDL => Foldl,
            EQUAL_LIST => EqualList,

            ERROR => Error,

            // Text stuff
            TO_TEXT_CODE_POINTS | FROM_TEXT_CODE_POINTS | SHA256_TEXT => Unsupported(proto),

            // Decimal unsupported
            ADD_DECIMAL | SUB_DECIMAL | MUL_DECIMAL | DIV_DECIMAL | ROUND_DECIMAL
            | EQUAL_DECIMAL | LEQ_DECIMAL | LESS_DECIMAL | GEQ_DECIMAL | GREATER_DECIMAL
            | TO_TEXT_DECIMAL | FROM_TEXT_DECIMAL | INT64_TO_DECIMAL | DECIMAL_TO_INT64 => {
                Unsupported(proto)
            }

            // Date unsupported
            EQUAL_DATE | LEQ_DATE | LESS_DATE | GEQ_DATE | GREATER_DATE | TO_TEXT_DATE
            | DATE_TO_UNIX_DAYS | UNIX_DAYS_TO_DATE => Unsupported(proto),

            // Timestamp unsupported
            EQUAL_TIMESTAMP
            | LEQ_TIMESTAMP
            | LESS_TIMESTAMP
            | GEQ_TIMESTAMP
            | GREATER_TIMESTAMP
            | TO_TEXT_TIMESTAMP
            | TIMESTAMP_TO_UNIX_MICROSECONDS
            | UNIX_MICROSECONDS_TO_TIMESTAMP => Unsupported(proto),

            // Party unsupported
            EQUAL_PARTY | LEQ_PARTY | LESS_PARTY | GEQ_PARTY | GREATER_PARTY
            | TO_QUOTED_TEXT_PARTY | TO_TEXT_PARTY | FROM_TEXT_PARTY => Unsupported(proto),

            // ContractId unsupported
            EQUAL_CONTRACT_ID | COERCE_CONTRACT_ID => Unsupported(proto),

            // Map unsupported
            MAP_EMPTY | MAP_INSERT | MAP_LOOKUP | MAP_DELETE | MAP_TO_LIST | MAP_SIZE => {
                Unsupported(proto)
            }

            // Misc
            TRACE => Unsupported(proto),
        }
    }
}

#[derive(Clone, Debug)]
pub enum PrimLit {
    Unit,
    Bool(bool),
    Nil,
    None,
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

#[derive(Debug)]
pub enum Pat {
    Default,
    Variant(String, Var),
    Unit,
    Bool(bool),
    Nil,
    Cons(Var, Var),
    None,
    Some(Var),
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
            },
            nil(_) => Pat::Nil,
            cons(x) => Pat::Cons(x.var_head, x.var_tail),
            none(_) => Pat::None,
            some(x) => Pat::Some(x.var_body),
            field_enum(_) => panic!("UNSUPPORTED: enum types"),
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
            Pat::None => vec![],
            Pat::Some(x) => vec![x],
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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
                let module_ref =
                    ModuleRef::from_proto(x.module.unwrap(), env.self_package_id.clone());
                let name = x.name.join(".");
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
                let tycon = TypeCon::from_proto(x.tycon.unwrap(), env.self_package_id.clone());
                let mut fields = Vec::new();
                fields.reserve(x.fields.len());
                let mut exprs = Vec::new();
                exprs.reserve(x.fields.len());
                for fx in x.fields.into_vec() {
                    fields.push(fx.field);
                    exprs.push(Self::from_proto(env, fx.expr.unwrap()));
                }
                Expr::RecCon {
                    tycon,
                    fields,
                    exprs,
                }
            }
            rec_proj(x) => {
                let tycon = TypeCon::from_proto(x.tycon.unwrap(), env.self_package_id.clone());
                let field = x.field;
                let record = Self::from_proto_ptr(env, x.record);
                Expr::RecProj {
                    tycon,
                    field,
                    record,
                }
            }
            variant_con(x) => {
                let tycon = TypeCon::from_proto(x.tycon.unwrap(), env.self_package_id.clone());
                let con = x.variant_con;
                let arg = Self::from_proto_ptr(env, x.variant_arg);
                Expr::VariantCon { tycon, con, arg }
            }
            tuple_con(_) => Expr::Unsupported("Expr::TupleCon"),
            tuple_proj(_) => Expr::Unsupported("Expr::TupleProj"),
            app(x) => {
                let fun = Self::from_proto_ptr(env, x.fun);
                let args = x
                    .args
                    .into_iter()
                    .map(|y| Self::from_proto(env, y))
                    .collect();
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
                let alts = x
                    .alts
                    .into_iter()
                    .map(|y| Alt::from_proto(env, y))
                    .collect();
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
                bindings
                    .into_iter()
                    .rev()
                    .fold(body, |body, (binder, bound)| Expr::Let {
                        binder,
                        bound,
                        body: Box::new(body),
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
            none(_) => Expr::PrimLit(PrimLit::None),
            some(x) => {
                let body = Self::from_proto(env, x.body.unwrap());
                Expr::App {
                    fun: Box::new(Expr::Builtin(Builtin::Some)),
                    args: vec![body],
                }
            }
            enum_con(_) => Expr::Unsupported("Expr::EnumCon"),
            update(_) => Expr::Unsupported("Expr::Update"),
            scenario(_) => Expr::Unsupported("Expr::Scenario"),
            rec_upd(_) => Expr::Unsupported("Expr::RecUpd"),
            tuple_upd(_) => Expr::Unsupported("Expr::TupleUpd"),
        }
    }

    fn from_proto_ptr(
        env: &mut Env,
        proto: ::protobuf::SingularPtrField<daml_lf_1::Expr>,
    ) -> Box<Expr> {
        Box::new(Expr::from_proto(env, proto.unwrap()))
    }

    fn make_case(scrut: Box<Expr>, mut alts: Vec<Alt>) -> Self {
        assert!(!alts.is_empty(), "Empty case expression");
        match alts[0].pattern {
            Pat::Default => panic!("Case starting with default pattern"),
            Pat::Bool(b0) => {
                let pos = alts
                    .iter()
                    .position(|alt| match &alt.pattern {
                        Pat::Bool(b) => b0 != *b,
                        Pat::Default => true,
                        _ => false,
                    })
                    .expect("Incomplete boolean pattern match");
                alts.swap(1, pos);
                alts.truncate(2);
                if b0 {
                    alts.swap(0, 1);
                }
            }
            Pat::Nil => {
                let pos = alts
                    .iter()
                    .position(|alt| match &alt.pattern {
                        Pat::Cons(..) | Pat::Default => true,
                        _ => false,
                    })
                    .expect("Incomplete list pattern match");
                alts.swap(1, pos);
                alts.truncate(2);
            }
            Pat::Cons(..) => {
                let pos = alts
                    .iter()
                    .position(|alt| match &alt.pattern {
                        Pat::Nil | Pat::Default => true,
                        _ => false,
                    })
                    .expect("Incomplete list pattern match");
                alts.swap(1, pos);
                alts.truncate(2);
                alts.swap(0, 1);
            }
            _ => (),
        };
        Expr::Case { scrut, alts }
    }
}

#[derive(Debug)]
pub struct DefValue {
    pub name: String,
    pub expr: Expr,
}

impl DefValue {
    fn from_proto(proto: daml_lf_1::DefValue, self_package_id: PackageId) -> Self {
        let mut env = Env::new(self_package_id);
        let name = proto.name_with_type.unwrap().name.join(".");
        let expr = Expr::from_proto(&mut env, proto.expr.unwrap());
        DefValue { name, expr }
    }
}

#[derive(Debug)]
pub struct Module {
    name: DottedName,
    values: FnvHashMap<String, DefValue>,
}

impl Module {
    fn from_proto(proto: daml_lf_1::Module, self_package_id: PackageId) -> Self {
        let name = DottedName::from_proto(proto.name.unwrap());
        let values = proto
            .values
            .into_iter()
            .map(|x| {
                let y = DefValue::from_proto(x, self_package_id.clone());
                (y.name.clone(), y)
            })
            .collect();
        Module { name, values }
    }
}

#[derive(Debug)]
pub struct Package {
    pub id: PackageId,
    modules: FnvHashMap<DottedName, Module>,
}

impl Package {
    fn from_proto(proto: daml_lf::Archive) -> Self {
        let payload: daml_lf::ArchivePayload = protobuf::parse_from_bytes(&proto.payload).unwrap();
        let id = proto.hash;
        let modules = match payload.Sum.unwrap() {
            daml_lf::ArchivePayload_oneof_Sum::daml_lf_0(_) => panic!("DAML-LF 0.x not supported"),
            daml_lf::ArchivePayload_oneof_Sum::daml_lf_1(proto) => proto
                .modules
                .into_iter()
                .map(|x| {
                    let y = Module::from_proto(x, id.clone());
                    (y.name.clone(), y)
                })
                .collect(),
        };
        Package { id, modules }
    }

    fn load<R: Read>(reader: &mut R) -> Result<Self> {
        let proto = protobuf::parse_from_reader(reader)?;
        let package = Package::from_proto(proto);
        Ok(package)
    }
}

pub struct World {
    pub main: PackageId,
    packages: FnvHashMap<PackageId, Package>,
}

impl World {
    pub fn load<P: AsRef<std::path::Path>>(path: P) -> Result<Self> {
        use std::fs::File;
        let zip_file: File = File::open(path)?;
        let mut zip = zip::ZipArchive::new(zip_file)?;
        let manifest = zip.by_name("META-INF/MANIFEST.MF")?;
        let manifest_buffered = BufReader::new(manifest);
        let mut main_name: String = String::new();
        let mut package_names: Vec<String> = Vec::new();
        for line in manifest_buffered.lines() {
            let line = line?;
            if line.starts_with("Main-Dalf:") {
                main_name = String::from(line[10..].trim());
            } else if line.starts_with("Dalfs:") {
                package_names = line[6..]
                    .split(',')
                    .map(|x| String::from(x.trim()))
                    .collect();
            }
        }
        let main_index = package_names.iter().position(|x| x == &main_name).unwrap();
        package_names.swap(0, main_index);
        let mut packages = Vec::new();
        for name in package_names {
            let mut file = zip.by_name(&name)?;
            let package = Package::load(&mut file)?;
            packages.push(package);
        }

        let main = packages[0].id.clone();
        let packages = packages
            .into_iter()
            .map(|package| (package.id.clone(), package))
            .collect();
        let world = World { main, packages };
        Ok(world)
    }

    pub fn get_value(&self, module_ref: &ModuleRef, name: &String) -> &DefValue {
        self.packages
            .get(&module_ref.package_id)
            .unwrap()
            .modules
            .get(&module_ref.module_name)
            .unwrap()
            .values
            .get(name)
            .unwrap()
    }
}
