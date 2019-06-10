use std::env;
use std::rc::Rc;
use std::time::{Duration, Instant};

mod ast;
mod builtin;
mod cek;
mod protos;
mod value;

use crate::ast::*;
use crate::cek::State;
use crate::value::Value;

struct RunResult<'a> {
    count: i64,
    duration: Duration,
    value: Rc<Value<'a>>,
}

fn make_entry_point(world: &World) -> Expr {
    Expr::Val {
        module_ref: ModuleRef {
            package_id: world.main.clone(),
            module_name: DottedName {
                segments: vec![String::from("Main")],
            },
        },
        name: String::from("main"),
    }
}

fn run<'a>(world: &'a World, entry_point: &'a Expr) -> RunResult<'a> {
    let start = Instant::now();
    let mut state = State::init(&entry_point);
    let mut count = 0;
    while !state.is_final() {
        state.step(&world);
        count += 1;
    }
    let duration = start.elapsed();
    let value = state.get_result();

    RunResult {
        count,
        duration,
        value,
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let world = World::load(filename)?;

    let entry_point = make_entry_point(&world);
    let run_result = run(&world, &entry_point);

    println!(
        "Input:  {}\nSteps:  {}\nTime:   {:?}\nResult: {:?}",
        filename, run_result.count, run_result.duration, run_result.value,
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn dar_test<F>(path: &str, handler: F)
    where
        F: FnOnce(Rc<Value>) -> (),
    {
        let world = World::load(path).unwrap();
        let entry_point = make_entry_point(&world);
        let run_result = run(&world, &entry_point);
        handler(run_result.value);
    }

    #[test]
    fn queens() {
        dar_test("test/Queens.dar", |result| match *result {
            Value::Int64(n) => assert_eq!(n, 92),
            _ => assert!(false),
        });
    }

    #[test]
    fn sort() {
        dar_test("test/Sort.dar", |result| match *result {
            Value::Int64(n) => assert_eq!(n, -487896960),
            _ => assert!(false),
        });
    }

    #[test]
    fn equal_list() {
        dar_test("test/EqualList.dar", |result| match *result {
            Value::Bool(b) => assert!(b),
            _ => assert!(false),
        });
    }
}
