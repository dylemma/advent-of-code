use std::env;

use crate::helper::{GenResult, puzzle_input_path};

mod helper;
mod puzzle01;
mod puzzle02;
mod puzzle03;
mod puzzle04;
mod puzzle05;
mod puzzle06;
mod puzzle07;

fn main() -> GenResult<()> {

    let mut args = env::args();

    // ignore this-program path
    let _ = args.next();

    // first actual argument should be the puzzle number
    let puzzle_num = args
        .next()
        .ok_or("Expected a puzzle number")?
        .parse::<u8>()?;

    let mut debug_on = false;
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--debug" => { debug_on = true },
            other => Err(format!("Unexpected argument '{}'", other))?,
        }
    }

    let input_path = puzzle_input_path(puzzle_num)?;
    println!("get input from \"{}\"", input_path.display());

    // dispatch the appropriate puzzle script based on the argument

    match puzzle_num {
        1 => puzzle01::run(&input_path),
        2 => puzzle02::run(&input_path),
        3 => puzzle03::run(&input_path),
        4 => puzzle04::run(&input_path),
        5 => puzzle05::run(&input_path, debug_on),
        6 => puzzle06::run(&input_path, debug_on),
        7 => puzzle07::run(&input_path, debug_on),
        _ => Ok(()),
    }
}
