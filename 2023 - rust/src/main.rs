use std::env;

use crate::helper::{GenResult, puzzle_input_path};

mod puzzle01;
mod helper;

fn main() -> GenResult<()> {

    let mut args = env::args();

    // ignore this-program path
    let _ = args.next();

    // first actual argument should be the puzzle number
    let puzzle_num = args
        .next()
        .ok_or("Expected a puzzle number")?
        .parse::<u8>()?;

    // assert that no other arguments were passed
    let _ = (
        if args.next().is_none() { Ok(()) } else { Err("Too many arguments") }
    )?;

    let input_path = puzzle_input_path(puzzle_num)?;
    println!("get input from \"{}\"", input_path.display());

    // dispatch the appropriate puzzle script based on the argument

    match puzzle_num {
        1 => puzzle01::run(&input_path),
        _ => Ok(()),
    }
}