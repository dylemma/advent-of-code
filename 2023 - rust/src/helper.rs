use std::env;
use std::error::Error;
use std::path::PathBuf;

pub type GenResult<A> = Result<A, Box<dyn Error>>;

pub fn puzzle_input_path(puzzle_num: u8) -> GenResult<PathBuf> {
    let mut dir = env::current_dir()?;
    dir.push("inputs");
    dir.push(format!("{:0>2}.txt", puzzle_num));
    Ok(dir)
}