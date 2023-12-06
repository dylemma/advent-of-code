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

pub fn parse_u32(s: &str) -> Result<u32, String> {
    s.parse::<u32>().map_err(|e| e.to_string())
}

pub fn parse_u64(s: &str) -> Result<u64, String> {
    s.parse::<u64>().map_err(|e| format!("Couldn't parse u64 from {} ({})", s, e.to_string()))
}
