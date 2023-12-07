use std::env;
use std::error::Error;
use std::io::{BufRead, Lines};
use std::path::PathBuf;
use std::time::SystemTime;

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

pub fn print_elapsed(description: &str, time: SystemTime) {
    let elapsed = time.elapsed().unwrap().as_micros();
    println!("{} in {} μs", description, elapsed);
}

#[macro_export]
macro_rules! timed {
    ($label:expr, $body:block) => {
        {
            let start = std::time::SystemTime::now();
            let result = $body;
            crate::helper::print_elapsed($label, start);
            result
        }
    }
}

pub trait EzLines {
    fn expect_next_line(&mut self) -> Result<String, String>;
}

impl <B: BufRead> EzLines for Lines<B> {
    fn expect_next_line(&mut self) -> Result<String, String> {
        self.next()
            .ok_or("missing expected line")?
            .map_err(|io_err| io_err.to_string())
    }
}
