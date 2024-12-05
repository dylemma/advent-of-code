mod helper;

use crate::helper::*;
use std::env;

fn main() -> GenResult<()> {
    println!("Hello, world!");

    let cwd = env::current_dir().unwrap();
    println!("Running in {}", cwd.display());

    let _ = get_input(1)?;
    let _ = get_input(2)?;

    Ok(())
}
