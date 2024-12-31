mod helper;
mod puzzle01;
mod puzzle02;
mod puzzle03;
mod puzzle04;
mod puzzle05;
mod puzzle06;
mod geometry;
mod puzzle07;
mod puzzle08;
mod puzzle09;
mod puzzle10;
mod puzzle11;
mod puzzle12;
mod puzzle13;
mod puzzle14;
mod puzzle15;
mod puzzle16;
mod puzzle17;

use crate::helper::*;
use env_logger::Builder;
use log::{debug, error, info, LevelFilter};
use std::env;

fn main() -> GenResult<()> {
    let mut args = env::args().skip(1);

    let puzzle_num = args.next().ok_or("Expected puzzle number")?.parse::<u32>().map_err(|err| {
        format!("Expected a puzzle number, but got a parsing error: {}", err)
    })?;

    let mut log_level_filter = LevelFilter::Info;
    let mut is_example_input = false;

    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--debug" => {
                log_level_filter = LevelFilter::Debug;
            }
            "--trace" => {
                log_level_filter = LevelFilter::Trace;
            }
            "--example" => {
                is_example_input = true;
            },
            other => Err(format!("Unexpected argument: {}", other))?,
        }
    }

    // initialize global logger to enable `info!` etc
    Builder::new()
        .filter(None, log_level_filter)
        .format_module_path(false)
        .format_target(false)
        .init();

    info!("Running puzzle {}", puzzle_num);

    let puzzle_input_path = if is_example_input {
        env::current_dir()?.join(format!("example_inputs/{}.txt", puzzle_num))
    } else {
        get_input(puzzle_num)?
    };
    debug!("Input path: {:?}", puzzle_input_path);

    match puzzle_num {
        1 => puzzle01::run(&puzzle_input_path)?,
        2 => puzzle02::run(&puzzle_input_path)?,
        3 => puzzle03::run(&puzzle_input_path)?,
        4 => puzzle04::run(&puzzle_input_path)?,
        5 => puzzle05::run(&puzzle_input_path)?,
        6 => puzzle06::run(&puzzle_input_path)?,
        7 => puzzle07::run(&puzzle_input_path)?,
        8 => puzzle08::run(&puzzle_input_path)?,
        9 => puzzle09::run(&puzzle_input_path)?,
        10 => puzzle10::run(&puzzle_input_path)?,
        11 => puzzle11::run(&puzzle_input_path)?,
        12 => puzzle12::run(&puzzle_input_path)?,
        13 => puzzle13::run(&puzzle_input_path)?,
        14 => puzzle14::run(&puzzle_input_path, is_example_input)?,
        15 => puzzle15::run(&puzzle_input_path)?,
        16 => puzzle16::run(&puzzle_input_path)?,
        17 => puzzle17::run(&puzzle_input_path)?,
        _ => error!("That puzzle isn't solved yet"),
    }

    Ok(())
}
