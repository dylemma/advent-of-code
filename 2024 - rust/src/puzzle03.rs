use crate::helper::GenResult;
use log::{debug, info, warn};
use regex::{Captures, Regex};
use std::fs::File;
use std::io::Read;
use std::path::PathBuf;

pub fn run(input_path: &PathBuf) -> GenResult<()> {
    // gobble the entire input to a String in memory
    let input = {
        let mut input_file = File::open(input_path)?;
        let mut input = String::new();
        input_file.read_to_string(&mut input)?;
        input
    };

    // create an iterator over instructions in the input, using regex
    let pattern = Instruction::pattern();
    let instructions_itr = pattern.captures_iter(&input).map(|cap| {
        Instruction::from_capture(&cap)
            .ok_or_else(|| cap.iter().next().unwrap().unwrap().as_str().to_string())
    });

    // fold the instructions into both parts during a single iteration
    let mut part1_state = Part1Fold::new();
    let mut part2_state = Part2Fold::new();
    for instruction in instructions_itr {
        match instruction {
            Err(bad_input) => {
                warn!("Bad captured input: {}", bad_input);
            }
            Ok(instruction) => {
                debug!("Instruction: {:?}", instruction);
                part1_state.run(instruction);
                part2_state.run(instruction);
            }
        }
    }

    // log results
    info!("Part1 result: {}", part1_state.accum);
    info!("Part2 result: {}", part2_state.accum);

    Ok(())
}

#[derive(Debug, Copy, Clone)]
enum Instruction {
    Mul(u32, u32),
    Deactivate,
    Activate,
}

static INSTRUCTION_PATTERN: &str = "mul\\((\\d+),(\\d+)\\)|do\\(\\)|don't\\(\\)";

impl Instruction {
    fn pattern() -> Regex {
        Regex::new(INSTRUCTION_PATTERN).unwrap()
    }

    fn from_capture(capture: &Captures) -> Option<Instruction> {
        let mut itr = capture.iter();
        let whole_match = itr.next().unwrap().unwrap().as_str();
        match whole_match {
            "do()" => Some(Instruction::Activate),
            "don't()" => Some(Instruction::Deactivate),
            _ => {
                let a = itr.next()??.as_str().parse::<u32>().ok()?;
                let b = itr.next()??.as_str().parse::<u32>().ok()?;
                Some(Instruction::Mul(a, b))
            }
        }
    }
}

struct Part1Fold {
    accum: u32,
}

impl Part1Fold {
    fn new() -> Part1Fold {
        Part1Fold { accum: 0 }
    }

    fn run(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::Mul(a, b) => {
                debug!("  part1: add {} * {}", a, b);
                self.accum += a * b;
            }
            Instruction::Activate => (),
            Instruction::Deactivate => (),
        }
    }
}

struct Part2Fold {
    accum: u32,
    active: bool,
}

impl Part2Fold {
    fn new() -> Part2Fold {
        Part2Fold {
            accum: 0,
            active: true,
        }
    }

    fn run(&mut self, instruction: Instruction) {
        match instruction {
            Instruction::Mul(a, b) => {
                if self.active {
                    debug!("  part2: add {} * {}", a, b);
                    self.accum += a * b;
                } else {
                    debug!("  part2: noop");
                }
            }
            Instruction::Activate => {
                debug!("  part2: activate");
                self.active = true;
            }
            Instruction::Deactivate => {
                debug!("  part2: deactivate");
                self.active = false;
            }
        }
    }
}
