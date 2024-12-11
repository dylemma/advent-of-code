use crate::helper::GenResult;
use log::info;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::str::FromStr;
use colored::Colorize;

pub fn run(input_path: &Path) -> GenResult<()> {
    let puzzle_lines: Vec<PuzzleLine> = {
        let file = File::open(input_path)?;
        let reader = BufReader::new(file);
        reader
            .lines()
            .map(|l| l?.parse::<PuzzleLine>())
            .collect::<GenResult<Vec<_>>>()?
    };

    let mut sum_of_solved = 0u64;
    for line in puzzle_lines {
        let solutions = line.solve();
        if solutions.is_empty() {
            info!("{}", Unsolved(line))
        } else {
            let first = &solutions[0];
            sum_of_solved += first.output;
            let second = &solutions[1..];
            if second.is_empty() {
                info!("{}", first);
            } else {
                info!("{} (and {} alternate solutions)", first, second.len());
            }
        }
    }

    info!("Sum of solved: {}", sum_of_solved.to_string().green());

    Ok(())
}

#[derive(Debug)]
struct PuzzleLine {
    output: u64,
    inputs: Vec<u64>,
}

impl FromStr for PuzzleLine {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (raw_output, raw_inputs) = s.split_once(": ").ok_or("bad split")?;
        let output: u64 = raw_output.parse()?;
        let inputs = raw_inputs
            .split_whitespace()
            .map(|s| s.parse::<u64>())
            .collect::<Result<Vec<u64>, _>>()?;
        Ok(PuzzleLine { output, inputs })
    }
}

impl PuzzleLine {
    fn solve(&self) -> Vec<Solution> {
        if self.inputs.is_empty() {
            Vec::new()
        } else {
            let mut out = Vec::new();
            recursive_solve(self.output, (self.inputs[0], Vec::new()), &self.inputs[1..], &mut out);
            out.into_iter().map(|operators| {
                Solution {
                    inputs: self.inputs.clone(),
                    operators,
                    output: self.output,
                }
            }).collect()
        }
    }
}

fn recursive_solve(goal: u64, accum: (u64, Vec<Operator>), tail: &[u64], out: &mut Vec<Vec<Operator>>) {
    let (total, path) = accum;
    if tail.is_empty() {
        // recursion end case
        if total == goal {
            out.push(path)
        }
    } else {
        let head = tail[0];
        let remaining = &tail[1..];

        for op in [Operator::Plus, Operator::Times] {
            let next_total = op.apply(total, head);
            let mut next_path = path.clone();
            next_path.push(op);
            recursive_solve(goal, (next_total, next_path), remaining, out);
        }
    }
}

#[derive(Copy, Clone)]
enum Operator {
    Plus,
    Times,
}

impl Operator {
    fn apply(&self, a: u64, b: u64) -> u64 {
        match self {
            Operator::Plus => a + b,
            Operator::Times => a * b,
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Operator::Plus => "+".to_string().cyan(),
            Operator::Times => "*".to_string().bright_magenta(),
        };
        write!(f, "{}", str)
    }
}

struct Solution {
    inputs: Vec<u64>,
    operators: Vec<Operator>,
    output: u64,
}

impl Display for Solution {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = ", self.output.to_string().green());
        for (i, n) in self.inputs.iter().enumerate() {
            if i > 0 {
                write!(f, " {} ", self.operators[i - 1].to_string().yellow())?;
            }
            write!(f, "{}", n.to_string())?;
        }
        Ok(())
    }
}

struct Unsolved(PuzzleLine);

impl Display for Unsolved {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = ", self.0.output.to_string().red())?;
        for (i, n) in self.0.inputs.iter().enumerate() {
            if i > 0 {
                write!(f, " {} ", "?".red())?;
            }
            write!(f, "{}", n.to_string())?;
        }
        Ok(())
    }
}