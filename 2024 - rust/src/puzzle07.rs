use crate::helper::GenResult;
use colored::Colorize;
use log::info;
use std::error::Error;
use std::fmt::{Debug, Display, Formatter};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::str::FromStr;

pub fn run(input_path: &Path) -> GenResult<()> {
    let puzzle_lines: Vec<PuzzleLine> = {
        let file = File::open(input_path)?;
        let reader = BufReader::new(file);
        reader
            .lines()
            .map(|l| l?.parse::<PuzzleLine>())
            .collect::<GenResult<Vec<_>>>()?
    };

    let mut part1_sum = 0u64;
    let mut part2_sum = 0u64;

    for line in puzzle_lines {
        let solutions = line.solve();
        if solutions.is_solved() {
            part2_sum += solutions.puzzle.output;
            if !solutions.required_concat {
                part1_sum += solutions.puzzle.output;
            }
        }
        info!("{}", solutions);
    }

    info!("Part 1 sum: {}", part1_sum.to_string().green());
    info!("Part 2 sum: {}", part2_sum.to_string().yellow());

    Ok(())
}

/// Represents a line from the day 7 puzzle input
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
    fn solve(self) -> Solutions {
        if self.inputs.is_empty() {
            Solutions {
                puzzle: self,
                solutions: Vec::new(),
                required_concat: false,
            }
        } else {
            let mut required_concat = false;
            let mut solutions = (&self).inner_solve(&[Operator::Plus, Operator::Times]);
            if solutions.is_empty() {
                required_concat = true;
                solutions =
                    (&self).inner_solve(&[Operator::Plus, Operator::Times, Operator::Concat]);
            }
            Solutions {
                puzzle: self,
                solutions,
                required_concat,
            }
        }
    }

    fn inner_solve(&self, operators: &[Operator]) -> Vec<Solution> {
        let mut out = Vec::new();
        Self::recursive_solve(
            self.output,
            (self.inputs[0], Vec::new()),
            &self.inputs[1..],
            operators,
            &mut out,
        );
        out.into_iter()
            .map(|operators| Solution {
                inputs: self.inputs.clone(),
                operators,
                output: self.output,
            })
            .collect()
    }

    // Basically a DFS through a binary/trinary tree;
    // Each step inspects the next number from the `tail`, once per operator, resulting in
    // O(num_numbers ^ num_operators) worst case performance.
    // Includes an early-exit to avoid continuing down a search path whose `accum`
    // total already went past the `goal` number.
    fn recursive_solve(
        goal: u64,
        accum: (u64, Vec<Operator>),
        tail: &[u64],
        operators: &[Operator],
        out: &mut Vec<Vec<Operator>>,
    ) {
        let (total, path) = accum;
        if total > goal {
            return; // since there are no decreasing operators, use this to exit early
        }
        if tail.is_empty() {
            // recursion end case
            if total == goal {
                out.push(path)
            }
        } else {
            let head = tail[0];
            let remaining = &tail[1..];

            for op in operators {
                let next_total = op.apply(total, head);
                let mut next_path = path.clone();
                next_path.push(*op);
                Self::recursive_solve(goal, (next_total, next_path), remaining, operators, out);
            }
        }
    }
}

/// Enum of the operators defined for day 7
#[derive(Copy, Clone)]
enum Operator {
    Plus,
    Times,
    Concat,
}

impl Operator {
    fn apply(&self, a: u64, b: u64) -> u64 {
        match self {
            Operator::Plus => a + b,
            Operator::Times => a * b,
            Operator::Concat => format!("{}{}", a, b).parse().unwrap(),
        }
    }
}

// Fancy colored output for operators
impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Operator::Plus => "+".to_string().cyan(),
            Operator::Times => "*".to_string().bright_magenta(),
            Operator::Concat => "||".to_string().yellow(),
        };
        write!(f, "{}", str)
    }
}

// One possible solution to a puzzle
struct Solution {
    inputs: Vec<u64>,
    operators: Vec<Operator>,
    output: u64,
}

// Fancy colored + formatted output for a puzzle solution
impl Display for Solution {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} = ", self.output.to_string().green())?;
        for (i, n) in self.inputs.iter().enumerate() {
            if i > 0 {
                write!(f, " {} ", self.operators[i - 1].to_string().yellow())?;
            }
            write!(f, "{}", n.to_string())?;
        }
        Ok(())
    }
}

// Aggregation of solutions to a puzzle
struct Solutions {
    puzzle: PuzzleLine,
    solutions: Vec<Solution>,
    required_concat: bool,
}

impl Solutions {
    fn is_solved(&self) -> bool {
        !self.solutions.is_empty()
    }
}

// Nice output for the solutions
impl Display for Solutions {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.solutions.is_empty() {
            write!(f, "   {}", Unsolved(&self.puzzle))
        } else {
            if self.required_concat {
                write!(f, " {} ", "^".yellow())?;
            } else {
                write!(f, "{}{} ", "^".green(), "^".yellow())?;
            }
            write!(f, "{}", &self.solutions[0])?;
            if self.solutions.len() > 1 {
                write!(f, " (and {} alternate solutions)", self.solutions.len() - 1)?;
            }
            Ok(())
        }
    }
}

struct Unsolved<'a>(&'a PuzzleLine);

// Nice output for an unsolved puzzle
impl<'a> Display for Unsolved<'a> {
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
