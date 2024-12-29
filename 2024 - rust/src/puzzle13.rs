use crate::helper::GenResult;
use colored::Colorize;
use log::info;
use regex::Regex;
use std::fmt::Display;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub fn run(input_path: &Path) -> GenResult<()> {
    // parse input file
    let machines = {
        let file = File::open(input_path)?;
        let reader = BufReader::new(file);
        let mut lines = reader.lines();
        let mut out = Vec::new();

        // each "machine" is represented as three lines, followed by either a blank line or EOF
        while let Some(button_a_line) = lines.next() {
            let button_a_line = button_a_line?;
            let button_b_line = lines.next().transpose()?.ok_or("missing Button B line")?;
            let prize_line = lines.next().transpose()?.ok_or("missing Prize line")?;
            if let Some(blank_line) = lines.next() {
                let blank_line = blank_line?;
                if !blank_line.is_empty() {
                    None.ok_or(format!("unexpected non-blank line: {}", blank_line))?;
                }
            }

            out.push(Machine::from_input_lines(
                button_a_line,
                button_b_line,
                prize_line,
            )?);
        }

        out
    };

    let mut p1_cost = 0;
    let mut p2_cost = 0;

    // report the parsed input
    for (i, machine) in machines.iter().enumerate() {
        info!("Machine {}:", i + 1);

        info!("  p1: {}", machine);
        if let Some(solution) = solve(machine) {
            info!(
                "    solved with cost {} via {:?} ",
                solution.cost().to_string().bright_blue(),
                solution
            );
            p1_cost += solution.cost();
        } else {
            info!("    no solution")
        }

        let p2_machine: Machine = {
            let mut out = machine.clone();
            out.prize_pos.0 += 10000000000000;
            out.prize_pos.1 += 10000000000000;
            out
        };

        info!("  p2: {}", p2_machine);

        if let Some(solution) = solve(&p2_machine) {
            info!(
                "    solved with cost {} via {:?} ",
                solution.cost().to_string().bright_green(),
                solution
            );
            p2_cost += solution.cost();
        }
    }
    info!("Part 1 cost: {}", p1_cost.to_string().bright_blue());
    info!("Part 2 cost: {}", p2_cost.to_string().bright_green());

    Ok(())
}

#[derive(Clone)]
struct Machine {
    a_deltas: (usize, usize),
    b_deltas: (usize, usize),
    prize_pos: (usize, usize),
}

impl Display for Machine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[ Button A: X+{}, Y+{} | ",
            self.a_deltas.0.to_string().yellow(),
            self.a_deltas.1.to_string().yellow(),
        )?;
        write!(
            f,
            "Button B: X+{}, Y+{} | ",
            self.b_deltas.0.to_string().green(),
            self.b_deltas.1.to_string().green(),
        )?;
        write!(
            f,
            "Prize: X={}, Y={} ]",
            self.prize_pos.0.to_string().cyan(),
            self.prize_pos.1.to_string().cyan()
        )
    }
}

impl Machine {
    fn from_input_lines(
        button_a_line: String,
        button_b_line: String,
        prize_line: String,
    ) -> GenResult<Machine> {
        let button_pattern = Regex::new("^Button [AB]: X\\+(\\d+), Y\\+(\\d+)$")?;
        let prize_pattern = Regex::new("^Prize: X=(\\d+), Y=(\\d+)$")?;

        // capture X and Y values from the "Button A" line
        let (_, [adx, ady]) = button_pattern
            .captures(&button_a_line)
            .ok_or(format!(
                "Couldn't capture info from A line: {}",
                button_a_line
            ))?
            .extract();
        let adx: usize = adx.parse()?;
        let ady: usize = ady.parse()?;

        // capture X and Y values from the "Button B" line
        let (_, [bdx, bdy]) = button_pattern
            .captures(&button_b_line)
            .ok_or(format!(
                "Couldn't capture info from B line: {}",
                button_b_line
            ))?
            .extract();
        let bdx: usize = bdx.parse()?;
        let bdy: usize = bdy.parse()?;

        // capture X and Y values from the "Prize" line
        let (_, [prize_x, prize_y]) = prize_pattern
            .captures(&prize_line)
            .ok_or(format!(
                "Couldn't capture info from Prize line: {}",
                prize_line
            ))?
            .extract();
        let prize_x: usize = prize_x.parse()?;
        let prize_y: usize = prize_y.parse()?;

        Ok(Machine {
            a_deltas: (adx, ady),
            b_deltas: (bdx, bdy),
            prize_pos: (prize_x, prize_y),
        })
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone, Default)]
struct ButtonsPressed {
    num_a_presses: usize,
    num_b_presses: usize,
}

impl ButtonsPressed {
    fn cost(&self) -> usize {
        (self.num_a_presses * 3) + (self.num_b_presses)
    }
}

fn solve(machine: &Machine) -> Option<ButtonsPressed> {
    // Solving the machine can be done with arithmetic instead of a search.
    // Let `x` be the number of times Button A is pressed.
    // Let `y` be the number of times Button B is pressed.
    // Solve for:
    //  (x * machine.a_deltas) + (y * machine.b_deltas) = machine.prize_pos
    // Extrapolate the vector math here to two equations:
    //  (x * machine.a_deltas.0) + (y * machine.b_deltas.0) = machine.prize_pos.0
    //  (x * machine.a_deltas.1) + (y * machine.b_deltas.1) = machine.prize_pos.1
    // Then represent it as a matrix multiplication problem:
    //   [ machine.a_deltas.0   machine.b_deltas.0 ] * [ x ] = [ machine.prize_pos.0 ]
    //   [ machine.a_deltas.1   machine.b_deltas.1 ]   [ y ]   [ machine.prize_pos.1 ]
    // Then apply Cramer's rule:
    //   For any Ax = b equation (where `A` is an NxN matrix, `x` is a column vector of the unknowns,
    //   and B is a column vector of size N), if det(A) is nonzero, then each `i`th unknown can be solved
    //   by taking `det(Ai) / det(A)`, where `Ai` is the result of replacing the `i`th column in `A` with `b`.
    // Note: det([ a  b ]) = (a*d) - (c*b)
    //           [ c  d ]

    let a = machine.a_deltas.0 as i128;
    let b = machine.b_deltas.0 as i128;
    let c = machine.prize_pos.0 as i128;

    let d = machine.a_deltas.1 as i128;
    let e = machine.b_deltas.1 as i128;
    let f = machine.prize_pos.1 as i128;

    // [ a, b ] * [ x ] = [ c ]
    // [ d, e ]   [ y ]   [ f ]

    let denom = (a * e) - (b * d);
    let x_num = (c * e) - (f * b);
    let y_num = (a * f) - (d * c);

    if denom == 0 {
        None
    } else {
        let x = x_num / denom;
        let y = y_num / denom;

        // can't press a button negative times
        if x < 0 || y < 0 {
            return None;
        }
        let x = x as usize;
        let y = y as usize;

        // rule out non-integer solutions
        let pos_x = (x * machine.a_deltas.0) + (y * machine.b_deltas.0);
        let pos_y = (x * machine.a_deltas.1) + (y * machine.b_deltas.1);

        if (pos_x, pos_y) == machine.prize_pos {
            Some(ButtonsPressed {
                num_a_presses: x,
                num_b_presses: y,
            })
        } else {
            None
        }
    }
}
