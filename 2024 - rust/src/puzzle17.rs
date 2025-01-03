use crate::helper::GenResult;
use colored::Colorize;
use log::{debug, info};
use std::fmt::{Debug, Formatter, Write};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub fn run(input_path: &Path) -> GenResult<()> {
    // parse the puzzle input
    let (machine, raw_program, ops) = {
        let file = File::open(input_path)?;
        let reader = BufReader::new(file);
        // Program::read_from(reader)?
        parse_input(reader)?
    };

    // report what was parsed
    info!("Program: {:?}", raw_program);
    info!("Init: {:?}, ops: {:?}", machine, ops);

    // run the program (part 1)
    info!("Run Program with given initial state...");
    let (_, out) = run_full(machine.clone(), &ops);

    // reverse-engineer the puzzle input (part 2)
    info!("Attempt to reverse-engineer the correct value for Register A...");
    let seed_a =
        reverse_engineer(&ops, &raw_program, 0).ok_or("Couldn't reverse-engineer Register A")?;

    // verify the reverse-engineering attempt
    {
        let mut machine = machine.clone();
        machine.a = seed_a;
        let (_, out) = run_full(machine, &ops);
        assert_eq!(
            out, raw_program,
            "output mismatch: {:?} vs {:?}",
            out, raw_program
        );
    }

    // report solutions
    info!(
        "Part 1 output: '{}'",
        out.iter()
            .map(|n| n.to_string())
            .collect::<Vec<String>>()
            .join(",")
            .green()
    );
    info!("Part 2 initial A: {}", seed_a.to_string().bright_blue());

    Ok(())
}

fn parse_input<R: BufRead>(reader: R) -> GenResult<(Machine, Vec<u64>, Vec<Op>)> {
    let input_lines = reader.lines().collect::<Result<Vec<String>, _>>()?;

    let [a_line, b_line, c_line, _, p_line] = input_lines.as_slice() else {
        Err("input didn't have 5 lines")?
    };
    let register_a = a_line[12..].parse::<u64>()?;
    let register_b = b_line[12..].parse::<u64>()?;
    let register_c = c_line[12..].parse::<u64>()?;
    let raw_opcodes = p_line[9..]
        .split(',')
        .map(|s| s.parse::<u64>())
        .collect::<Result<Vec<u64>, _>>()?;

    let mut ops = Vec::new();
    let mut opcodes_itr = raw_opcodes.iter();
    while let Some(opcode) = opcodes_itr.next() {
        let operand = opcodes_itr
            .next()
            .ok_or(format!("Missing operand after opcode {}", opcode))?;
        let op = Op::try_from((*opcode, *operand))?;
        ops.push(op);
    }

    Ok((
        Machine {
            a: register_a,
            b: register_b,
            c: register_c,
        },
        raw_opcodes,
        ops,
    ))
}

/// Program memory
#[derive(Debug, Copy, Clone)]
struct Machine {
    a: u64,
    b: u64,
    c: u64,
}

/// Program interpreter commands
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Step {
    Continue,
    JumpTo(usize),
    Output(u64),
}

/// Part of the `Op` type, used with commands whose operand is a "combo" operand.
#[derive(Debug, Copy, Clone)]
enum ComboOperand {
    Const0,
    Const1,
    Const2,
    Const3,
    RegA,
    RegB,
    RegC,
}

impl ComboOperand {
    fn get_value(&self, machine: &Machine) -> u64 {
        match self {
            ComboOperand::Const0 => 0,
            ComboOperand::Const1 => 1,
            ComboOperand::Const2 => 2,
            ComboOperand::Const3 => 3,
            ComboOperand::RegA => machine.a,
            ComboOperand::RegB => machine.b,
            ComboOperand::RegC => machine.c,
        }
    }
}

impl TryFrom<u64> for ComboOperand {
    type Error = String;

    fn try_from(value: u64) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(ComboOperand::Const0),
            1 => Ok(ComboOperand::Const1),
            2 => Ok(ComboOperand::Const2),
            3 => Ok(ComboOperand::Const3),
            4 => Ok(ComboOperand::RegA),
            5 => Ok(ComboOperand::RegB),
            6 => Ok(ComboOperand::RegC),
            _ => Err(format!("Invalid combo operand: {}", value)),
        }
    }
}

/// Program operation type, per the puzzle description
#[derive(Debug, Copy, Clone)]
enum Op {
    Adv(ComboOperand),
    Bxl(u64),
    Bst(ComboOperand),
    Jnz(usize),
    Bxc,
    Out(ComboOperand),
    Bdv(ComboOperand),
    Cdv(ComboOperand),
}

impl TryFrom<(u64, u64)> for Op {
    type Error = String;

    fn try_from(value: (u64, u64)) -> Result<Self, Self::Error> {
        match value.0 {
            0 => Ok(Op::Adv(value.1.try_into()?)),
            1 => Ok(Op::Bxl(value.1)),
            2 => Ok(Op::Bst(value.1.try_into()?)),
            3 => Ok(Op::Jnz(
                usize::try_from(value.1).map_err(|e| e.to_string())?,
            )),
            4 => Ok(Op::Bxc),
            5 => Ok(Op::Out(value.1.try_into()?)),
            6 => Ok(Op::Bdv(value.1.try_into()?)),
            7 => Ok(Op::Cdv(value.1.try_into()?)),
            _ => Err(format!("Invalid opcode {}", value.1)),
        }
    }
}

/// Implementation of the opcodes as described in the puzzle
impl Op {
    fn run(&self, machine: &mut Machine) -> Step {
        match self {
            Op::Adv(operand) => {
                debug!("  A := A >> {:?}", operand);
                machine.a = machine.a >> operand.get_value(machine);
                Step::Continue
            }
            Op::Bxl(value) => {
                debug!("  B := B ^ {}", value);
                machine.b = machine.b ^ value;
                Step::Continue
            }
            Op::Bst(operand) => {
                debug!("  B := {:?} % 8", operand);
                machine.b = operand.get_value(machine) % 8;
                Step::Continue
            }
            Op::Jnz(value) => {
                if machine.a == 0 {
                    debug!("  <don't jump>");
                    Step::Continue
                } else {
                    debug!("  <jump to {}>", value);
                    Step::JumpTo(*value)
                }
            }
            Op::Bxc => {
                debug!("  B := B ^ C");
                machine.b = machine.b ^ machine.c;
                Step::Continue
            }
            Op::Out(operand) => {
                debug!("  <output {:?} % 8>", operand);
                Step::Output(operand.get_value(machine) % 8)
            }
            Op::Bdv(operand) => {
                debug!("  B := A >> {:?}", operand);
                machine.b = machine.a >> operand.get_value(machine);
                Step::Continue
            }
            Op::Cdv(operand) => {
                debug!("  C := A >> {:?}", operand);
                machine.c = machine.a >> operand.get_value(machine);
                Step::Continue
            }
        }
    }
}

/// Program interpreter that collects outputs to a `Vec`
fn run_full(mut machine: Machine, ops: &[Op]) -> (Machine, Vec<u64>) {
    let mut out = Vec::new();
    let mut pointer = 0;

    while pointer < ops.len() {
        debug!(
            "{:?}",
            ProgramState {
                machine: &machine,
                ops,
                pointer
            }
        );
        // debug!("{:?}", &ops[pointer]);
        match ops[pointer].run(&mut machine) {
            Step::Continue => pointer += 1,
            Step::Output(n) => {
                out.push(n);
                pointer += 1;
            }
            Step::JumpTo(address) => {
                pointer = address / 2;
            }
        }
    }

    (machine, out)
}

/// Alternate program interpreter that ignores the `Jump` command and ends after one iteration
fn run_single(mut machine: Machine, ops: &[Op]) -> (Machine, Vec<u64>) {
    let mut out = Vec::new();
    let mut pointer = 0;

    while pointer < ops.len() {
        debug!(
            "{:?}",
            ProgramState {
                machine: &machine,
                ops,
                pointer
            }
        );
        match ops[pointer].run(&mut machine) {
            Step::Output(n) => out.push(n),
            _ => (),
        };
        pointer += 1;
    }

    (machine, out)
}

/// See if an initial value for register A will result in the expected output after one iteration of the program loop
fn test_input_output(init_a: u64, program: &[Op], expected_output: u64) -> bool {
    let (_, out) = run_single(
        Machine {
            a: init_a,
            b: 0,
            c: 0,
        },
        program,
    );
    out.len() == 1 && out[0] == expected_output
}

/// Part 2 solution:
/// This bakes in some assumptions about the program given by the puzzle input:
/// - The program ends with `jnz 0`, i.e. it is a loop that ends when A=0
/// - The program will always contain an `adv 3` command, causing an `A >> 3` each iteration
/// - The initial values of B and C don't matter; they will always be set in terms of A before outputting
/// Given these assumptions, and the `%8` nature of the `out` operation, we can infer that the output
/// for any given iteration of the loop is based on only the lowest 3 bits of `A`. We can test all 8
/// possibilities for those bottom 3 bits, to run a single iteration of the loop and see if it outputs
/// the value we expect. If it does, we can take the successful input, bitshift it left by 3, and
/// repeat the process of guessing the next lowest bits.
/// Sometimes, there are multiple numbers in the 0..8 range that satisfy the current iteration,
/// but result in unsatisfied earlier iterations. We use recursion, in what is essentially a DFS,
/// to account for this.
fn reverse_engineer(program: &[Op], expected_output: &[u64], accum: u64) -> Option<u64> {
    if expected_output.is_empty() {
        debug!("Success with {}", accum);
        Some(accum)
    } else {
        let last_output = expected_output[expected_output.len() - 1];
        for n in 0..8 {
            let candidate = (accum << 3) | n;
            if test_input_output(candidate, program, last_output) {
                debug!(
                    "{}th output may be satisfied by {}",
                    expected_output.len(),
                    candidate
                );
                if let Some(answer) = reverse_engineer(
                    program,
                    &expected_output[0..expected_output.len() - 1],
                    candidate,
                ) {
                    return Some(answer);
                }
            }
        }
        None
    }
}

/// Helper struct for `debug!` logs
struct ProgramState<'m, 'o> {
    machine: &'m Machine,
    ops: &'o [Op],
    pointer: usize,
}

impl Debug for ProgramState<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_char('[')?;

        for (idx, op) in self.ops.iter().enumerate() {
            if idx > 0 {
                f.write_str(", ")?;
            }
            if idx == self.pointer {
                write!(f, "{}", format!("{:?}", op).bright_blue())?;
            } else {
                write!(f, "{:?}", op)?;
            }
        }
        write!(
            f,
            "], registers: {{ A: {}, B: {}, C: {} }}",
            self.machine.a.to_string().yellow(),
            self.machine.b.to_string().yellow(),
            self.machine.c.to_string().yellow()
        )
    }
}
