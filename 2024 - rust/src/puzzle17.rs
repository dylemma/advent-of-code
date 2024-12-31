use crate::helper::GenResult;
use colored::Colorize;
use log::{debug, info};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Write};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub fn run(input_path: &Path) -> GenResult<()> {
    let mut program = {
        let file = File::open(input_path)?;
        let reader = BufReader::new(file);
        Program::read_from(reader)?
    };

    info!("Program: {:?}", program);

    program.run()?;
    info!("Output: {}", program.nice_output());
    Ok(())
}

struct Program {
    register_a: u32,
    register_b: u32,
    register_c: u32,
    ops: Vec<u32>,
    pointer: usize,
    output: Vec<u32>,
}

impl Debug for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_char('[')?;
        for (idx, op) in self.ops.iter().enumerate() {
            if idx > 0 {
                f.write_char(',')?;
            }
            if idx == self.pointer {
                write!(f, "{}", op.to_string().yellow())?;
            } else if idx == self.pointer + 1 {
                write!(f, "{}", op.to_string().green())?;
            } else {
                write!(f, "{}", op)?;
            }
        }
        write!(
            f,
            "], registers: {{ A: {}, B: {}, C: {} }}",
            self.register_a.to_string().bright_blue(),
            self.register_b.to_string().bright_blue(),
            self.register_c.to_string().bright_blue(),
        )
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum ProgramError {
    ReservedComboOperand,
    InvalidComboOperand,
    InvalidOpcode,
}

impl Display for ProgramError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ProgramError::ReservedComboOperand => write!(f, "Reserved combo operand"),
            ProgramError::InvalidComboOperand => write!(f, "Invalid combo operand"),
            ProgramError::InvalidOpcode => write!(f, "Invalid opcode"),
        }
    }
}

impl Error for ProgramError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(self)
    }
}

type ProgramResult<T> = Result<T, ProgramError>;

impl Program {
    fn read_from<R: BufRead>(reader: R) -> GenResult<Program> {
        let input_lines = reader.lines().collect::<Result<Vec<String>, _>>()?;

        let [a_line, b_line, c_line, _, p_line] = input_lines.as_slice() else {
            Err("input didn't have 5 lines")?
        };
        let register_a = a_line[12..].parse::<u32>()?;
        let register_b = b_line[12..].parse::<u32>()?;
        let register_c = c_line[12..].parse::<u32>()?;
        let ops = p_line[9..]
            .split(',')
            .map(|s| s.parse::<u32>())
            .collect::<Result<Vec<u32>, _>>()?;

        Ok(Program {
            register_a,
            register_b,
            register_c,
            ops,
            pointer: 0,
            output: Vec::new(),
        })
    }

    fn run(&mut self) -> ProgramResult<()> {
        while let Some(step_res) = self.step() {
            step_res?;
        }
        Ok(())
    }

    fn step(&mut self) -> Option<ProgramResult<()>> {
        debug!("State: {:?}", self);
        if self.pointer + 1 >= self.ops.len() {
            debug!("halt!");
            None
        } else {
            let opcode = self.ops[self.pointer];
            let operand = self.ops[self.pointer + 1];
            Some(self.run_op(opcode, operand))
        }
    }

    fn run_op(&mut self, opcode: u32, operand: u32) -> ProgramResult<()> {
        match opcode {
            0 => self.adv(operand),
            1 => self.bxl(operand),
            2 => self.bst(operand),
            3 => self.jnz(operand),
            4 => self.bxc(operand),
            5 => self.out(operand),
            6 => self.bdv(operand),
            7 => self.cdv(operand),
            _ => Err(ProgramError::InvalidOpcode),
        }
    }

    fn combo_operand(&self, n: u32) -> ProgramResult<u32> {
        match n {
            0..=3 => Ok(n),
            4 => Ok(self.register_a),
            5 => Ok(self.register_b),
            6 => Ok(self.register_c),
            7 => Err(ProgramError::ReservedComboOperand),
            _ => Err(ProgramError::InvalidComboOperand),
        }
    }

    fn adv(&mut self, operand: u32) -> ProgramResult<()> {
        // From the puzzle description:
        //
        // The adv instruction (opcode 0) performs division. The numerator is the value in the A register.
        // The denominator is found by raising 2 to the power of the instruction's combo operand.
        // (So, an operand of 2 would divide A by 4 (2^2); an operand of 5 would divide A by 2^B.)
        // The result of the division operation is truncated to an integer and then written to the A register.
        let numer = self.register_a;
        let denom = 2u32.pow(self.combo_operand(operand)?);
        debug!("  A := A / (2 pow {})", self.combo_operand(operand)?);
        self.register_a = numer / denom;
        self.pointer += 2;
        Ok(())
    }

    fn bxl(&mut self, operand: u32) -> ProgramResult<()> {
        debug!("  B := B ^ {}", operand);
        // From the puzzle description:
        //
        // The bxl instruction (opcode 1) calculates the bitwise XOR of register B and the
        // instruction's literal operand, then stores the result in register B.
        self.register_b = self.register_b ^ operand;
        self.pointer += 2;
        Ok(())
    }

    fn bst(&mut self, operand: u32) -> ProgramResult<()> {
        debug!("  B := {} % 8", self.combo_operand(operand)?);
        // The bst instruction (opcode 2) calculates the value of its combo operand modulo 8
        // (thereby keeping only its lowest 3 bits), then writes that value to the B register.
        self.register_b = self.combo_operand(operand)? % 8;
        self.pointer += 2;
        Ok(())
    }

    fn jnz(&mut self, operand: u32) -> ProgramResult<()> {
        // The jnz instruction (opcode 3) does nothing if the A register is 0. However,
        // if the A register is not zero, it jumps by setting the instruction pointer
        // to the value of its literal operand; if this instruction jumps, the instruction
        // pointer is not increased by 2 after this instruction.
        if self.register_a == 0 {
            debug!("  Noop");
            self.pointer += 2;
        } else {
            debug!("  Jump to {}", operand);
            self.pointer = operand as usize;
        }
        Ok(())
    }

    fn bxc(&mut self, _: u32) -> ProgramResult<()> {
        debug!("  B := {} ^ {}", self.register_b, self.register_c);
        // The bxc instruction (opcode 4) calculates the bitwise XOR of register B and register C,
        // then stores the result in register B. (For legacy reasons, this instruction reads an operand but ignores it.)
        self.register_b = self.register_b ^ self.register_c;
        self.pointer += 2;
        Ok(())
    }

    fn out(&mut self, operand: u32) -> ProgramResult<()> {
        debug!("  output.push({})", self.combo_operand(operand)? % 8);
        // The out instruction (opcode 5) calculates the value of its combo operand modulo 8, then outputs that value.
        // (If a program outputs multiple values, they are separated by commas.)
        self.output.push(self.combo_operand(operand)? % 8);
        self.pointer += 2;
        Ok(())
    }

    fn bdv(&mut self, operand: u32) -> ProgramResult<()> {
        debug!("  B := A / (2 pow {})", self.combo_operand(operand)?);
        // The bdv instruction (opcode 6) works exactly like the adv instruction except that the
        // result is stored in the B register. (The numerator is still read from the A register.)
        let numer = self.register_a;
        let denom = 2u32.pow(self.combo_operand(operand)?);
        self.register_b = numer / denom;
        self.pointer += 2;
        Ok(())
    }

    fn cdv(&mut self, operand: u32) -> ProgramResult<()> {
        debug!("  C := A / (2 pow {})", self.combo_operand(operand)?);
        // The cdv instruction (opcode 7) works exactly like the adv instruction except that the
        // result is stored in the C register. (The numerator is still read from the A register.)
        let numer = self.register_a;
        let denom = 2u32.pow(self.combo_operand(operand)?);
        self.register_c = numer / denom;
        self.pointer += 2;
        Ok(())
    }

    fn nice_output(&self) -> String {
        self.output
            .iter()
            .map(u32::to_string)
            .collect::<Vec<_>>()
            .join(",")
    }
}
