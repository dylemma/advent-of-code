use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Sum;
use std::path::Path;

use crate::helper::GenResult;

/// Main entry point for Puzzle 1
///
/// This reads the input file, interpreting each line as a `Values`
/// to compute a `sum` that represents the puzzle solutions for both
/// parts simultaneously.
pub fn run(input: &Path) -> GenResult<()> {
    println!("Hello, puzzle 1!");
    let file = File::open(input)?;
    let lines = BufReader::new(file).lines();

    let config_sum: Values = lines
        .map(|l| {
            let line: String = l?;
            let values: Values = line.as_str().try_into()?;
            println!("[input] {} = {:?}", line, values);
            Ok(values)
        })
        .sum::<GenResult<Values>>()?;

    println!("[part1] Sum: {:?}", config_sum);
    Ok(())
}

/// Represents the two different interpretations of a "line" for the Day 1 puzzle.
///
/// The `Sum` impl is provided since both parts are solved by taking the sum of their
/// respective interpretations. By providing a `Sum for Values`, we can compute the
/// sums for both parts in a single iteration.
///
/// The `TryFrom` impl is where the actual puzzle logic happens. Each part of this puzzle
/// involves interpreting a line of the input in its own way. The `TryFrom` implementation
/// computes both interpretations at once, representing the pair of results as a `Values`.
#[derive(Debug)]
struct Values {
    part1: u32,
    part2: u32,
}

impl Sum for Values {
    fn sum<I: Iterator<Item=Self>>(iter: I) -> Self {
        let mut part1 = 0u32;
        let mut part2 = 0u32;
        for line in iter {
            part1 += line.part1;
            part2 += line.part2;
        }
        Values { part1, part2 }
    }
}

impl TryFrom<&str> for Values {
    type Error = String;

    fn try_from(line: &str) -> Result<Self, Self::Error> {
        let part1l = first_digit_of(line).ok_or(format!("Couldn't find first digit in '{}'", line))?;
        let part1r = last_digit_of(line).ok_or(format!("Couldn't find last digit in '{}'", line))?;
        let part1 = part1l * 10 + part1r;

        let part2l = find_number_fwd(line).ok_or(format!("Couldn't find first number in '{}'", line))?;
        let part2r = find_number_back(line).ok_or(format!("Couldn't find last number in '{}'", line))?;
        let part2 = part2l * 10 + part2r;

        Ok(Values{ part1, part2 })
    }
}

/// Finds the first digit character in a `line`, converting it to a decimal number.
fn first_digit_of(line: &str) -> Option<u32> {
    line
        .chars()
        .find(|c| c.is_numeric())
        .and_then(|c| c.to_digit(10))
}

/// Finds the last digit character in a `line`, converting it to a decimal number.
fn last_digit_of(line: &str) -> Option<u32> {
    line
        .chars()
        .rfind(|c| c.is_numeric())
        .and_then(|c| c.to_digit(10))
}

/// Dictionary of numeric words, where the word at index `i` represents the number `i + 1`
const NUMERICS: [&str; 9] = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"];


/// Check for a digit or a word representing a number, scanning forwards from the start of the line.
///
/// Operates recursively, where each recursive step takes a slice of the previous step's `line`,
/// with the first character omitted. This also works around Rust's encoding of characters as UTF8
/// bytes internally in `str`; while not relevant for this puzzle, some characters can occupy more
/// than a single byte, so shifting the search by "one character" doesn't necessarily mean shifting
/// the search by "one byte". Each recursive step needs to step by a number of bytes equal to the
/// width of the first character in the string.
fn find_number_fwd(line: &str) -> Option<u32> {
    match line.chars().next()? {
        c @ '0'..='9' => c.to_digit(10),

        c =>
            NUMERICS
                .into_iter()
                .position(|word| line.starts_with(word))
                .map(|idx| idx as u32 + 1)
                .or_else(|| {
                    // no word match found; advance to next character if possible
                    // (respect underlying utf8 byte-level representation of `str`)
                    let char_width = c.len_utf8();
                    if line.len() > char_width {
                        find_number_fwd(&line[char_width..])
                    } else {
                        None
                    }
                })
    }
}


/// Check for a digit or a word representing a number, scanning backwards from the end of the line.
///
/// Operates recursively, where each recursive step takes a slice of the previous step's `line`,
/// with the last character omitted. This also works around Rust's encoding of characters as UTF8
/// bytes internally in `str`; while not relevant for this puzzle, some characters can occupy more
/// than a single byte, so shifting the search by "one character" doesn't necessarily mean shifting
/// the search by "one byte". Each recursive step needs to step by a number of bytes equal to the
/// width of the last character in the string.
fn find_number_back(line: &str) -> Option<u32> {
    let last_char = line.chars().last()?;

    match last_char {
        '0'..='9' => last_char.to_digit(10),

        _ =>
            // see if the current substring ends with one of the numbers
            NUMERICS
                .into_iter()
                .position(|word| line.ends_with(word))
                // return the number based on the found index
                .map(|idx| idx as u32 + 1)
                .or_else(|| {
                    // no word match found; advance by dropping the last character if possible
                    // (respect underlying utf8 byte-level representation of `str`)
                    let char_width = last_char.len_utf8();
                    let len = line.len();
                    if len > char_width {
                        // try again with all but the last char
                        find_number_back(&line[..(len - char_width)])
                    } else {
                        None
                    }
                })
    }
}
