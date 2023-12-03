use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::ops::RangeInclusive;
use std::path::Path;

use crate::helper::GenResult;

pub fn run(path: &Path) -> GenResult<()> {
    // collect the puzzle input lines as a Vec
    let raw_lines: Vec<String> = BufReader::new(File::open(path)?).lines().collect::<Result<Vec<_>, _>>()?;

    // interpret each line of input as a `LineInfo`, in a separate `Vec`
    let line_infos = raw_lines
        .iter()
        .map(|line| LineInfo::from(line.as_str()))
        .collect::<Vec<_>>();

    // accumulator state for both parts
    let mut parts_sum = 0;
    let mut gears_sum = 0;

    // interpreter loop:
    // For each line, we also need to grab the previous and next lines,
    // then use them to identify "part nums" and "gear powers", which
    // get added to their respective sums during the loop.
    for (idx, current_line) in line_infos.iter().enumerate() {
        let raw_line = &raw_lines[idx];
        let prev_line = if idx > 0 { line_infos.get(idx - 1) } else { None };
        let next_line = line_infos.get(idx + 1);

        let mut debug_out = String::new();

        for num in &current_line.nums {
            if is_part_num(*num, prev_line, current_line, next_line) {
                parts_sum += num.n;
                debug_out.push_str(format!(" part({})", num.n).as_str());
            }
        }

        for gear_index in &current_line.gear_indexes {
            if let Some((g1, g2)) = as_gear(*gear_index, prev_line, current_line, next_line) {
                gears_sum += g1 * g2;
                debug_out.push_str(format!(" gear({}*{})", g1, g2).as_str());
            }
        }

        // Debug output
        println!("{}{}", raw_line, debug_out);
    }

    // Result output
    println!("\nParts Sum: {}\nGears Sum: {}", parts_sum, gears_sum);
    Ok(())
}

/// Represents a number found in one of the puzzle input lines.
///
/// Tracks the actual parsed number, along with the character
/// offset at which it was found, and how many charaters wide it is.
#[derive(Debug, Copy, Clone)]
struct Num {
    n: u32,
    index: usize,
    width: usize,
}

impl Num {
    /// Returns a Range of character offsets at which a symbol in
    /// the current or adjacent line may appear to be considered
    /// adjacent. I.e. the offset before this `Num` started (or 0
    /// if this `Num` started at 0), through index just past the
    /// end of this `Num`.
    fn adjacency_range(&self) -> RangeInclusive<usize> {
        let min = if self.index > 0 { self.index - 1 } else { 0 };
        let max = self.index + self.width;
        min..=max
    }
}

/// Represents information captured about a line of the puzzle input.
/// The `nums` represent numbers found on the line.
/// The `symbol_indexes` are the character offsets at which symbols were found.
/// The `gear_indexes` are a subset of `symbol_indexes`, specifically for `*` ("gear") symbols.
#[derive(Debug)]
struct LineInfo {
    nums: Vec<Num>,
    symbol_indexes: HashSet<usize>,
    gear_indexes: HashSet<usize>,
}

/// Parsing logic for a single line of the puzzle input.
impl From<&str> for LineInfo {
    fn from(line: &str) -> Self {
        // Scanning state values:
        let mut is_in_digit = false;
        let mut symbol_indexes = HashSet::new();
        let mut gear_indexes = HashSet::new();
        let mut nums = Vec::new();

        for (char_index, (byte_offset, c)) in line.char_indices().enumerate() {
            if c.is_ascii_digit() {
                // when we encounter the beginning of a number, set the `is_in_digit` flag
                // and then use string processing to parse the number. We'll allow the iterator
                // to continue along normally, ignoring further digits in the current number.
                if !is_in_digit {
                    is_in_digit = true;
                    let raw_num = line[byte_offset..]
                        .split(|c: char| !c.is_ascii_digit())
                        .next().unwrap();
                    let num_width = raw_num.chars().count();
                    let num = raw_num.parse::<u32>().unwrap();
                    nums.push(Num {
                        n: num,
                        index: char_index,
                        width: num_width,
                    });
                }
            } else {
                // When a non-digit is found, we can cancel the digit-scanning logic, so that
                // a new number can be processed later. Then just check if the character is a
                // symbol and/or gear.
                is_in_digit = false;
                if c != '.' {
                    symbol_indexes.insert(char_index);
                }
                if c == '*' {
                    gear_indexes.insert(char_index);
                }
            }
        }

        LineInfo {
            nums,
            symbol_indexes,
            gear_indexes,
        }
    }
}

/// Predicate used for part 1 of the puzzle.
///
/// A `Num` is considered a "part" if it is adjacent to at least one symbol.
fn is_part_num(num: Num, prev_line: Option<&LineInfo>, current_line: &LineInfo, next_line: Option<&LineInfo>) -> bool {
    let min_index = num.index.checked_sub(1).unwrap_or(num.index);
    let max_index = num.index + num.width;

    // check to the left
    if current_line.symbol_indexes.contains(&min_index) {
        return true;
    }

    // check to the right
    if current_line.symbol_indexes.contains(&max_index) {
        return true;
    }

    // check above
    if let Some(prev_line) = prev_line {
        for idx in min_index..=max_index {
            if prev_line.symbol_indexes.contains(&idx) {
                return true;
            }
        }
    }

    // check below
    if let Some(next_line) = next_line {
        for idx in min_index..=max_index {
            if next_line.symbol_indexes.contains(&idx) {
                return true;
            }
        }
    }

    // give up
    return false
}

/// Interpreter for part 2 of the puzzle.
///
/// A `*` symbol is considered a gear if it is adjacent to exactly two `Num`s.
/// We also need to capture the specific numeric values of those two `Nums`.
fn as_gear(gear_index: usize, prev_line: Option<&LineInfo>, current_line: &LineInfo, next_line: Option<&LineInfo>) -> Option<(u32, u32)> {

    let adjacent_nums = vec!(prev_line, Some(current_line), next_line)
        .iter()
        .flatten()
        .flat_map(|x| &x.nums)
        .filter(|num| num.adjacency_range().contains(&gear_index))
        .collect::<Vec<_>>();

    if adjacent_nums.len() == 2 {
        Some((adjacent_nums[0].n, adjacent_nums[1].n))
    } else {
        None
    }
}