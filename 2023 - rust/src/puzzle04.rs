use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

use crate::helper;
use crate::helper::GenResult;

pub fn run(path: &Path) -> GenResult<()> {

    let sum: u32 = BufReader::new(File::open(path)?).lines()
        .map(|r| r.map_err(|e| e.to_string()))
        .map(|line| {
            let card = Card::try_from(line?.as_str())?;
            let score = card.score();
            println!("{:?} -> {}", card, score);
            Ok(score)
        })
        .sum::<Result<u32, String>>()?;

    println!("sum: {}", sum);
    Ok(())
}

/// Interpretation of a line of puzzle input
#[derive(Debug)]
struct Card {
    id: u32,
    winning_numbers: HashSet<u32>,
    my_numbers: Vec<u32>,
}

impl Card {
    /// Scoring function for puzzle Part 1
    fn score(&self) -> u32 {
        let num_matches = self
            .my_numbers
            .iter()
            .filter(|n| self.winning_numbers.contains(n))
            .count();

        if num_matches > 0 {
            2u32.pow(num_matches as u32 - 1)
        } else {
            0
        }
    }
}

/// Parser for `Card` from a line of puzzle input
impl TryFrom<&str> for Card {
    type Error = String;

    fn try_from(line: &str) -> Result<Self, Self::Error> {
        let (id_part, nums_part) = line.split_once(':').ok_or("Missing ':' in line")?;

        let id = (if id_part.starts_with("Card ") {
            helper::parse_u32(&id_part[5..].trim())
        } else {
            Err(format!("Couldn't find ID in '{}'", id_part))
        })?;


        let (winning_part, my_part) = nums_part
            .split_once('|')
            .ok_or("Missing '|' separator in line")?;

        let winning_numbers: HashSet<u32> = winning_part
            .split_whitespace()
            .map(helper::parse_u32)
            .collect::<Result<_, _>>()?;

        let my_numbers = my_part
            .split_whitespace()
            .map(helper::parse_u32)
            .collect::<Result<_, _>>()?;

        Ok(Self {
            id,
            winning_numbers,
            my_numbers,
        })
    }
}
