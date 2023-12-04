use std::collections::HashSet;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

use crate::helper;
use crate::helper::GenResult;

pub fn run(path: &Path) -> GenResult<()> {

    // Parse lines of input as `Card`s, collecting them into a `Vec`
    let cards: Vec<Card> = BufReader::new(File::open(path)?)
        .lines()
        .map(|line| {
            let line = line.map_err(|e| e.to_string())?;
            let card = Card::try_from(line.as_str())?;
            println!("{} -> num_matches: {}", line, card.num_matches());
            Ok(card)
        })
        .collect::<Result<_, String>>()?;

    // For Part 1, compute the sum of each card's "score"
    let score_sum: u32 = cards.iter().map(Card::score).sum();

    // For Part 2, accumulate a number of cards, where each card
    // may influence the count of some number of cards that appear
    // after it.
    let num_cards: u32 = {
        let mut card_counts = vec![1u32; cards.len()];
        for (index, card) in cards.iter().enumerate() {
            let num_matches = card.num_matches();
            let num_copies = card_counts[index];
            for dx in 1..=num_matches {
                card_counts[index + dx as usize] += num_copies;
            }
        }
        card_counts.iter().sum()
    };

    println!("Score Sum: {}", score_sum);
    println!("Num Cards: {}", num_cards);
    Ok(())
}

/// Interpretation of a line of puzzle input
#[derive(Debug)]
struct Card {
    #[allow(unused)] // even though it didn't turn out to be relevant, it's still nice
    id: u32,
    winning_numbers: HashSet<u32>,
    my_numbers: Vec<u32>,
}

impl Card {
    /// Scoring function for puzzle Part 1
    fn score(&self) -> u32 {
        let num_matches = self.num_matches();

        if num_matches > 0 {
            2u32.pow(num_matches as u32 - 1)
        } else {
            0
        }
    }

    fn num_matches(&self) -> u32 {
        self
            .my_numbers
            .iter()
            .filter(|n| self.winning_numbers.contains(n))
            .count() as u32
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
