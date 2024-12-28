use crate::helper::GenResult;
use colored::Colorize;
use log::{debug, info};
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub fn run(input_path: &Path) -> GenResult<()> {
    let initial_stones = {
        let file = File::open(input_path)?;
        let mut reader = BufReader::new(file);
        let mut s = String::new();
        reader.read_line(&mut s)?;
        s.split_whitespace()
            .map(|n| Stone(n.parse::<u64>().unwrap()))
            .collect::<Vec<_>>()
    };

    info!("Initial stones: {:?}", initial_stones);

    let initial_counts = {
        let mut counts = HashMap::new();
        for stone in &initial_stones {
            *counts.entry(*stone).or_insert(0) += 1;
        }
        counts
    };

    info!("Initial counts: {:?}", initial_counts);

    let mut counts = initial_counts.clone();

    for n in 0..75 {
        counts = evolve_by_counts(counts);

        if counts.len() < 100 {
            debug!("Stone Counts: {:?}", counts);
        }

        report_counts(n + 1, score(&counts));
    }

    Ok(())
}

fn report_counts(n: u32, count: u64) {
    if n == 25 || n == 75 {
        info!(
            "After {} blinks: {} stones",
            n.to_string().green(),
            count.to_string().green()
        );
    } else {
        info!("After {} blinks, {} stones", n, count);
    }
}

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
struct Stone(u64);

impl Stone {
    fn evolve(self) -> Vec<Stone> {
        let n = self.0;
        // Rule 1: 0 becomes 1
        if n == 0 {
            return vec![Stone(1)];
        }

        // Rule 2: even-digit stones get split
        let s = n.to_string();
        if s.len() % 2 == 0 {
            let (prefix, suffix) = s.split_at(s.len() / 2);
            return vec![
                Stone(prefix.parse().unwrap()),
                Stone(suffix.parse().unwrap()),
            ];
        }

        // Rule 3: multiply by 2024
        vec![Stone(n * 2024)]
    }
}

/// Since we are only *counting* the number of stones and order doesn't matter,
/// we'll only keep track of how many of each unique stone (by its u64 id) there are.
/// When a stone would "split", the result is multiplied by however many of that
/// stone there currently are, rather than individual stones being added to a Vec.
fn evolve_by_counts(stones: HashMap<Stone, u64>) -> HashMap<Stone, u64> {
    let mut next_counts = HashMap::new();
    for (stone, count) in stones {
        for s2 in stone.evolve() {
            *next_counts.entry(s2).or_insert(0) += count;
        }
    }
    next_counts
}

fn score(stone_counts: &HashMap<Stone, u64>) -> u64 {
    stone_counts.values().sum()
}
