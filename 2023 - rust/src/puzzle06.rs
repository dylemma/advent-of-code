use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

use crate::{helper, timed};
use crate::helper::{EzLines, GenResult};

pub fn run(path: &Path, debug_on: bool) -> GenResult<()> {
    let races = timed!("parsed input", {
        parse_input(path)?
    });
    if debug_on { println!("Races: {:?}", races) }

    let part1_result = timed!("compute part 1 result", {
        races
            .iter()
            .fold(1, |p, race| {
                println!("{:?}", race);
                p * race.count_possible_wins(debug_on)
            })
    });
    println!("PART 1 RESULT: {}", part1_result);

    let big_race = timed!("parse input v2", {
        parse_input_v2(path)?
    });

    if debug_on { println!("Big race: {:?}", big_race); }
    let part2_result = timed!("computed part 2 answer", {
        big_race.count_possible_wins(debug_on)
    });
    println!("PART 2 RESULT: {}", part2_result);

    Ok(())
}

#[derive(Debug, Copy, Clone)]
struct Race {
    time: u64,
    dist: u64,
}

impl Race {
    fn find_win_boundaries(&self) -> (u64, u64) {
        // the distance_after_holding function is Duration * (RaceTime - Duration),
        // and we want to identify the Duration values where that function is equal(ish)
        // to the race's record distance, i.e. `Duration * (RaceTime - Duration) - Record = 0`,
        // which expands to `-Duration^2 + Duration*RaceTime - Record = 0`, i.e. the quadratic
        // equation; `ax^2 + bx + c = 0`
        //
        // We can just use the quadratic equation `ax^2 + bx + c = 0`, where
        // `a` is -1, `b` is RaceTime, `c` is -Record, and `x` is the Duration.
        // use `x = (-b +/- sqrt(b^2 - 4ac)) / 2a
        let a = -1f64;
        let b = self.time as f64;
        let c = -(self.dist as f64);
        let sqrt_part = ((b.powi(2) - (4f64 * a * c)) as f64).sqrt();
        let denom = 2f64 * a;
        // since a is negative, the higher numerator will result in the smaller number
        let x0 = (-b + sqrt_part) / denom;
        let x1 = (-b - sqrt_part) / denom;
        (x0.ceil() as u64, x1.floor() as u64)
    }

    fn count_possible_wins(&self, debug_on: bool) -> u64 {
        let (min, max) = self.find_win_boundaries();
        let num_wins = win_count(min, max);
        if debug_on {
            println!("win by holding anywhere from {} through {} ms; that's {} possibilities", min, max, num_wins);
        }
        num_wins
    }
}

fn win_count(min_hold: u64, max_hold: u64) -> u64 {
    // the range is inclusive, so we add 1
    u64::abs_diff(min_hold, max_hold) + 1
}

fn parse_input(path: &Path) -> Result<Vec<Race>, String> {
    let mut lines = BufReader::new(File::open(path).map_err(|e| e.to_string())?).lines();

    let time_nums = parse_input_nums(lines.expect_next_line()?.as_str())?;
    let dist_nums = parse_input_nums(lines.expect_next_line()?.as_str())?;

    if time_nums.len() == dist_nums.len() {
        Ok(time_nums.into_iter().zip(dist_nums).map(|(time, dist)| {
            Race { time, dist }
        }).collect())
    } else {
        Err(format!("mismatched number of numbers: {:?} vs {:?}", time_nums, dist_nums))
    }
}

fn parse_input_nums(nums: &str) -> Result<Vec<u64>, String> {
    nums
        .split(':').skip(1).next()
        .ok_or("missing ':' in nums line")?
        .split_whitespace()
        .map(helper::parse_u64)
        .collect()
}

fn parse_input_v2(path: &Path) -> Result<Race, String> {
    let mut lines = BufReader::new(File::open(path).map_err(|e| e.to_string())?).lines();
    let time = parse_input_nums_v2(lines.expect_next_line()?.as_str())?;
    let dist = parse_input_nums_v2(lines.expect_next_line()?.as_str())?;
    Ok(Race { time, dist })
}

fn parse_input_nums_v2(nums: &str) -> Result<u64, String> {
    let concated_nums = nums
        .split(':').skip(1).next()
        .ok_or("missing ':' in nums line")?
        .split_whitespace()
        .collect::<String>();
    helper::parse_u64(concated_nums.as_str())
}
