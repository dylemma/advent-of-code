use crate::helper::GenResult;
use log::*;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

pub fn run(input_path: &PathBuf) -> GenResult<()> {
    let (mut left_nums, mut right_nums) = parse(input_path)?;
    debug!("Left nums: {:?}", left_nums);
    debug!("Right nums: {:?}", right_nums);

    left_nums.sort();
    right_nums.sort();

    debug!("Left nums (sorted): {:?}", left_nums);
    debug!("Right nums (sorted): {:?}", right_nums);

    let total_dist = left_nums
        .iter()
        .zip(right_nums.iter())
        .fold(0, |acc, (&left, &right)| {
            let dist = u32::abs_diff(left, right);
            acc + dist
        });
    info!("Solution 1: {}", total_dist);

    let mut right_counts = HashMap::new();
    for n in &right_nums {
        *right_counts.entry(*n).or_insert(0u32) += 1;
    }
    debug!("Right counts: {:?}", right_counts);

    let similarity_score = left_nums.iter().fold(0, |acc, left| {
        let count = right_counts.get(left).unwrap_or(&0u32);
        acc + (count * left)
    });

    info!("Solution 2: {}", similarity_score);

    Ok(())
}

fn parse(input_path: &PathBuf) -> GenResult<(Vec<u32>, Vec<u32>)> {
    let file = File::open(input_path)?;
    let mut left_nums = Vec::new();
    let mut right_nums = Vec::new();

    let mut lines = BufReader::new(file).lines();
    let mut line_count = 0;
    while let Some(Ok(line)) = lines.next() {
        line_count += 1;
        trace!("line {}: {}", line_count, line);
        let mut spliterator = line.split_whitespace();
        let left = spliterator.next().ok_or("missing left number")?;
        let left = left.parse::<u32>()?;
        let right = spliterator.next().ok_or("missing right number")?;
        let right = right.parse::<u32>()?;

        left_nums.push(left);
        right_nums.push(right);
    }
    Ok((left_nums, right_nums))
}
