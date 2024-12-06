use crate::helper::GenResult;
use colored::Colorize;
use log::{debug, info};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

pub fn run(input_path: &PathBuf) -> GenResult<()> {
    info!("hello puzzle 2! {:?}", input_path);
    let reports = parse(input_path)?;

    let mut v1_count = 0;
    let mut v2_count = 0;

    for report in reports {
        let is_safe_v1 = report.is_safe();
        let is_safe_v2 = report.is_safe_v2();

        let txt = if is_safe_v1 && is_safe_v2 {
            "safe     ".green()
        } else if is_safe_v2 {
            "tolerated".yellow()
        } else {
            "unsafe   ".red()
        };

        println!("{} {:?}", txt, report.0);

        if is_safe_v1 {
            v1_count += 1
        };
        if is_safe_v2 {
            v2_count += 1
        };
    }

    info!("Solution 1: {}", v1_count);
    info!("Solution 2: {}", v2_count);

    Ok(())
}

fn parse(input_path: &PathBuf) -> GenResult<Vec<Report>> {
    let file = File::open(input_path)?;
    let mut reader = BufReader::new(file).lines();

    let mut reports = Vec::new();
    while let Some(Ok(line)) = reader.next() {
        let nums: Vec<u32> = line
            .split_whitespace()
            .map(|raw_num| raw_num.parse::<u32>())
            .collect::<Result<Vec<_>, _>>()?;
        reports.push(Report(nums));
    }

    Ok(reports)
}

#[derive(Debug)]
struct Report(Vec<u32>);

#[derive(Copy, Clone, Eq, PartialEq)]
enum Direction {
    Up,
    Down,
}

fn safe_gap(left: u32, right: u32) -> Option<Direction> {
    let dir = if left < right {
        Some(Direction::Up)
    } else if left > right {
        Some(Direction::Down)
    } else {
        None
    }?;
    let mag = u32::abs_diff(left, right);
    if mag >= 1 && mag <= 3 {
        Some(dir)
    } else {
        None
    }
}

impl Report {
    fn is_safe(&self) -> bool {
        self.scan(|_| false).is_some()
    }

    fn len(&self) -> usize {
        self.0.len()
    }

    fn is_safe_v2(&self) -> bool {
        if let Some(()) = self.scan(|_| false) {
            debug!("{:?} inherently safe", self);
            true
        } else {
            debug!("{:?} not safe", self);
            for i in 0..self.len() {
                debug!(" try skipping [{}]", i);
                if let Some(()) = self.scan(|idx| idx == i) {
                    debug!(" safe!");
                    return true;
                }
            }
            debug!(" no safe skips found");
            false
        }
    }

    fn scan(&self, to_skip: impl Fn(usize) -> bool) -> Option<()> {
        let mut itr = self
            .0
            .iter()
            .enumerate()
            .filter(|(idx, _)| !to_skip(*idx))
            .map(|(_, &value)| value);

        let first = itr.next()?;
        let second = itr.next()?;

        // establish initial direction
        let dir = safe_gap(first, second);
        if dir.is_none() {
            debug!(" unsafe initial gap: {} -> {}", first, second)
        }
        let dir = dir?;

        let mut prev = second;
        while let Some(next) = itr.next() {
            if safe_gap(prev, next) != Some(dir) {
                debug!(" unsafe gap: {}->{}", prev, next);
                return None;
            }
            prev = next;
        }

        Some(())
    }
}
