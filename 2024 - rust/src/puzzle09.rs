use crate::helper::GenResult;
use colored::Colorize;
use log::info;
use std::collections::LinkedList;
use std::fmt::{Debug, Write};
use std::fs::File;
use std::io::BufReader;
use std::path::Path;
use utf8_chars::BufReadCharsExt;

pub fn run(input_path: &Path) -> GenResult<()> {
    let mut fs = parse_input(input_path)?;

    info!("Start: {:?}", fs);
    defrag(&mut fs);
    info!("End: {:?}", fs);
    let part1_sum = checksum(&fs);
    info!("Part 1 Checksum: {}", part1_sum);

    Ok(())
}

struct Segment {
    id: usize,
    length: usize,
    gap: usize,
}

impl Debug for Segment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[a{}]", self.id.to_string().green())?;
        for _ in 0..self.length {
            f.write_char('#')?;
        }
        for _ in 0..self.gap {
            write!(f, "{}", ".".bright_black())?;
        }
        Ok(())
    }
}

#[derive(Debug)]
struct Filesystem {
    formatted: Vec<Segment>,
    unformatted: LinkedList<Segment>,
}

fn parse_input(input_path: &Path) -> GenResult<Filesystem> {
    let file = File::open(input_path)?;
    let mut reader = BufReader::new(file);
    let mut itr = reader.chars().map(Result::unwrap).filter(|c| *c != '\n');
    let mut out = LinkedList::new();
    let mut id = 0;

    while let Some(c) = itr.next() {
        let length = {
            c.to_digit(10).ok_or(format!("not a digit: {}", c))? as usize
        };
        let gap =
            if let Some(c2) = itr.next() {
                c2.to_digit(10).ok_or(format!("not a digit: {}", c2))? as usize
            } else {
                0
            };
        out.push_back(Segment { id, length, gap });
        id += 1;
    }

    Ok(Filesystem {
        formatted: Vec::new(),
        unformatted: out,
    })
}

fn defrag(fs: &mut Filesystem) {
    while let Some(mut segment) = fs.unformatted.pop_front() {
        let gap = segment.gap;

        // move the segment to the formatted section
        segment.gap = 0;
        fs.formatted.push(segment);

        // handle the remaining gap by pulling items from the back of the filesystem
        fill_gap(gap, fs);
    }
}

fn fill_gap(gap: usize, fs: &mut Filesystem) {
    if gap > 0 {
        if let Some(mut segment) = fs.unformatted.pop_back() {
            let length = segment.length;
            if length <= gap {
                // whole segment fits in the gap
                segment.gap = 0;
                fs.formatted.push(segment);
                // recurse the fill what remains of the gap
                fill_gap(gap - length, fs);
            } else {
                // segment is too big for the gap;
                // add a slice of this segment to the formatted region,
                // and return the rest to the back of the unformatted region
                fs.formatted.push(Segment {
                    id: segment.id,
                    length: gap,
                    gap: 0,
                });
                segment.length -= gap;
                fs.unformatted.push_back(segment);
            }
        }
    }
}

fn checksum(fs: &Filesystem) -> usize {
    let mut i = 0;
    let mut accum = 0;
    for seg in fs.formatted.iter() {
        for d in 0..seg.length {
            accum += (i + d) * seg.id;
        }
        i += seg.length;
    }
    accum
}