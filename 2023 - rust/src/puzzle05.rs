use std::collections::BTreeSet;
use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::ops::{Add, Range, Sub};
use std::path::Path;

use crate::helper;
use crate::helper::GenResult;
use crate::timed;

pub fn run(path: &Path, debug_on: bool) -> GenResult<()> {
    let lines = BufReader::new(File::open(path)?).lines().collect::<Result<Vec<_>, _>>()?;

    let seeds = lines[0].split(':').skip(1).next().unwrap().split_whitespace().map(helper::parse_u64).collect::<Result<Vec<_>, _>>()?;
    if debug_on { println!("seeds: {:?}", seeds); }

    // Parse the "mapping" parts of the input
    let maps = timed!("parsed input", {
        parse_maps(&lines[2..], debug_on)?
    });

    // The big trick for this puzzle was to "flatten" all of the mappings
    // into a single level. This naturally results in a larger number of
    // individual input-output mappings, but since the mappings are represented
    // as ranges, this drastically cuts down on the search space for part 2.
    // Condense the mappings into a single mega-mapping to help with part 2
    let final_merged = timed!("computed merged mappings", {
        maps.into_iter().reduce(|l, r| {
            Mappings::merge(&l, &r)
        }).unwrap()
    });

    // Part 1 solution (using the updated representation for Part 2)
    let min_seed = timed!("found minimum seed (the new way)", {
        seeds.iter().map(|seed| {
            let output = final_merged.translate(*seed);
            if debug_on { println!("seed {} -> soil {}", seed, output); }
            output
        }).min().unwrap()
    });
    println!("PART 1 RESULT: {}\n", min_seed);

    // Part 2
    let seed_ranges = parse_seed_ranges(&seeds)?;
    if debug_on { println!("Seed Ranges: {:?}", seed_ranges); }
    let (seed, min_dest) = timed!("determined minimum location for part 2", {
        final_merged.mappings.iter().flat_map(|&m| {
            let m_start = m.input_start();
            let m_end = m.input_end();
            seed_ranges.iter().filter_map(move |&Range { start: seed_start, end: seed_end }| {
                // does the seed-range overlap with the mapping input range?
                if seed_start < m_end && seed_end > m_start {
                    // the minimum output will correspond to the minimum mappable input,
                    // which will be the max of the two range's starts
                    let input = seed_start.max(m_start);
                    let dest = m.maybe_translate(input)?;
                    Some((input, dest))
                } else {
                    None
                }
            })
        }).min_by_key(|&t| t.1).unwrap()
    });
    println!("seed {} maps to minimum location", seed);
    println!("PART 2 RESULT: {}\n", min_dest);

    Ok(())
}

/// Interprets the seeds from the puzzle input as a series of ranges,
/// where each (n*2)th seed is the start of the range, and (n*2 + 1)th
/// seed is the length of the range. Used for Part 2 of the puzzle.
fn parse_seed_ranges(seeds: &Vec<u64>) -> Result<Vec<Range<u64>>, &'static str> {
    let mut out = Vec::new();
    let mut itr = seeds.into_iter();

    while let Some(start) = itr.next() {
        if let Some(length) = itr.next() {
            out.push(*start..(*start + *length));
        } else {
            return Err("Uneven number of seeds");
        }
    }

    Ok(out)
}

/// Parses the puzzle input after the initial "seeds" line, as a Vec<Mapping>
fn parse_maps<'a>(map_lines: &'a [String], debug_on: bool) -> Result<Vec<Mappings<'a>>, String> {
    let mut current_header = None;
    let mut current_buffer = Vec::new();
    let mut out = Vec::new();

    for line in map_lines {
        if let Some(header) = current_header {
            if line.is_empty() {
                // end of section
                if debug_on { println!("<end>"); }
                out.push(Mappings::new(header, current_buffer));
                current_header = None;
                current_buffer = Vec::new();
            } else {
                // line should be a mapping
                let mapping = Mapping::try_from(line.as_str())?;
                if debug_on { println!("{:?}", mapping); }
                current_buffer.push(mapping);
            }
        } else {
            if !line.is_empty() {
                // lines following an empty line should be headers
                let header = MapHeader::try_from(line.as_str())?;
                if debug_on { println!("{:?}", header); }
                current_header = Some(header);
            }
        }
    }
    if let Some(header) = current_header {
        out.push(Mappings::new(header, current_buffer));
        if debug_on { println!("<end>"); }
    }

    Ok(out)
}

/// Represents one of the numeric lines of the puzzle input.
///
/// A mapping is treated as an transformation function on u64,
/// which can be inverted as well as combined with other mappings.
#[derive(Debug, Copy, Clone)]
struct Mapping {
    start: u64,
    width: u64,
    shift: Shift,
}

impl TryFrom<&str> for Mapping {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut parts = value.split_whitespace();
        let dest_start = helper::parse_u64(parts.next().ok_or("missing first num")?)?;
        let src_start = helper::parse_u64(parts.next().ok_or("missing second num")?)?;
        let width = helper::parse_u64(parts.next().ok_or("missing third num")?)?;

        if parts.next().is_some() {
            Err(format!("too many numbers on line '{}'", value))
        } else {
            Ok(Mapping {
                start: src_start,
                width,
                shift: Shift::from_diff(src_start, dest_start),
            })
        }
    }
}

impl Mapping {
    fn input_start(&self) -> u64 { self.start }
    fn input_end(&self) -> u64 { self.start + self.width }

    fn output_start(&self) -> u64 { self.start + self.shift }
    fn output_end(&self) -> u64 { self.input_end() + self.shift }

    fn maybe_translate(&self, input: u64) -> Option<u64> {
        if input >= self.input_start() && input < self.input_end() {
            Some(input + self.shift)
        } else {
            None
        }
    }
    fn maybe_invert(&self, output: u64) -> Option<u64> {
        if output >= self.output_start() && output < self.output_end() {
            Some(output - self.shift)
        } else {
            None
        }
    }
}

/// Represents one of the "x-to-y map" parts of the input
#[derive(Debug, Copy, Clone)]
struct MapHeader<'a>(&'a str, &'a str);

impl<'a> TryFrom<&'a str> for MapHeader<'a> {
    type Error = String;

    fn try_from(line: &'a str) -> Result<Self, Self::Error> {
        if line.ends_with(" map:") {
            let (src, dest) = line
                .split_whitespace()
                .next()
                .unwrap()
                .split_once("-to-")
                .ok_or(format!("couldn't split map header '{}'", line))?;
            Ok(MapHeader(src, dest))
        } else {
            Err(format!("unrecognized map header format: '{}'", line))
        }
    }
}

/// Helper struct for representing Mappings.
///
/// A `Left` indicates a subtraction, when moving from input to output;
/// a `Right` indicates addition.
///
/// As part of the Mapping merge function, `Shift`s accumulate via addition.
/// We also define `Add<Shift>` and `Sub<Shift>` impls for `u64` to make
/// the `Mapping` translation functions a little nicer.
#[derive(Debug, Copy, Clone)]
enum Shift {
    Left(u64),
    Right(u64),
}

impl Shift {
    fn from_diff(start: u64, end: u64) -> Self {
        if start == end { Shift::Right(0) } else if start < end { Shift::Right(end - start) } else { Shift::Left(start - end) }
    }
}

impl Add<Shift> for u64 {
    type Output = u64;

    fn add(self, rhs: Shift) -> Self::Output {
        match rhs {
            Shift::Left(n) => self - n,
            Shift::Right(n) => self + n,
        }
    }
}

impl Sub<Shift> for u64 {
    type Output = u64;

    fn sub(self, rhs: Shift) -> Self::Output {
        match rhs {
            Shift::Left(n) => self + n,
            Shift::Right(n) => self - n,
        }
    }
}

impl Add<Shift> for Shift {
    type Output = Shift;

    fn add(self, rhs: Shift) -> Self::Output {
        match (self, rhs) {
            (Shift::Right(r1), Shift::Right(r2)) => Shift::Right(r1 + r2),
            (Shift::Left(l1), Shift::Left(l2)) => Shift::Left(l1 + l2),
            (Shift::Right(r), Shift::Left(l)) => {
                if r >= l { Shift::Right(r - l) } else { Shift::Left(l - r) }
            }
            (Shift::Left(l), Shift::Right(r)) => {
                if r >= l { Shift::Right(r - l) } else { Shift::Left(l - r) }
            }
        }
    }
}

/// Represents an `x-to-y map` from the puzzle input.
struct Mappings<'a> {
    header: MapHeader<'a>,
    mappings: Vec<Mapping>,
}

impl <'a> Debug for Mappings<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}\n", self.header)?;
        for m in &self.mappings {
            write!(f, "  segment[{}..{}) + {:?} -> {}..{}\n",
                   m.input_start(),
                   m.input_end(),
                   m.shift,
                   m.output_start(),
                   m.output_end(),
            )?;
        }
        Ok(())
    }
}

impl<'a> Mappings<'a> {
    fn new(header: MapHeader<'a>, mut mappings: Vec<Mapping>) -> Self {
        mappings.sort_by_key(|m| m.start);
        Mappings { header, mappings }
    }

    fn translate(&self, input: u64) -> u64 {
        self.mappings.iter().find_map(|m| m.maybe_translate(input)).unwrap_or(input)
    }
    fn invert(&self, output: u64) -> u64 {
        self.mappings.iter().find_map(|m| m.maybe_invert(output)).unwrap_or(output)
    }

    /// The real solution to the puzzle begins here!
    ///
    /// `step1` and `step2` are each "map"s from the puzzle input, e.g.
    /// the "light-to-temperature map" and "temperature-to-humidity map".
    /// The goal is to merge them into a single "light-to-humidity map".
    ///
    /// This works sort of like polynomial expansion: each individual Mapping
    /// from step1 needs to be cut into each individual mapping from step2.
    /// If the output-range of a mapping from step1 overlaps with the input-range
    /// of a mapping from step2, the two can be combined into one or more new
    /// mappings. We have to take some extra care when the overlap isn't 100%,
    /// e.g. if the output-range from step1 extends beyond the start or end
    /// of step2's input range, we have to "cut" the mapping from step1 across
    /// that boundary, mapping the left part and right part to different output
    /// ranges.
    fn merge(step1: &Mappings<'a>, step2: &Mappings<'a>) -> Mappings<'a> {
        let mut boundaries = BTreeSet::new();

        for m in &step1.mappings {
            // println!("{:?} = boundaries @ {} and {}", m, m.input_start(), m.input_end());
            boundaries.insert(m.input_start());
            boundaries.insert(m.input_end());
        }
        for m in &step2.mappings {
            // println!("invert({:?}) = boundaries @ {} and {}", m, step1.invert(m.input_start()), step1.invert(m.input_end()));
            boundaries.insert(step1.invert(m.input_start()));
            boundaries.insert(step1.invert(m.input_end()));
        }

        let mut boundary_itr = boundaries.iter().peekable();
        let mut out = Vec::new();
        while let Some(&start) = boundary_itr.next() {
            let end = boundary_itr.peek();
            match end {
                None => {
                    //println!("final segment = {}..", start)
                }
                Some(&&end) => {
                    // println!("segment = {}..{}", start, end);
                    let translated = step2.translate(step1.translate(start));
                    let width = end - start;
                    // println!("  -> {}..{}", translated, translated + width);
                    out.push(Mapping {
                        start,
                        width,
                        shift: Shift::from_diff(start, translated),
                    })
                }
            };
        }

        Mappings::new(
            MapHeader(step1.header.0, step2.header.1),
            out,
        )
    }
}