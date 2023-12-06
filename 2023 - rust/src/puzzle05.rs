use std::fmt::{Debug, Formatter};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::ops::Range;
use std::path::Path;
use std::time::SystemTime;
use crate::helper;

use crate::helper::GenResult;

pub fn run(path: &Path) -> GenResult<()> {

    let lines = BufReader::new(File::open(path)?).lines().collect::<Result<Vec<_>, _>>()?;

    let seeds = lines[0].split(':').skip(1).next().unwrap().split_whitespace().map(helper::parse_u64).collect::<Result<Vec<_>, _>>()?;
    println!("seeds: {:?}", seeds);

    let maps = parse_maps(&lines[2..])?;

    let min = seeds.iter().map(|seed| {
        run_seed(*seed, &maps, false)
    }).min().unwrap();

    println!("\nMinimum: {}", min);


    let seed_ranges = parse_seed_ranges(&seeds)?;
    println!("Seed Ranges: {:?}", seed_ranges);

    let start = SystemTime::now();
    let min_v2 = (0u64..)
        .into_iter()
        .map(|loc| {
            if loc % 1000000 == 0 {
                println!("checking loc {}...", loc)
            }
            loc
        })
        .find(|loc| {
            let seed_num = backtrack_location(*loc, &maps);
            seed_ranges.iter().find(|range| range.contains(&seed_num)).is_some()
        })
        .unwrap();
    let duration = start.elapsed().map_err(|e| e.to_string())?;
    println!("Minimum valid loc: {}", min_v2);
    println!("(took {}ms)", duration.as_millis());

    Ok(())
}

fn run_seed(seed: u64, maps: &Vec<Map>, debug: bool) -> u64 {
    if debug { println!("Running seed {}", seed); }
    let mut result = seed;
    for map in maps.iter() {
        result = map.translate(result);
        if debug { println!("  {} to {} -> {}", map.header.0, map.header.1, result); }
    }
    if debug { println!("  Final: {}", result); }
    result
}

fn backtrack_location(loc: u64, maps: &Vec<Map>) -> u64 {
    let mut result = loc;
    for map in maps.iter().rev() {
        let r0 = result;
        result = map.invert(result);
    }
    result
}

fn parse_seed_ranges(seeds: &Vec<u64>) -> Result<Vec<Range<u64>>, &'static str> {
    let mut out = Vec::new();
    let mut itr = seeds.into_iter();

    while let Some(start) = itr.next() {
        if let Some(length) = itr.next() {
            out.push(*start .. (*start + *length));
        } else {
            return Err("Uneven number of seeds");
        }
    }

    Ok(out)
}

fn parse_maps<'a>(map_lines: &'a [String]) -> Result<Vec<Map<'a>>, String> {
    let mut current_header = None;
    let mut current_buffer = Vec::new();
    let mut out = Vec::new();

    for line in map_lines {
        if let Some(header) = current_header {
            if line.is_empty() {
                // end of section
                println!("<end>");
                out.push(Map {
                    header: header,
                    mappings: current_buffer,
                });
                current_header = None;
                current_buffer = Vec::new();
            } else {
                // line should be a mapping
                let mapping = Mapping::try_from(line.as_str())?;
                println!("{:?}", mapping);
                current_buffer.push(mapping);
            }
        } else {
            if !line.is_empty() {
                // lines following an empty line should be headers
                let header = MapHeader::try_from(line.as_str())?;
                println!("{:?}", header);
                current_header = Some(header);
            }
        }
    }
    if let Some(header) = current_header {
        out.push(Map {
            header,
            mappings: current_buffer,
        });
        println!("<end>");
    }

    Ok(out)
}

struct Map<'a> {
    header: MapHeader<'a>,
    mappings: Vec<Mapping>,
}

impl <'a> Map<'a> {
    fn translate(&self, input: u64) -> u64 {
        self.mappings.iter().find_map(|m| m.translate(input)).unwrap_or(input)
    }
    fn invert(&self, output: u64) -> u64 {
        self.mappings.iter().find_map(|m| m.invert(output)).unwrap_or(output)
    }
}

impl <'a> Debug for Map<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}\n", self.header)?;
        for mapping in &self.mappings {
            write!(f, "{:?}\n", mapping)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
struct Mapping {
    dest_start: u64,
    src_start: u64,
    length: u64,
}

impl Mapping {
    fn translate(&self, input: u64) -> Option<u64> {
        if input >= self.src_start {
            let offset = input - self.src_start;
            if offset < self.length {
                Some(self.dest_start + offset)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn invert(&self, output: u64) -> Option<u64> {
        if output >= self.dest_start {
            let offset = output - self.dest_start;
            if offset < self.length {
                Some(self.src_start + offset)
            } else {
                None
            }
        } else {
            None
        }
    }
}

impl TryFrom<&str> for Mapping {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let mut parts = value.split_whitespace();
        let n1 = helper::parse_u64(parts.next().ok_or("missing first num")?)?;
        let n2 = helper::parse_u64(parts.next().ok_or("missing second num")?)?;
        let n3 = helper::parse_u64(parts.next().ok_or("missing third num")?)?;

        if parts.next().is_some() {
            Err(format!("too many numbers on line '{}'", value))
        } else {
            Ok(Mapping {
                dest_start: n1,
                src_start: n2,
                length: n3,
            })
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct MapHeader<'a>(&'a str, &'a str);

impl <'a> TryFrom<&'a str> for MapHeader<'a> {
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