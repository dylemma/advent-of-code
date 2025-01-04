use crate::geometry::{Cardinal, Grid, GridAddress};
use crate::helper::GenResult;
use colored::Colorize;
use log::{debug, info, trace};
use pathfinding::prelude::{astar, dfs};
use std::collections::{BTreeMap, HashMap, VecDeque};
use std::fmt::{Display, Formatter, Write};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub fn run(input_path: &Path, is_example: bool) -> GenResult<()> {
    let track = {
        let file = File::open(input_path)?;
        let mut reader = BufReader::new(file);
        let mut start = GridAddress::default();
        let mut end = GridAddress::default();
        let mut rows = Vec::new();
        for (y, line) in reader.lines().enumerate() {
            let mut row = Vec::new();
            for (x, c) in line?.chars().enumerate() {
                match c {
                    '#' => row.push(TrackTile::Wall),
                    '.' => row.push(TrackTile::Open),
                    'S' => {
                        start = GridAddress(x, y);
                        row.push(TrackTile::Open);
                    }
                    'E' => {
                        end = GridAddress(x, y);
                        row.push(TrackTile::Open);
                    }
                    _ => Err("unexpected tile character")?,
                }
            }
            rows.push(row);
        }

        Track {
            start,
            end,
            tiles: Grid { rows },
        }
    };

    let p1_threshold = if is_example { 2 } else { 100 };
    let p2_threshold = if is_example { 50 } else { 100 };

    let p1_duration = 2;
    let p2_duration = 20;

    info!("Track:\n{}", track);
    let path = get_path(&track).ok_or("Couldn't find path through track")?;
    info!("Path steps: {}", path.len() - 1);
    debug!("Path: {:?}", path);

    let p1_skips = count_skips(&path, p1_threshold, p1_duration);
    info!("Part 1: found {} skips that save at least {}ps", p1_skips.to_string().green(), p1_threshold);

    let p2_skips = count_skips(&path, p2_threshold, p2_duration);
    info!("Part 2: found {} skips that save at least {}ps", p2_skips.to_string().bright_blue(), p2_threshold);

    Ok(())
}

enum TrackTile {
    Wall,
    Open,
}

struct Track {
    start: GridAddress,
    end: GridAddress,
    tiles: Grid<TrackTile>,
}

impl Display for Track {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for y in 0..self.tiles.height() {
            if y > 0 {
                f.write_char('\n')?;
            }
            for x in 0..self.tiles.width() {
                let addr = GridAddress(x, y);
                if addr == self.start {
                    write!(f, "{}", "S".green())?;
                } else if addr == self.end {
                    write!(f, "{}", "E".green())?;
                } else {
                    match self.tiles[addr] {
                        TrackTile::Wall => f.write_char('#')?,
                        TrackTile::Open => write!(f, "{}", ".".bright_black())?,
                    }
                }
            }
        }
        Ok(())
    }
}

/// Get the main path through the track, such that `result[0]` is the start address,
/// `result[result.len() - 1]` is the end address, and for any given index `i` of
/// `result`, `result[i]` would take `i` picoseconds to reach from the start.
fn get_path(track: &Track) -> Option<Vec<GridAddress>> {
    dfs(
        track.start,
        |here| {
            Cardinal::ALL
                .iter()
                .flat_map(|&dir| here.checked_add(dir.into()))
                .filter(|there| match track.tiles.get_at(*there) {
                    Some(TrackTile::Open) => true,
                    _ => false,
                })
                .collect::<Vec<_>>()
        },
        |here| *here == track.end,
    )
}

/// This puzzle is *not* a pathfinding problem.
/// A "skip" is just a direct path of cardinal-direction steps between two addresses along the path
/// through the track. The skip duration restricts how far the skip can go, but since collision is
/// "off" during the skip, we don't actually have to search for a path, only check the distance.
/// Since there is only a single main path through the track, we can establish the search space for
/// skips by taking any two addresses in the path that are sufficiently far apart, index-wise.
fn count_skips(path: &Vec<GridAddress>, threshold: usize, max_skip_duration: usize) -> usize {
    let mut count = 0;
    let mut counts_by_savings = BTreeMap::<usize, usize>::new();
    for i in 0..path.len() {
        for j in (i + threshold)..path.len() {
            let start = &path[i];
            let end = &path[j];
            let abs_dx = start.0.abs_diff(end.0);
            let abs_dy = start.1.abs_diff(end.1);
            let dist = abs_dx + abs_dy;
            let time_saved = j - i - dist;
            if dist <= max_skip_duration && time_saved >= threshold{
                trace!("Found skip from {:?} to {:?}, saving {} ps", start, end, time_saved);
                count += 1;
                *counts_by_savings.entry(time_saved).or_default() += 1;
            }
        }
    }
    debug!("Skips summary: {:?}", counts_by_savings);
    count
}
