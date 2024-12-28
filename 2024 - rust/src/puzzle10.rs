use std::collections::HashSet;
use crate::geometry::{CharGrid, Grid, GridAddress, GridDelta, RenderTileChar};
use crate::helper::GenResult;
use colored::{ColoredString, Colorize};
use log::{debug, info};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub fn run(input_path: &Path) -> GenResult<()> {
    let input_grid = {
        let file = File::open(input_path)?;
        let reader = BufReader::new(file);
        let mut rows = Vec::new();
        for line in reader.lines() {
            let line = line?;
            let row = line
                .chars()
                .map(|c| c.to_digit(10).unwrap() as u8)
                .collect::<Vec<u8>>();
            rows.push(row);
        }
        Grid { rows }
    };

    info!("Initial grid:\n{}", CharGrid(&input_grid, GradientColors));

    let (part1_score, part1_grid) = score(&input_grid, false);
    let (part2_score, _) = score(&input_grid, true);
    info!("Path Grid:\n{}\nPart 1 Score: {}\nPart 2 Score: {}", CharGrid(&part1_grid, GradientColors), part1_score, part2_score);

    Ok(())
}

struct GradientColors;

impl RenderTileChar<u8> for GradientColors {
    fn render_tile_char(&self, tile: &u8, _: usize, _: usize) -> ColoredString {
        tile.to_string().normal()
    }
}

impl RenderTileChar<TrailTile> for GradientColors {
    fn render_tile_char(&self, tile: &TrailTile, _: usize, _: usize) -> ColoredString {
        if tile.is_on_trail {
            if tile.score > 0 {
                tile.height.to_string().green()
            } else if tile.height == 9 {
                "9".yellow()
            } else {
                tile.height.to_string().white()
            }
        } else {
            ".".bright_black()
        }
    }
}

fn score(grid: &Grid<u8>, part2: bool) -> (u32, Grid<TrailTile>) {
    let mut grid = Grid {
        rows: grid.rows.iter().map(|row| {
            row.iter().map(|n| {
                TrailTile { height: *n, is_on_trail: false, score: 0 }
            }).collect()
        }).collect(),
    };

    let mut total_score = 0u32;

    for y in 0..grid.height() {
        for x in 0..grid.width() {
            let addr = GridAddress(x, y);
            if grid[addr].height == 0 {
                let mut score = 0;
                let mut visited = HashSet::new();
                find_trail(0, addr, part2, &mut grid, &mut visited, &mut score);
                grid[addr].score = score;
                debug!("Trailhead at {:?} has score {}", addr, score);
                total_score += score;
            }
        }
    }

    (total_score, grid)
}

struct TrailTile {
    height: u8,
    is_on_trail: bool,
    score: u32,
}

fn find_trail(
    current_height: u8,
    current_addr: GridAddress,
    allow_merge: bool,
    map: &mut Grid<TrailTile>,
    visited: &mut HashSet<GridAddress>,
    score: &mut u32
) {
    // mark visited
    map[current_addr].is_on_trail = true;
    if !allow_merge && !visited.insert(current_addr) {
        return;
    }
   /* if !visited.insert(current_addr) {
        return; // avoid repeat visits
    }*/

    // success condition; early exit
    if map[current_addr].height == 9 {
        *score += 1;
        return;
    }

    // recursive search
    for delta in GridDelta::CARDINALS {
        if let Some(neighbor) = current_addr.checked_add(delta) {
            if let Some(neighbor_tile) = map.get_at(neighbor) {
                if neighbor_tile.height == current_height + 1 {
                    find_trail(neighbor_tile.height, neighbor, allow_merge, map, visited, score);
                }
            }
        }
    }
}