use crate::geometry::{CharGrid, Grid, GridAddress, GridDelta, RenderTileChar};
use crate::helper::GenResult;
use colored::{ColoredString, Colorize};
use log::info;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub fn run(input_path: &Path) -> GenResult<()> {
    let input_grid = {
        let file = File::open(input_path)?;
        let reader = BufReader::new(file);
        let mut rows = Vec::new();
        for line in reader.lines() {
            let tiles = line?
                .chars()
                .map(|c| match c {
                    '.' => Tile {
                        content: None,
                        has_antinode: false,
                    },
                    c => Tile {
                        content: Some(c),
                        has_antinode: false,
                    },
                })
                .collect::<Vec<Tile>>();
            rows.push(tiles);
        }
        Grid { rows }
    };
    info!("Input state:\n{}", CharGrid(&input_grid, GridDisplay));

    // efficient data structure for getting inputs to "anti-node" detection
    let positions = collect_positions(&input_grid);

    let p1_grid = {
        let mut grid = input_grid.clone();
        for (c, addresses) in positions.iter() {
            project_all_antinodes(addresses, |antinode_pos| {
                if let Some(tile) = grid.get_mut_at(antinode_pos) {
                    tile.has_antinode = true;
                }
            })
        }
        grid
    };
    info!("Part 1 state:\n{}", CharGrid(&p1_grid, GridDisplay));
    info!("Part 1 count: {}", count_antinodes(&p1_grid));

    let p2_grid = {
        let mut grid = input_grid.clone();
        for (c, addresses) in positions.iter() {
            project_further_antinodes(addresses, |antinode_pos| {
                if let Some(tile) = grid.get_mut_at(antinode_pos) {
                    tile.has_antinode = true;
                    true
                } else {
                    false
                }
            })
        }
        grid
    };
    info!("Part 2 state:\n{}", CharGrid(&p2_grid, GridDisplay));
    info!("Part 2 count: {}", count_antinodes(&p2_grid));

    Ok(())
}

#[derive(Copy, Clone)]
struct Tile {
    content: Option<char>,
    has_antinode: bool,
}

/// Marker struct for displaying the Grid<Tile> with colors
struct GridDisplay;

impl RenderTileChar<Tile> for GridDisplay {
    fn render_tile_char(&self, tile: &Tile, _: usize, _: usize) -> ColoredString {
        match tile.content {
            Some(c) if tile.has_antinode => c.to_string().red(),
            Some(c) => c.to_string().yellow(),
            None if tile.has_antinode => "#".red(),
            None => ".".bright_black(),
        }
    }
}

fn collect_positions(grid: &Grid<Tile>) -> HashMap<char, Vec<GridAddress>> {
    let mut out = HashMap::new();

    for (y, row) in grid.rows.iter().enumerate() {
        for (x, tile) in row.iter().enumerate() {
            if let Some(c) = tile.content {
                let addr = GridAddress(x, y);
                out.entry(c).or_insert_with(Vec::new).push(addr);
            }
        }
    }

    out
}

/// Part 1 anti-node projection function
fn project_all_antinodes(addresses: &Vec<GridAddress>, mut out: impl FnMut(GridAddress) -> ()) {
    for (i, a) in addresses.iter().enumerate() {
        for b in &addresses[i + 1..] {
            let vector = GridDelta::vector_between(*a, *b);

            // project an antinode looking from a to b, past b
            if let Some(addr) = b.checked_add(vector) {
                out(addr);
            }

            // project in the opposite direction, looking from b to a, past a
            if let Some(addr) = a.checked_add(vector.inverted()) {
                out(addr);
            }
        }
    }
}

/// Part 2 anti-node projection function
fn project_further_antinodes(
    addresses: &Vec<GridAddress>,
    mut out: impl FnMut(GridAddress) -> bool,
) {
    for (i, start) in addresses.iter().enumerate() {
        for end in &addresses[i + 1..] {
            let vector = GridDelta::vector_between(*start, *end);

            // the antennae themselves count as antinodes for part 2
            out(*start);
            out(*end);

            // project antinodes past B in the A->B direction
            let mut prev = *end;
            while let Some(pos) = prev.checked_add(vector) {
                prev = pos;
                if !out(pos) {
                    break;
                }
            }

            // project antinodes past A in the B->A direction
            prev = *start;
            let vector = vector.inverted();
            while let Some(pos) = prev.checked_add(vector) {
                prev = pos;
                if !out(pos) {
                    break;
                }
            }
        }
    }
}

fn count_antinodes(grid: &Grid<Tile>) -> usize {
    let mut accum = 0;
    for row in &grid.rows {
        for tile in row {
            if tile.has_antinode {
                accum += 1;
            }
        }
    }
    accum
}
