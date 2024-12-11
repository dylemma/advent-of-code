use crate::geometry::{CharGrid, Grid, GridAddress, GridDelta, RenderTileChar};
use crate::helper::GenResult;
use colored::{ColoredString, Colorize};
use log::info;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

pub fn run(input_path: &PathBuf) -> GenResult<()> {
    info!("Hello puzzle 4!");

    // read the letters of the input to a grid, with each tile initialized with `is_match: false`
    let mut grid1 = {
        let file = File::open(input_path)?;
        let reader = BufReader::new(file);
        let mut tile_rows = Vec::new();
        for line in reader.lines() {
            let line = line?;
            let row = line.chars().map(Tile::new_unmatched).collect::<Vec<_>>();
            tile_rows.push(row);
        }
        Grid { rows: tile_rows }
    };

    // before modifying the grid, make a clone of it for part 2
    let mut grid2 = grid1.clone();

    // run the part 1 search
    let mut num_matched = 0;
    for y in 0..grid1.height() {
        for x in 0..grid1.width() {
            let addr = GridAddress(x, y);
            num_matched += find_and_mark(&mut grid1, addr, "XMAS");
        }
    }

    // log the part1 end state
    info!("XMAS Search result\n{}", CharGrid(&grid1, XmasColors));

    // run the part 2 search
    let mut num_x = 0;
    for y in 0..grid2.height() {
        for x in 0..grid2.width() {
            let addr = GridAddress(x, y);
            if let Some(()) = is_x_center(&grid2, addr) {
                mark_x(&mut grid2, addr);
                num_x += 1;
            }
        }
    }

    // log the part2 end state
    info!("X-MAS Search result\n {}", CharGrid(&grid2, XmasColors));

    // results
    info!("Found XMAS {} times", num_matched);
    info!("Found X-MAS {} times", num_x);

    Ok(())
}

#[derive(Copy, Clone)]
struct Tile {
    c: char,
    is_match: bool,
}

impl Tile {
    fn new_unmatched(c: char) -> Tile {
        Tile { c, is_match: false }
    }
}

/// Used with `CharGrid` for display coloring
struct XmasColors;

impl RenderTileChar<Tile> for XmasColors {
    fn render_tile_char(&self, tile: &Tile, _: usize, _: usize) -> ColoredString {
        let s = tile.c.to_string();
        if tile.is_match {
            match tile.c {
                'X' => s.red(),
                'M' => s.green(),
                'A' => s.yellow(),
                'S' => s.bright_blue(),
                _ => s.white(),
            }
        } else {
            ".".bright_black()
        }
    }
}


fn find_and_mark(grid: &mut Grid<Tile>, start: GridAddress, target: &str) -> u32 {
    let mut num_matched = 0;
    for direction in GridDelta::CARDINALS_AND_DIAGONALS {
        if find(grid, start, direction, target).is_some() {
            mark(grid, start, direction, target.len());
            num_matched += 1;
        }
    }
    num_matched
}

fn find(grid: &Grid<Tile>, start: GridAddress, direction: GridDelta, target: &str) -> Option<()> {
    let mut pos = Some(start);
    for expected in target.chars() {
        let here = pos?;
        let tile = grid.get_at(here)?;
        if tile.c != expected {
            return None;
        }
        pos = here.checked_add(direction);
    }
    Some(())
}

fn mark(grid: &mut Grid<Tile>, start: GridAddress, direction: GridDelta, len: usize) {
    let mut pos = Some(start);
    for _ in 0..len {
        if let Some(here) = pos {
            if let Some(tile) = grid.get_mut_at(here) {
                tile.is_match = true;
            }
            pos = here.checked_add(direction);
        }
    }
}

fn is_x_center(grid: &Grid<Tile>, pos: GridAddress) -> Option<()> {
    // check that the center tile holds an 'A'
    let _ = grid.get_at(pos).filter(|t| t.c == 'A')?;

    fn char_value(c: char) -> u8 {
        match c {
            'M' => 1,
            'S' => 2,
            _ => 0,
        }
    }

    for delta in [GridDelta::UP_LEFT, GridDelta::UP_RIGHT] {
        let c1_value = char_value(grid.get_at(pos.checked_add(delta)?)?.c);
        let c2_value = char_value(grid.get_at(pos.checked_add(delta.inverted())?)?.c);
        if c1_value + c2_value != 3 {
            return None;
        }
    }

    Some(())
}

fn mark_x(grid: &mut Grid<Tile>, pos: GridAddress) {
    // note: assumes the pos and each the diagonally-adjacent addresses
    // are valid positions in the grid; will panic otherwise
    grid[pos].is_match = true;

    for delta in GridDelta::DIAGONALS {
        let corner_addr = pos.checked_add(delta).unwrap();
        grid[corner_addr].is_match = true;
    }
}
