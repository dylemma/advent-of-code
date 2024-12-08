use crate::helper::GenResult;
use colored::Colorize;
use log::info;
use std::fmt::{Debug, Formatter};
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
        Grid(tile_rows)
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
    info!("XMAS Search result\n{:?}", grid1);

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
    info!("X-MAS Search result\n {:?}", grid2);

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

impl Debug for Tile {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let s = self.c.to_string();
        if self.is_match {
            let cs = match self.c {
                'X' => s.red(),
                'M' => s.green(),
                'A' => s.yellow(),
                'S' => s.bright_blue(),
                _ => s.white(),
            };
            write!(f, "{}", cs)?;
        } else {
            write!(f, "{}", ".".bright_black())?;
        }
        Ok(())
    }
}

#[derive(Clone)]
struct Grid(Vec<Vec<Tile>>);

impl Debug for Grid {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for row in &self.0 {
            for tile in row {
                write!(f, "{:?}", tile)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

impl Grid {
    fn get(&self, addr: GridAddress) -> Option<&Tile> {
        let GridAddress(x, y) = addr;
        let row = self.0.get(y)?;
        row.get(x)
    }

    fn get_mut(&mut self, addr: GridAddress) -> Option<&mut Tile> {
        let GridAddress(x, y) = addr;
        let row = self.0.get_mut(y)?;
        row.get_mut(x)
    }

    fn height(&self) -> usize {
        self.0.len()
    }

    fn width(&self) -> usize {
        match self.0.get(0) {
            Some(row) => row.len(),
            None => 0,
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct GridAddress(usize, usize);
impl GridAddress {
    fn checked_add(&self, delta: GridDelta) -> Option<GridAddress> {
        let GridDelta(dx, dy) = delta;
        let x = self.0.checked_add_signed(dx)?;
        let y = self.1.checked_add_signed(dy)?;
        Some(GridAddress(x, y))
    }
}

#[derive(Copy, Clone, Debug)]
struct GridDelta(isize, isize);

impl GridDelta {
    const UP: GridDelta = GridDelta(0, -1);
    const DOWN: GridDelta = GridDelta(0, 1);
    const LEFT: GridDelta = GridDelta(-1, 0);
    const RIGHT: GridDelta = GridDelta(1, 0);
    const UP_RIGHT: GridDelta = GridDelta(1, -1);
    const UP_LEFT: GridDelta = GridDelta(-1, -1);
    const DOWN_RIGHT: GridDelta = GridDelta(1, 1);
    const DOWN_LEFT: GridDelta = GridDelta(-1, 1);

    const CARDINALS_AND_DIAGONALS: [GridDelta; 8] = [
        GridDelta::UP,
        GridDelta::UP_RIGHT,
        GridDelta::RIGHT,
        GridDelta::DOWN_RIGHT,
        GridDelta::DOWN,
        GridDelta::DOWN_LEFT,
        GridDelta::LEFT,
        GridDelta::UP_LEFT,
    ];

    const DIAGONALS: [GridDelta; 4] = [
        GridDelta::UP_RIGHT,
        GridDelta::DOWN_RIGHT,
        GridDelta::DOWN_LEFT,
        GridDelta::UP_LEFT,
    ];

    fn inverted(&self) -> GridDelta {
        GridDelta(-self.0, -self.1)
    }
}

fn find_and_mark(grid: &mut Grid, start: GridAddress, target: &str) -> u32 {
    let mut num_matched = 0;
    for direction in GridDelta::CARDINALS_AND_DIAGONALS {
        if find(grid, start, direction, target).is_some() {
            mark(grid, start, direction, target.len());
            num_matched += 1;
        }
    }
    num_matched
}

fn find(grid: &Grid, start: GridAddress, direction: GridDelta, target: &str) -> Option<()> {
    let mut pos = Some(start);
    for expected in target.chars() {
        let here = pos?;
        let tile = grid.get(here)?;
        if tile.c != expected {
            return None;
        }
        pos = here.checked_add(direction);
    }
    Some(())
}

fn mark(grid: &mut Grid, start: GridAddress, direction: GridDelta, len: usize) {
    let mut pos = Some(start);
    for _ in 0..len {
        if let Some(here) = pos {
            if let Some(tile) = grid.get_mut(here) {
                tile.is_match = true;
            }
            pos = here.checked_add(direction);
        }
    }
}

fn is_x_center(grid: &Grid, pos: GridAddress) -> Option<()> {
    // check that the center tile holds an 'A'
    let _ = grid.get(pos).filter(|t| t.c == 'A')?;

    fn char_value(c: char) -> u8 {
        match c {
            'M' => 1,
            'S' => 2,
            _ => 0,
        }
    }

    for delta in [GridDelta::UP_LEFT, GridDelta::UP_RIGHT] {
        let c1_value = char_value(grid.get(pos.checked_add(delta)?)?.c);
        let c2_value = char_value(grid.get(pos.checked_add(delta.inverted())?)?.c);
        if c1_value + c2_value != 3 {
            return None;
        }
    }

    Some(())
}

fn mark_x(grid: &mut Grid, pos: GridAddress) {
    grid.get_mut(pos).unwrap().is_match = true;

    for delta in GridDelta::DIAGONALS {
        let corner_addr = pos.checked_add(delta).unwrap();
        grid.get_mut(corner_addr).unwrap().is_match = true;
    }
}
