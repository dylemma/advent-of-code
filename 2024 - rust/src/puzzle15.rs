use crate::geometry::{Cardinal, CharGrid, Grid, GridAddress, RenderTileChar};
use crate::helper::GenResult;
use colored::{ColoredString, Colorize};
use log::{debug, info};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub fn run(input_path: &Path) -> GenResult<()> {
    // Parse Input
    let (grid, directions, initial_robot_pos) = {
        let file = File::open(input_path)?;
        let reader = BufReader::new(file);
        let mut rows = Vec::new();
        let mut directions = Vec::new();
        let mut robot_pos = None;
        for line in reader.lines() {
            let line = line?;
            if line.starts_with('#') {
                let row = line
                    .chars()
                    .map(Tile::from_char)
                    .collect::<GenResult<Vec<_>>>()?;
                if let Some(x) = row.iter().position(|t| *t == Tile::Robot) {
                    let y = rows.len();
                    robot_pos = Some(GridAddress(x, y));
                }
                rows.push(row);
            } else {
                for c in line.chars() {
                    directions.push(cardinal_from_char(c)?);
                }
            }
        }

        (
            Grid { rows },
            directions,
            robot_pos.ok_or("Couldn't find robot")?,
        )
    };

    // Part 1
    {
        info!("Begin Part 1:");
        let part1_end_state = run_simulation(grid.clone(), initial_robot_pos, &directions);
        info!("Part 1 Score: {}", score(&part1_end_state));
    }

    // Part 2
    {
        info!("Begin Part 2:");
        let part2_end_state = run_simulation(
            Grid {
                rows: grid
                    .rows
                    .iter()
                    .map(|row| row.iter().flat_map(|t| t.fatten()).collect())
                    .collect(),
            },
            GridAddress(initial_robot_pos.0 * 2, initial_robot_pos.1),
            &directions,
        );
        info!("Part 2 Score: {}", score(&part2_end_state));
    }

    Ok(())
}

// Represents one tile in the problem space
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum Tile {
    Wall,
    Box,
    BoxLeft,
    BoxRight,
    Robot,
    Empty,
}

impl Tile {
    fn from_char(c: char) -> GenResult<Tile> {
        match c {
            '#' => Ok(Tile::Wall),
            'O' => Ok(Tile::Box),
            '@' => Ok(Tile::Robot),
            '.' => Ok(Tile::Empty),
            _ => Err(format!("Unrecognised tile '{}'", c).into()),
        }
    }

    fn fatten(&self) -> [Tile; 2] {
        match self {
            Tile::Wall => [Tile::Wall, Tile::Wall],
            Tile::Box => [Tile::BoxLeft, Tile::BoxRight],
            Tile::Robot => [Tile::Robot, Tile::Empty],
            Tile::Empty => [Tile::Empty, Tile::Empty],
            _ => panic!("Didn't expect to fatten a {:?}", self),
        }
    }
}

// For pretty colors
impl RenderTileChar<Tile> for () {
    fn render_tile_char(&self, tile: &Tile, _: usize, _: usize) -> ColoredString {
        match tile {
            Tile::Wall => "#".normal(),
            Tile::Box => "O".bright_blue(),
            Tile::Robot => "@".yellow(),
            Tile::Empty => ".".bright_black(),
            Tile::BoxLeft => "[".bright_blue(),
            Tile::BoxRight => "]".bright_blue(),
        }
    }
}

// For input parsing
fn cardinal_from_char(c: char) -> GenResult<Cardinal> {
    match c {
        '^' => Ok(Cardinal::North),
        '>' => Ok(Cardinal::East),
        'v' => Ok(Cardinal::South),
        '<' => Ok(Cardinal::West),
        _ => Err(format!("Unrecognised direction '{}'", c).into()),
    }
}

/// Run the robot through all the directional instructions, pushing boxes around as it goes
fn run_simulation(
    mut grid: Grid<Tile>,
    mut robot_pos: GridAddress,
    instructions: &Vec<Cardinal>,
) -> Grid<Tile> {
    info!("Initial State:\n{}", CharGrid(&grid, ()));

    for &direction in instructions.iter() {
        debug!("Move {:?}", direction);

        if can_displace_tile(robot_pos, direction, &grid) {
            displace_tile(robot_pos, direction, &mut grid);
            robot_pos = robot_pos.checked_add(direction.into()).unwrap()
        } else {
            debug!("clunk");
        }

        debug!("State:\n{}", CharGrid(&grid, ()));
    }
    info!("Final State:\n{}", CharGrid(&grid, ()));
    grid
}

/// If the tile at `addr` would be pushed in the given `direction`, find the tile(s)
/// adjacent to it that would also need to be pushed. This accounts for "wide" tiles
/// for the Part 2 twist. Non-recursive; only the immediately-adjacent tile(s) are returned.
fn find_displaced_when_pushed(
    addr: GridAddress,
    direction: Cardinal,
    grid: &Grid<Tile>,
) -> Vec<GridAddress> {
    let this_tile = grid[addr];

    // depending on the tile being pushed, we may need to displace up to 2 other tiles
    // (i.e. when pushing a "big" box, both of its sides will move and each may displace
    // their own respective tiles)
    match this_tile {
        Tile::Empty => {
            panic!("Tried to push an empty tile?")
        }
        Tile::Wall => {
            panic!("Tried to push a wall?")
        }
        Tile::Box | Tile::Robot => {
            // a single-tile box only displaces the next tile over
            addr.checked_add(direction.into()).into_iter().collect()
        }
        Tile::BoxLeft | Tile::BoxRight
            if direction == Cardinal::East || direction == Cardinal::West =>
        {
            // a double-tile box being moved left or right will only really displace
            // a single tile to that left or right direction, but rather than computing
            // that displaced tile, we'll just pretend it's a normal box displacing a
            // tile next to it, and allow the recursion to reach the far side of the box
            addr.checked_add(direction.into()).into_iter().collect()
        }
        Tile::BoxLeft => {
            // when a double-tile box is pushed up or down, its other side is also pushed
            // with it, causing its own displacement(s)
            let left_displaced = addr.checked_add(direction.into());
            let right_displaced = addr
                .checked_add(Cardinal::East.into())
                .and_then(|xy| xy.checked_add(direction.into()));
            left_displaced
                .into_iter()
                .chain(right_displaced.into_iter())
                .collect()
        }
        Tile::BoxRight => {
            // same as with BoxLeft case, but the other side is to the West
            let left_displaced = addr
                .checked_add(Cardinal::West.into())
                .and_then(|xy| xy.checked_add(direction.into()));
            let right_displaced = addr.checked_add(direction.into());
            left_displaced
                .into_iter()
                .chain(right_displaced.into_iter())
                .collect()
        }
    }
}

/// Check if a tile in the grid can be "pushed" in a given direction.
///
/// `addr` is the address of the tile to be pushed
/// `grid` is the grid where the tile is stored
/// `direction` is the direction to push the tile
fn can_displace_tile(addr: GridAddress, direction: Cardinal, grid: &Grid<Tile>) -> bool {
    let this_tile = grid[addr];

    match this_tile {
        Tile::Wall => false,
        Tile::Empty => true,
        _ => {
            // for the various Box tile types, displacing the tile in any direction may
            // recursively displace other tiles in that same direction; only if all can
            // be displaced may the tile at `addr` be displaced
            find_displaced_when_pushed(addr, direction, grid)
                .into_iter()
                .all(|to_displace| can_displace_tile(to_displace, direction, grid))
        }
    }
}

/// Move the tile located at `addr` in the given `direction` by one step.
/// Should be guarded by a call to `can_displace_tile`; may panic or behave unexpectedly otherwise.
fn displace_tile(addr: GridAddress, direction: Cardinal, grid: &mut Grid<Tile>) {
    let this_tile = grid[addr];

    match this_tile {
        Tile::Wall => {
            panic!("Tried to displace a wall")
        }
        Tile::Empty => {
            // nothing to do
        }
        _ => {
            for to_displace in find_displaced_when_pushed(addr, direction, grid) {
                // push any tiles out of the way
                displace_tile(to_displace, direction, grid);

                // Now that the displaced tiles are out of the way, tiles can move into their place.
                // To account for double-wide tiles, we'll use `to_displace - direction` instead of `addr`.
                if let Some(asdf) = to_displace.checked_add(direction.opposite().into()) {
                    grid[to_displace] = grid[asdf];
                    grid[asdf] = Tile::Empty;
                }
            }
        }
    }
}

fn score(grid: &Grid<Tile>) -> usize {
    let mut accum = 0;
    for y in 0..grid.height() {
        for x in 0..grid.width() {
            match grid[GridAddress(x, y)] {
                Tile::Box | Tile::BoxLeft => {
                    accum += (y * 100) + x;
                }
                _ => (),
            }
        }
    }
    accum
}
