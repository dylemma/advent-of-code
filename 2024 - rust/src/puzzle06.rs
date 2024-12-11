use crate::helper::GenResult;
use colored::Colorize;
use log::{info, warn};
use std::fmt::{Debug, Formatter, Write};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

pub fn run(input_path: &PathBuf) -> GenResult<()> {
    let file = File::open(input_path)?;
    let mut reader = BufReader::new(file);
    let mut patrol_state: PatrolState = PatrolState::parse(&mut reader)?;
    info!("Initial Grid:\n{:?}", patrol_state);

    let mut num_steps = 0u32;
    while patrol_state.guard.is_some() {
        num_steps += 1;
        if num_steps > 100000 {
            warn!("Exceeded 100k steps; aborting");
            break;
        }

        patrol_state.advance();
    }

    info!("Grid after {} steps:\n{:?}", num_steps, patrol_state);
    info!(
        "Visited {} tiles",
        format!("{}", patrol_state.grid.num_visited().to_string().yellow())
    );

    Ok(())
}

#[derive(Copy, Clone)]
enum TileState {
    Empty,
    Obstacle,
    Visited,
    Occupied,
}

#[derive(Copy, Clone, Debug)]
enum Heading {
    North,
    East,
    South,
    West,
}

impl Heading {
    fn turn_right(&self) -> Heading {
        match self {
            Heading::North => Heading::East,
            Heading::East => Heading::South,
            Heading::South => Heading::West,
            Heading::West => Heading::North,
        }
    }
}

impl Into<(isize, isize)> for Heading {
    fn into(self) -> (isize, isize) {
        match self {
            Heading::North => (0, -1),
            Heading::East => (1, 0),
            Heading::South => (0, 1),
            Heading::West => (-1, 0),
        }
    }
}

impl TryFrom<char> for Heading {
    type Error = ();
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '^' => Ok(Heading::North),
            '>' => Ok(Heading::East),
            'v' => Ok(Heading::South),
            '<' => Ok(Heading::West),
            _ => Err(()),
        }
    }
}

#[derive(Copy, Clone, Debug)]
struct Guard {
    heading: Heading,
    pos: (usize, usize),
}

struct Grid {
    tiles: Vec<Vec<TileState>>,
}

impl Grid {
    fn get(&self, x: usize, y: usize) -> Option<TileState> {
        let row = self.tiles.get(y)?;
        row.get(x).cloned()
    }

    fn set(&mut self, x: usize, y: usize, tile: TileState) -> bool {
        if let Some(row) = self.tiles.get_mut(y) {
            if let Some(cell) = row.get_mut(x) {
                *cell = tile;
                return true;
            }
        }
        false
    }

    fn num_visited(&self) -> u32 {
        let mut count = 0;
        for row in &self.tiles {
            for cell in row {
                match *cell {
                    TileState::Visited => count += 1,
                    _ => (),
                }
            }
        }
        count
    }
}

struct PatrolState {
    grid: Grid,
    guard: Option<Guard>,
}

impl Debug for PatrolState {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for row in &self.grid.tiles {
            for tile in row {
                match *tile {
                    TileState::Empty => write!(f, "{}", ".".bright_black())?,
                    TileState::Obstacle => write!(f, "{}", "#".white())?,
                    TileState::Visited => write!(f, "{}", "X".yellow())?,
                    TileState::Occupied => {
                        if let Some(Guard { heading, .. }) = self.guard {
                            let char = match heading {
                                Heading::North => "^",
                                Heading::East => ">",
                                Heading::South => "v",
                                Heading::West => "<",
                            };
                            write!(f, "{}", char.green())?;
                        } else {
                            // invalid state; guard is None, but somehow tile is occupied
                            write!(f, "{}", "?".red())?;
                        }
                    }
                }
            }
            f.write_char('\n')?;
        }

        Ok(())
    }
}

impl PatrolState {
    fn parse<T: BufRead>(reader: &mut T) -> GenResult<Self> {
        let mut guard_facing = Heading::North;
        let mut rows = Vec::new();
        let mut current_row: usize = 0;
        let mut guard_pos: Option<(usize, usize)> = None;

        for line in reader.lines() {
            let line = line?;
            let mut row = Vec::new();
            for (col, char) in line.chars().enumerate() {
                match char {
                    '.' => row.push(TileState::Empty),
                    '#' => row.push(TileState::Obstacle),
                    other => match Heading::try_from(other) {
                        Ok(heading) => {
                            row.push(TileState::Occupied);
                            guard_facing = heading;
                            guard_pos = Some((col, current_row))
                        }
                        Err(_) => {
                            Err(format!("Unrecognized character: '{}'", other))?;
                        }
                    },
                }
            }
            rows.push(row);
            current_row += 1;
        }

        Ok(PatrolState {
            grid: Grid { tiles: rows },
            guard: Some(Guard {
                heading: guard_facing,
                pos: guard_pos.ok_or("guard not found")?,
            }),
        })
    }

    fn guard_target_tile(&self) -> Option<(TileState, (usize, usize))> {
        let Guard {
            heading,
            pos: (x, y),
        } = self.guard?;
        let (dx, dy) = heading.into();
        let x2 = x.checked_add_signed(dx)?;
        let y2 = y.checked_add_signed(dy)?;
        let tile = self.grid.get(x2, y2)?;
        Some((tile, (x2, y2)))
    }

    fn advance(&mut self) {
            if let PatrolState {
                grid,
                guard:
                    Some(Guard {
                        heading,
                        pos: (x, y),
                    }),
            } = self
            {
            if let Some((next_tile, x2, y2)) = next_guard_pos((*x, *y), *heading, grid) {
                match next_tile {
                    TileState::Empty | TileState::Visited => {
                        // guard moves to that tile; current tile gets marked as visited
                        grid.set(*x, *y, TileState::Visited);
                        grid.set(x2, y2, TileState::Occupied);
                        // self.guard = Some(Guard { heading, pos: (x2, y2) })
                        *x = x2;
                        *y = y2;
                    }
                    TileState::Obstacle => {
                        // guard turns, but stays in place
                        // self.guard = Some(Guard { heading: heading.turn_right(), pos: (x, y) });
                        *heading = heading.turn_right();
                    }
                    TileState::Occupied => {
                        panic!("guard somehow ran into itself!");
                    }
                }
            } else {
                // guard walked off the map!
                grid.set(*x, *y, TileState::Visited);
                self.guard = None
            }
        }
    }
}

fn next_guard_pos((x, y): (usize, usize), heading: Heading, grid: &Grid) -> Option<(TileState, usize, usize)> {
    let (dx, dy) = heading.into();
    let x2 = x.checked_add_signed(dx)?;
    let y2 = y.checked_add_signed(dy)?;
    let tile = grid.get(x2, y2)?;
    Some((tile, x2, y2))
}
