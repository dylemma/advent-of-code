use crate::geometry::{Cardinal, CardinalSet, CharGrid, Grid, RenderTileChar};
use crate::helper::GenResult;
use colored::{ColoredString, Colorize};
use log::{info, warn};
use std::collections::HashSet;
use std::fmt::Debug;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::PathBuf;

pub fn run(input_path: &PathBuf) -> GenResult<()> {
    let (initial_grid, initial_guard) = {
        let file = File::open(input_path)?;
        let mut reader = BufReader::new(file);
        parse_input(&mut reader)?
    };

    let mut patrol_state: PatrolState =
        PatrolState::new(initial_grid.clone(), initial_guard.clone());
    info!(
        "Initial Grid:\n{}",
        CharGrid(&patrol_state.grid, WithGuard(patrol_state.guard))
    );

    let mut num_steps = 0u32;
    while patrol_state.guard.is_some() {
        num_steps += 1;
        if num_steps > 100000 {
            warn!("Exceeded 100k steps; aborting");
            break;
        }

        patrol_state.advance();
    }

    info!(
        "Grid after {} steps:\n{}",
        num_steps,
        CharGrid(&patrol_state.grid, WithGuard(patrol_state.guard))
    );
    info!(
        "Visited {} tiles",
        format!("{}", patrol_state.num_traversed().to_string().yellow())
    );

    Ok(())
}

#[derive(Copy, Clone)]
enum TileState {
    Obstacle,
    ArtificialObstacle,
    Empty,
    Traversed(CardinalSet),
}

impl TileState {
    fn mark_traversed(&mut self, heading: Cardinal) {
        match self {
            TileState::Traversed(cardinals) => {
                *cardinals += heading;
            }
            TileState::Empty => {
                *self = TileState::Traversed(CardinalSet::default() + heading);
            }
            TileState::Obstacle | TileState::ArtificialObstacle => {
                panic!("Attempted to mark an obstacle as traversed");
            }
        }
    }
    fn is_traversable(&self) -> bool {
        match self {
            TileState::Traversed(_) => true,
            TileState::Empty => true,
            TileState::Obstacle => false,
            TileState::ArtificialObstacle => false,
        }
    }
}

struct HeadingArrow(Cardinal);

impl TryFrom<char> for HeadingArrow {
    type Error = ();
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '^' => Ok(HeadingArrow(Cardinal::North)),
            '>' => Ok(HeadingArrow(Cardinal::East)),
            'v' => Ok(HeadingArrow(Cardinal::South)),
            '<' => Ok(HeadingArrow(Cardinal::West)),
            _ => Err(()),
        }
    }
}

impl From<HeadingArrow> for char {
    fn from(value: HeadingArrow) -> Self {
        match value.0 {
            Cardinal::North => '^',
            Cardinal::East => '>',
            Cardinal::South => 'v',
            Cardinal::West => '<',
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
struct Guard {
    heading: Cardinal,
    pos: (usize, usize),
}

struct PatrolState {
    grid: Grid<TileState>,
    guard: Option<Guard>,
    corners: HashSet<Guard>,
    detected_loop: bool,
}

/// Used to enable fancy debugging of a `Grid<TileState>` when used with
/// `CharGrid(&grid, WithGuard(guard))`
struct WithGuard(Option<Guard>);
impl RenderTileChar<TileState> for WithGuard {
    fn render_tile_char(&self, tile: &TileState, x: usize, y: usize) -> ColoredString {
        if let Some(Guard {
            pos: (gx, gy),
            heading,
        }) = self.0
        {
            if gx == x && gy == y {
                // render the guard heading arrow instead of the tile character
                let c = char::from(HeadingArrow(heading));
                return c.to_string().green();
            }
        }

        match tile {
            TileState::ArtificialObstacle => "O".cyan(),
            TileState::Obstacle => "#".white(),
            TileState::Empty => ".".bright_black(),
            TileState::Traversed(cardinals) => cardinals.to_box_drawing_char().to_string().yellow(),
        }
    }
}

fn parse_input<T: BufRead>(reader: &mut T) -> GenResult<(Grid<TileState>, Guard)> {
    let mut rows = Vec::new();
    let mut current_row: usize = 0;
    let mut guard = None;

    for line in reader.lines() {
        let line = line?;
        let mut row = Vec::new();
        for (col, char) in line.chars().enumerate() {
            match char {
                '.' => row.push(TileState::Empty),
                '#' => row.push(TileState::Obstacle),
                other => match HeadingArrow::try_from(other) {
                    Ok(HeadingArrow(heading)) => {
                        row.push(TileState::Empty);
                        guard = Some(Guard {
                            heading,
                            pos: (col, current_row),
                        });
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

    Ok((Grid { rows }, guard.ok_or("guard not found")?))
}

impl PatrolState {
    fn new(grid: Grid<TileState>, guard: Guard) -> Self {
        PatrolState {
            grid,
            guard: Some(guard),
            corners: HashSet::new(),
            detected_loop: false,
        }
    }

    fn advance(&mut self) {
        if let PatrolState {
            grid,
            guard: Some(Guard {
                heading,
                pos: (x, y),
            }),
            corners,
            detected_loop,
        } = self
        {
            if let Some((can_enter_next_tile, x2, y2)) = next_guard_pos((*x, *y), *heading, grid) {
                if can_enter_next_tile {
                    // Guard will enter a tile;
                    // First, mark the tile they are leaving as having been traversed, via the direction they left.
                    // Then, mark the tile they are entering as having traversed via the opposite direction
                    // (e.g. while heading north, they enter a tile via its south cardinal).
                    // Finally, update the guard's position in memory.
                    if let Some(prev_tile) = grid.get_mut(*x, *y) {
                        prev_tile.mark_traversed(*heading);
                    }
                    grid.get_mut(x2, y2)
                        .unwrap() // safe because we already checked that a tile is there
                        .mark_traversed(heading.opposite());
                    *x = x2;
                    *y = y2;
                } else {
                    // Guard encountered an obstacle;
                    // They will turn right, but not update their position during this step
                    let corner = Guard {
                        heading: *heading,
                        pos: (*x, *y),
                    };
                    if corners.contains(&corner) {
                        info!("Detected loop @ {:?}", corner);
                        *detected_loop = true;
                    } else {
                        corners.insert(corner);
                    }
                    *heading = heading.turn_right();
                }
            } else {
                // guard walked off the map!
                grid.get_mut(*x, *y).unwrap().mark_traversed(*heading);
                self.guard = None
            }
        }
    }

    fn num_traversed(&self) -> u32 {
        let mut count = 0;
        for row in &self.grid.rows {
            for cell in row {
                match *cell {
                    TileState::Traversed(_) => count += 1,
                    _ => (),
                }
            }
        }
        count
    }
}

fn next_guard_pos(
    (x, y): (usize, usize),
    heading: Cardinal,
    grid: &Grid<TileState>,
) -> Option<(bool, usize, usize)> {
    let (dx, dy) = heading.into();
    let x2 = x.checked_add_signed(dx)?;
    let y2 = y.checked_add_signed(dy)?;
    let tile = grid.get(x2, y2)?;
    Some((tile.is_traversable(), x2, y2))
}
