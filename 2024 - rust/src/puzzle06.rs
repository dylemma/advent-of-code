use crate::geometry::{Cardinal, CardinalSet, CharGrid, Grid, RenderTileChar};
use crate::helper::GenResult;
use colored::{ColoredString, Colorize};
use log::{debug, info};
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

    // Log the initial state
    info!(
        "Initial Grid:\n{}",
        CharGrid(&initial_grid, WithGuard(Some(initial_guard)))
    );

    // Run the patrol with the state as given
    let (part1_state, part1_result) = run_patrol(initial_grid.clone(), initial_guard.clone());
    {
        let PatrolState { grid, guard, .. } = &part1_state;
        info!(
            "Part 1 patrol ended with {:?}:\n{}",
            part1_result,
            CharGrid(grid, WithGuard(guard.clone()))
        );
        info!(
            "Visited {} tiles",
            format!("{}", part1_state.num_traversed().to_string().yellow())
        );
    }

    let mut num_loops = 0;
    let mut num_tested = 0;
    // for any tile along the path traversed in part 1, see if inserting an obstacle on that type would induce a cycle
    for x in 0..initial_grid.width() {
        for y in 0..initial_grid.height() {
            if (x, y) != initial_guard.pos {
                if part1_state.grid.get(x, y).unwrap().is_traversed() {
                    num_tested += 1;
                    let altered_grid = {
                        let mut g = initial_grid.clone();
                        let tile = g.get_mut(x, y).unwrap();
                        *tile = TileState::ArtificialObstacle;
                        g
                    };
                    let (patrol_state, result) = run_patrol(altered_grid, initial_guard.clone());
                    match result {
                        PatrolResult::LoopDetected => {
                            // I want log output, but not log spam
                            if num_loops == 0 {
                                info!("First loop detected in \n{}", CharGrid(&patrol_state.grid, WithGuard(patrol_state.guard)));
                            } else {
                                debug!("Loop detected in:\n{}", CharGrid(&patrol_state.grid, WithGuard(patrol_state.guard)));
                            }
                            num_loops += 1;
                        },
                        _ => (),
                    }
                }
            }
        }
    }
    info!("Detected {} loop-inducing positions (out of {} attempts)", format!("{}", num_loops).cyan(), num_tested);

    Ok(())
}

fn run_patrol(initial_grid: Grid<TileState>, initial_guard: Guard) -> (PatrolState, PatrolResult) {
    let mut patrol_state = PatrolState::new(initial_grid, initial_guard);
    let mut num_steps = 0u32;

    while patrol_state.guard.is_some() && !patrol_state.detected_loop && num_steps < 100000 {
        num_steps += 1;
        patrol_state.advance();
    }

    let result = if patrol_state.guard.is_none() {
        PatrolResult::WalkedOff
    } else if patrol_state.detected_loop {
        PatrolResult::LoopDetected
    } else {
        PatrolResult::Aborted
    };

    (patrol_state, result)
}

#[derive(Debug)]
enum PatrolResult {
    WalkedOff,
    LoopDetected,
    Aborted,
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
    fn is_traversed(&self) -> bool {
        match self {
            TileState::Traversed(_) => true,
            _ => false,
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
                if cell.is_traversed() {
                    count += 1;
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
