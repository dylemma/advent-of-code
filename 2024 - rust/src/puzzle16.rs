use crate::geometry::{Cardinal, CardinalSet, CharGrid, Grid, GridAddress, RenderTileChar};
use crate::helper::GenResult;
use colored::{ColoredString, Colorize};
use log::info;
use pathfinding::prelude::dijkstra;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub fn run(input_path: &Path) -> GenResult<()> {
    let (maze, maze_meta) = parse_input(input_path)?;

    info!("Parsed input file:\n{}", CharGrid(&maze, &maze_meta));

    let start_pos = MazePosition {
        address: maze_meta.start_address,
        heading: Cardinal::East,
    };

    let (path, cost): (Vec<MazePosition>, u32) = dijkstra(
        &start_pos,
        |pos| {
            pos.get_adjacent().into_iter().filter(|(candidate, _)| {
                maze.get_at(candidate.address)
                    .is_some_and(|tile| *tile == MazeTile::Open)
            })
        },
        |pos| pos.address == maze_meta.end_address,
    )
    .ok_or("Couldn't find path from start to end")?;

    let solved_grid = paint_solution(&maze, &path);
    info!("Solved:\n{}", CharGrid(&solved_grid, &maze_meta));
    info!("Part 1 cost: {}", cost);

    Ok(())
}

/// Parse the puzzle input, producing a grid representing the maze,
/// along with the start and end addresses, respectively.
fn parse_input(input_path: &Path) -> GenResult<(Grid<MazeTile>, MazeMetadata)> {
    let file = File::open(input_path)?;
    let reader = BufReader::new(file);
    let mut start_addr = None;
    let mut end_addr = None;
    let mut rows = Vec::new();
    for (y, line) in reader.lines().enumerate() {
        let row = line?
            .chars()
            .enumerate()
            .map(|(x, c)| match c {
                '#' => MazeTile::Wall,
                '.' => MazeTile::Open,
                'S' => {
                    start_addr = Some(GridAddress(x, y));
                    MazeTile::Open
                }
                'E' => {
                    end_addr = Some(GridAddress(x, y));
                    MazeTile::Open
                }
                other => panic!("Unexpected maze character: '{}'", other),
            })
            .collect();
        rows.push(row);
    }
    Ok((
        Grid { rows },
        MazeMetadata {
            start_address: start_addr.ok_or("Didn't find maze start")?,
            end_address: end_addr.ok_or("Didn't find maze end")?,
        },
    ))
}

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum MazeTile {
    Wall,
    Open,
}

#[derive(Debug, Copy, Clone)]
struct MazeMetadata {
    start_address: GridAddress,
    end_address: GridAddress,
}

impl<'a> RenderTileChar<MazeTile> for &'a MazeMetadata {
    fn render_tile_char(&self, tile: &MazeTile, x: usize, y: usize) -> ColoredString {
        let here = GridAddress(x, y);
        if here == self.start_address {
            "S".magenta()
        } else if here == self.end_address {
            "E".cyan()
        } else {
            match tile {
                MazeTile::Wall => "#".normal(),
                MazeTile::Open => ".".bright_black(),
            }
        }
    }
}

struct SolutionTile {
    maze_tile: MazeTile,
    traversed: CardinalSet,
}

impl From<MazeTile> for SolutionTile {
    fn from(value: MazeTile) -> Self {
        SolutionTile {
            maze_tile: value,
            traversed: CardinalSet::default(),
        }
    }
}

impl<'a> RenderTileChar<SolutionTile> for &'a MazeMetadata {
    fn render_tile_char(&self, tile: &SolutionTile, x: usize, y: usize) -> ColoredString {
        let here = GridAddress(x, y);
        if here == self.start_address {
            "S".magenta()
        } else if here == self.end_address {
            "E".cyan()
        } else {
            match tile.maze_tile {
                MazeTile::Wall =>
                    if tile.traversed.is_empty() {
                        "#".normal()
                    } else {
                        "!".red()
                    }
                MazeTile::Open => {
                    if tile.traversed.is_empty() {
                        " ".normal()
                    } else {
                        tile.traversed.to_box_drawing_char().to_string().yellow()
                    }
                }
            }
        }
    }
}

fn paint_solution(maze: &Grid<MazeTile>, path: &Vec<MazePosition>) -> Grid<SolutionTile> {
    let mut solution_grid: Grid<SolutionTile> = Grid {
        rows: maze
            .rows
            .iter()
            .map(|row| row.iter().map(|t| (*t).into()).collect())
            .collect::<Vec<_>>(),
    };

    let mut itr = path.iter();

    let mut current_addr = itr.next().unwrap().address;
    while let Some(pos) = itr.next() {
        if pos.address != current_addr {
            // solution moved between tiles; mark the heading as traversed from the current address,
            // and the reverse of that heading as traversed in the new address
            solution_grid[current_addr].traversed += pos.heading;
            solution_grid[pos.address].traversed += pos.heading.opposite();
            current_addr = pos.address;
        }
    }

    solution_grid
}

/// Each tile of the maze can be considered a cluster of four "nodes",
/// each residing at the same address in the grid, differentiated by
/// the "heading" that the maze runner is facing while at that address.
/// Advancing forward amounts to shifting to a new MazePosition at an
/// adjacent address, with the same heading. Rotating amounts to shifting
/// to a new MazePosition at the same address, but with a new heading.
/// The costs of advancing forward and rotating can be accounted for as
/// edge costs between `MazePosition` nodes in a graph.
#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
struct MazePosition {
    address: GridAddress,
    heading: Cardinal,
}

impl MazePosition {
    /// Return positions "adjacent" to the current position, along with the cost to reach
    /// each position from the current one (+1 for advancing forward, +1000 for rotating)
    fn get_adjacent(&self) -> Vec<(MazePosition, u32)> {
        let mut out = Vec::new();

        // advance forward
        if let Some(ahead) = self.address.checked_add(self.heading.into()) {
            out.push((
                MazePosition {
                    address: ahead,
                    heading: self.heading,
                },
                1,
            ));
        }

        // turn left
        out.push((
            MazePosition {
                address: self.address,
                heading: self.heading.turn_left(),
            },
            1000,
        ));

        // turn right
        out.push((
            MazePosition {
                address: self.address,
                heading: self.heading.turn_right(),
            },
            1000,
        ));

        out
    }
}

/// A node in the path to the maze's end, representing the cost
/// and previous position leading to some associated position.
struct SolutionState {
    cost: u32,
    prev: Option<MazePosition>,
}
