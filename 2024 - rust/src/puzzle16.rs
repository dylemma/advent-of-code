use crate::geometry::{Cardinal, CardinalSet, CharGrid, Grid, GridAddress, RenderTileChar};
use crate::helper::GenResult;
use colored::{ColoredString, Colorize};
use log::{debug, info};
use std::cmp::{Ordering, Reverse};
use std::collections::{BinaryHeap, HashMap, HashSet, VecDeque};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub fn run(input_path: &Path) -> GenResult<()> {
    let (maze, maze_meta) = parse_input(input_path)?;

    debug!("Parsed input file:\n{}", CharGrid(&maze, &maze_meta));

    let path_data = explore(&maze, &maze_meta);
    let grid2 = paint_solution(&maze, &path_data, &maze_meta);
    let cost = solution_cost(&path_data, &maze_meta);
    let num_spots = spectator_spots(&grid2);
    info!("Solved maze:\n{}", CharGrid(&grid2, &maze_meta));
    info!("Solution cost: {}", cost);
    info!("Spectator seats: {}", num_spots);

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

/// Glorified boolean, representing a tile in the maze
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
enum MazeTile {
    Wall,
    Open,
}

/// Helper struct to remember the start and end points
#[derive(Debug, Copy, Clone)]
struct MazeMetadata {
    start_address: GridAddress,
    end_address: GridAddress,
}

/// Helper for colored debug output of the maze as given
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

/// Struct used for our modified Dijkstra's algorithm, to be placed in the priority queue.
/// Represents a new position to be explored, and the cost to reach that position.
struct PathCandidate {
    pos: MazePosition,
    cost: u32,
}

impl Eq for PathCandidate {}

impl PartialEq<Self> for PathCandidate {
    fn eq(&self, other: &Self) -> bool {
        other.cost == self.cost
    }
}

impl PartialOrd<Self> for PathCandidate {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for PathCandidate {
    fn cmp(&self, other: &Self) -> Ordering {
        Reverse(self.cost).cmp(&Reverse(other.cost))
    }
}

/// Result structure for our modified Dijkstra's algorithm.
/// Instead of remembering a single "parent" position, it remembers all parents that
/// can reach the associated position with the same minimal `cost`.
#[derive(Debug)]
struct PathData {
    cost: u32,
    parents: Vec<MazePosition>,
}

/// Modified Dijkstra's algorithm which computes the cheapest `cost` to reach each position in the maze,
/// and the `parents` for each position that provided that minimal cost.
fn explore(maze: &Grid<MazeTile>, maze_metadata: &MazeMetadata) -> HashMap<MazePosition, PathData> {
    let start_pos = MazePosition {
        address: maze_metadata.start_address,
        heading: Cardinal::East,
    };

    // create a queue of positions that have yet to be explored
    let mut to_search = BinaryHeap::new();

    // track the best cost+parent for each explored position
    let mut path_data = HashMap::new();

    // track which positions we already visited
    let mut visited: HashSet<MazePosition> = HashSet::new();

    // seed the exploration queue and path data with the start node with 0 cost and no parent
    to_search.push(PathCandidate {
        pos: start_pos,
        cost: 0,
    });
    path_data.insert(
        start_pos,
        PathData {
            cost: 0,
            parents: vec![/*no parent!*/],
        },
    );

    // Exploration loop
    while let Some(PathCandidate { pos, cost }) = to_search.pop() {
        // skip already-visited nodes, since we should already have found their minimum cost
        if !visited.insert(pos) {
            continue;
        }

        // for each of the current position's neighbors, adjust the known cost of that neighbor
        // and update its "parents" if a lower cost was found. For the "part 2" twist, the parents
        // will be a Vec of any parent that results in the same minimum cost.
        for (neighbor, edge_cost) in pos.get_adjacent() {
            if let Some(neighbor_tile) = maze.get_at(neighbor.address) {
                if *neighbor_tile == MazeTile::Open {
                    let neighbor_cost = cost + edge_cost;

                    // update the path data for the neighbor
                    let datum = path_data.entry(neighbor).or_insert(PathData {
                        cost: neighbor_cost,
                        parents: Vec::with_capacity(1),
                    });

                    // if a cheaper cost was found, update the node's cost and replace its "parents"
                    if neighbor_cost < datum.cost {
                        datum.cost = neighbor_cost;
                        datum.parents.clear();
                    }

                    // if reaching this neighbor from the current `pos` decreased or matched its cost,
                    // we'll mark `pos` as a parent of this neighbor
                    if neighbor_cost <= datum.cost {
                        datum.parents.push(pos);
                    }

                    // add the neighbor to the exploration queue
                    if !visited.contains(&neighbor) {
                        to_search.push(PathCandidate {
                            pos: neighbor,
                            cost: neighbor_cost,
                        })
                    }
                }
            }
        }
    }

    path_data
}

/// Extract the minimal cost to reach the maze's `end_address`, according to the path data returned by `explore`
fn solution_cost(path_data: &HashMap<MazePosition, PathData>, maze_meta: &MazeMetadata) -> u32 {
    Cardinal::ALL
        .iter()
        .map(|heading| MazePosition {
            address: maze_meta.end_address,
            heading: *heading,
        })
        .flat_map(|pos| path_data.get(&pos).map(|pd| (pos, pd)))
        .min_by_key(|(_, pd)| pd.cost)
        .map_or(u32::MAX, |(_, pd)| pd.cost)
}

/// Count the number of tiles that exist along any of the optimal paths through the maze.
/// Assumes `solution_grid` was returned by `paint_solution`
fn spectator_spots(solution_grid: &Grid<SolutionTile>) -> usize {
    let mut count = 0;
    for row in &solution_grid.rows {
        for tile in row {
            if !tile.traversed.is_empty() || !tile.alt_traversed.is_empty() {
                count += 1;
            }
        }
    }
    count
}

/// Combines the original maze and the resulting path-finding information from `explore`
/// to create a new `Grid` which renders the solution.
fn paint_solution(
    maze: &Grid<MazeTile>,
    path_data: &HashMap<MazePosition, PathData>,
    maze_meta: &MazeMetadata,
) -> Grid<SolutionTile> {
    let mut solution_grid: Grid<SolutionTile> = Grid {
        rows: maze
            .rows
            .iter()
            .map(|row| row.iter().map(|t| (*t).into()).collect())
            .collect::<Vec<_>>(),
    };

    // The goal address may have been reached via any of the four cardinal directions
    let reached_goal_positions: Vec<(MazePosition, &PathData)> = Cardinal::ALL
        .iter()
        .map(|heading| MazePosition {
            address: maze_meta.end_address,
            heading: *heading,
        })
        .flat_map(|pos| path_data.get(&pos).map(|pd| (pos, pd)))
        .collect::<Vec<_>>();

    // ...but not all incoming headings will have the same cost; filter down to only those with the lowest cost
    let cheapest_goal_positions: Vec<(MazePosition, &PathData)> = {
        if let Some((_, cheapest)) = reached_goal_positions.iter().min_by_key(|(_, pd)| pd.cost) {
            reached_goal_positions
                .iter()
                .filter(|(_, pd)| pd.cost == cheapest.cost)
                .cloned()
                .collect()
        } else {
            vec![]
        }
    };

    // Walk backwards from the end of the path, following `parents` links via only the first listed parent.
    // This will paint the "primary" path.
    {
        // for the "primary" path, arbitrarily pick the first of the "cheapest" goal positions
        let mut cursor_opt = cheapest_goal_positions.get(0).cloned();

        while let Some((cursor, datum)) = cursor_opt {
            // pick the first "parent" listed for the current `cursor` as the "primary" path
            if let Some(parent) = datum.parents.get(0) {
                // When the path crosses between multiple addresses, we want to paint a line.
                // We do so by painting an outgoing line from the parent in the listed heading,
                // and simulating an incoming line to the cursor by painting the opposite heading.
                if parent.address != cursor.address {
                    solution_grid[parent.address].traversed += cursor.heading;
                    solution_grid[cursor.address].traversed += cursor.heading.opposite();
                }

                // Advance the cursor by moving it to the parent
                cursor_opt = path_data.get(parent).map(|pd| (*parent, pd));
            } else {
                // if no parent was found, we have presumably reached the maze's `start_address`
                cursor_opt = None
            }
        }
    }

    // Redo the same walk, but follow *all* of the parent links from *all* of the cheapest goal positions
    {
        let mut visited = HashSet::new();
        let mut to_visit = VecDeque::from(cheapest_goal_positions);

        while let Some((cursor, datum)) = to_visit.pop_front() {
            // avoid re-visiting positions
            if !visited.insert(cursor) {
                continue;
            }

            for parent_pos in &datum.parents {
                // draw a line, similar to with the primary path, but with the `alt_traversed` field
                if parent_pos.address != cursor.address {
                    solution_grid[parent_pos.address].alt_traversed += cursor.heading;
                    solution_grid[cursor.address].alt_traversed += cursor.heading.opposite();
                }

                // add the parent to the exploration queue
                if let Some(parent_path_data) = path_data.get(&parent_pos) {
                    to_visit.push_back((*parent_pos, parent_path_data));
                }
            }
        }
    }

    solution_grid
}

/// A tile in the "painted" solution.
/// `maze_tile` is the tile from the original maze.
/// `traversed` is the directions that the tile was visited along the primary path through the maze
/// `alt_traversed` is the directions that the tile was visited along *any* optimal path through the maze
struct SolutionTile {
    maze_tile: MazeTile,
    traversed: CardinalSet,
    alt_traversed: CardinalSet,
}

impl From<MazeTile> for SolutionTile {
    fn from(value: MazeTile) -> Self {
        SolutionTile {
            maze_tile: value,
            traversed: CardinalSet::default(),
            alt_traversed: CardinalSet::default(),
        }
    }
}

/// Pretty colored output for the solution
impl<'a> RenderTileChar<SolutionTile> for &'a MazeMetadata {
    fn render_tile_char(&self, tile: &SolutionTile, x: usize, y: usize) -> ColoredString {
        let here = GridAddress(x, y);
        if here == self.start_address {
            "S".magenta()
        } else if here == self.end_address {
            "E".cyan()
        } else {
            match tile.maze_tile {
                MazeTile::Wall => {
                    if tile.traversed.is_empty() && tile.alt_traversed.is_empty() {
                        "#".normal()
                    } else {
                        "!".red()
                    }
                }
                MazeTile::Open => {
                    if !tile.traversed.is_empty() {
                        tile.traversed.to_box_drawing_char().to_string().yellow()
                    } else if !tile.alt_traversed.is_empty() {
                        tile.alt_traversed.to_box_drawing_char().to_string().cyan()
                    } else {
                        " ".normal()
                    }
                }
            }
        }
    }
}
