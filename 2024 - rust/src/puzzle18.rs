use crate::geometry::{Cardinal, CardinalSet, GridAddress};
use crate::helper::GenResult;
use colored::Colorize;
use log::{debug, info, warn};
use pathfinding::directed::astar::astar;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter, Write};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub fn run(input_path: &Path, is_example: bool) -> GenResult<()> {
    let byte_addresses = {
        let file = File::open(input_path)?;
        let reader = BufReader::new(file);
        let mut addresses = Vec::new();
        for line in reader.lines() {
            let line = line?;
            let (l, r) = line.split_once(',').ok_or("missing comma in input line")?;
            let addr = GridAddress(l.parse()?, r.parse()?);
            addresses.push(addr);
        }
        addresses
    };

    let grid_size = if is_example { 7 } else { 71 };
    let part1_length = if is_example { 12 } else { 1024 };
    let start = GridAddress(0, 0);
    let goal = GridAddress(grid_size - 1, grid_size - 1);

    let (first_batch, second_batch) = byte_addresses.split_at(part1_length);

    let mut fallen_bytes = first_batch.into_iter().cloned().collect::<HashSet<_>>();

    // Part 1
    let (p1_path, p1_cost) = find_path(&GridAddress(0, 0), &goal, &fallen_bytes, grid_size)
        .ok_or("couldn't find path")?;

    info!(
        "After {} bytes fallen, best path is {} steps:\n{}",
        part1_length.to_string().green(),
        p1_cost,
        PathGrid {
            fallen_bytes: &fallen_bytes,
            path_tiles: &render_path(&p1_path),
            size: grid_size
        }
    );

    let mut part2_positions_itr = second_batch.into_iter().enumerate();
    let mut current_path = Some((render_path(&p1_path), p1_cost));

    // A binary search for the position of the final blocker byte sounds tempting,
    // but would involve an O(N) re-building the set of obstacles for each test. Instead,
    // We'll advance linearly through the list of remaining obstacles in order, adding them
    // to the mutable obstacles set as we go, and only perform a path-finding test when
    // an obstacle is added directly in the way of the current path.
    while let Some((i, next_obstacle)) = part2_positions_itr.next() {
        if let Some((path, cost)) = &current_path {
            fallen_bytes.insert(*next_obstacle);

            // check if the new obstacle blocks the previously-computed path
            if path.contains_key(next_obstacle) {
                // search for a new path
                if let Some((new_path, new_cost)) =
                    find_path(&start, &goal, &fallen_bytes, grid_size)
                {
                    debug!(
                        "recomputed path due to new obstacle at (index: {}, {:?})",
                        i,
                        format!("{:?}", next_obstacle).yellow()
                    );
                    current_path = Some((render_path(&new_path), new_cost));
                } else {
                    // the final blocker was found!

                    info!(
                        "Last possible path has {} steps:\n{}",
                        cost,
                        PathGrid {
                            fallen_bytes: &fallen_bytes,
                            path_tiles: path,
                            size: grid_size,
                        }
                    );

                    info!(
                        "Final blocker found at {}",
                        format!("{},{}", next_obstacle.0, next_obstacle.1).bright_blue()
                    );
                    break;
                }
            }
        } else {
            warn!("path no longer available");
            break;
        }
    }

    Ok(())
}

fn find_path(
    start: &GridAddress,
    goal: &GridAddress,
    obstacles: &HashSet<GridAddress>,
    grid_size: usize,
) -> Option<(Vec<GridAddress>, usize)> {
    astar(
        start,
        |&here| {
            Cardinal::ALL
                .iter()
                .flat_map(move |&c| here.checked_add(c.into()))
                .filter(|neighbor| neighbor.0 < grid_size && neighbor.1 < grid_size)
                .filter(|addr| !obstacles.contains(addr))
                .map(|neighbor| (neighbor, 1))
        },
        |here| {
            // manhattan distance from `here` to `goal`
            (goal.0 - here.0) + (goal.1 - here.1)
        },
        |here| *here == *goal,
    )
}

struct PathGrid<'b, 'p> {
    fallen_bytes: &'b HashSet<GridAddress>,
    path_tiles: &'p HashMap<GridAddress, CardinalSet>,
    size: usize,
}

impl Display for PathGrid<'_, '_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for y in 0..self.size {
            if y > 0 {
                write!(f, "\n")?;
            }
            for x in 0..self.size {
                let here = GridAddress(x, y);
                if let Some(path_cards) = self.path_tiles.get(&here) {
                    write!(
                        f,
                        "{}",
                        path_cards.to_box_drawing_char().to_string().yellow()
                    )?;
                } else if self.fallen_bytes.contains(&here) {
                    f.write_char('#')?;
                } else {
                    write!(f, "{}", ".".bright_black())?;
                }
            }
        }
        Ok(())
    }
}

fn render_path(path: &[GridAddress]) -> HashMap<GridAddress, CardinalSet> {
    let mut out = HashMap::<GridAddress, CardinalSet>::new();
    for i in 1..path.len() {
        let prev = &path[i - 1];
        let next = &path[i];
        if let Some(dir) = determine_cardinal(prev, next) {
            *out.entry(*prev).or_default() += dir;
            *out.entry(*next).or_default() += dir.opposite();
        }
    }
    out
}

fn determine_cardinal(from: &GridAddress, to: &GridAddress) -> Option<Cardinal> {
    if from.0 > to.0 {
        Some(Cardinal::West)
    } else if from.0 < to.0 {
        Some(Cardinal::East)
    } else if from.1 > to.1 {
        Some(Cardinal::North)
    } else if from.1 < to.1 {
        Some(Cardinal::South)
    } else {
        None
    }
}
