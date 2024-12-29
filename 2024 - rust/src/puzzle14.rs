use crate::geometry::{CharGrid, Grid, GridAddress, GridDelta, RenderTileChar};
use crate::helper::{GenError, GenResult};
use colored::{ColoredString, Colorize};
use log::{debug, info};
use regex::Regex;
use std::fmt::{Display, Formatter};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::str::FromStr;

pub fn run(input_path: &Path, is_example: bool) -> GenResult<()> {
    // Parse input
    let init_robots = {
        let mut out = Vec::new();
        let file = File::open(input_path)?;
        let reader = BufReader::new(file);
        for line in reader.lines() {
            let line = line?.parse::<Robot>()?;
            out.push(line);
        }
        out
    };

    // Map size is bigger for the real puzzle, but is not part of the input itself
    let map_size = if is_example {
        Quadrants {
            width: 11,
            height: 7,
        }
    } else {
        Quadrants {
            width: 101,
            height: 103,
        }
    };

    // Report initial state
    let initial_grid = robot_count_grid(&init_robots, map_size.width, map_size.height);
    info!("{}", CharGrid(&initial_grid, map_size));

    // Compute state after 100 seconds
    {
        let mut robots = init_robots.clone();
        for robot in &mut robots {
            robot.step(100);
        }

        let after_100_seconds = robot_count_grid(&robots, map_size.width, map_size.height);
        info!(
            "After 100 seconds:\n{}",
            CharGrid(&after_100_seconds, map_size)
        );
        let safety_100 = safety_factor(&robots, map_size);
        info!("Safety factor after 100 seconds: {}", safety_100);
    }

    // Step 1 second at a time, looking for a Christmas tree picture
    {
        let mut robots = init_robots.clone();
        let mut seconds_elapsed = 0;
        while seconds_elapsed < 10000 {
            let grid = robot_count_grid(&robots, map_size.width, map_size.height);
            let score = maybe_xmas(&robots, &grid);
            if score > 1000 {
                info!(
                    "Possible Xmas tree at {} seconds with score {}:\n{}",
                    seconds_elapsed,
                    score,
                    CharGrid(&grid, map_size)
                );
                break;
            }
            for robot in &mut robots {
                robot.step(1);
            }
            seconds_elapsed += 1;
        }
    }

    Ok(())
}

#[derive(Copy, Clone)]
struct Robot {
    pos: (i64, i64),
    velocity: (i64, i64),
}

impl Robot {
    fn step(&mut self, num_seconds: u32) {
        self.pos.0 += self.velocity.0 * num_seconds as i64;
        self.pos.1 += self.velocity.1 * num_seconds as i64;
    }

    fn wrapped_pos(&self, width: usize, height: usize) -> (usize, usize) {
        let (x, y) = self.pos;
        let mut wx = x % (width as i64);
        let mut wy = y % (height as i64);

        // negative modulo positive is still negative, so we
        // need to subtract its magnitude from the corresponding maximum
        if wx < 0 {
            wx = (width as i64) + wx;
        }
        if wy < 0 {
            wy = (height as i64) + wy;
        }

        (wx as usize, wy as usize)
    }
}

impl FromStr for Robot {
    type Err = GenError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let pattern = Regex::new(r#"^p=(\d+),(\d+) v=(-?\d+),(-?\d+)$"#)?;
        let (_, [px, py, vx, vy]) = pattern
            .captures(s)
            .ok_or(format!("unmatched input: {}", s))?
            .extract();
        Ok(Robot {
            pos: (px.parse()?, py.parse()?),
            velocity: (vx.parse()?, vy.parse()?),
        })
    }
}

impl Display for Robot {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "p={},{} v={},{}",
            self.pos.0, self.pos.1, self.velocity.0, self.velocity.1
        )
    }
}

fn robot_count_grid(robots: &Vec<Robot>, width: usize, height: usize) -> Grid<usize> {
    let mut grid = Grid::new_default(width, height);
    for robot in robots {
        let (x, y) = robot.wrapped_pos(width, height);
        if let Some(tile) = grid.get_mut(x, y) {
            *tile += 1;
        }
    }
    grid
}

fn safety_factor(robots: &Vec<Robot>, map_size: Quadrants) -> usize {
    // NW, NE, SE, SW (clockwise from northwest)
    let mut quadrants = [0; 4];

    let mid_x = map_size.width / 2;
    let mid_y = map_size.height / 2;

    for robot in robots {
        let (x, y) = robot.wrapped_pos(map_size.width, map_size.height);

        if x < mid_x && y < mid_y {
            quadrants[0] += 1; // Northwest quadrant
        } else if x > mid_x && y < mid_y {
            quadrants[1] += 1; // Northeast quadrant
        } else if x > mid_x && y > mid_y {
            quadrants[2] += 1; // Southwest quadrant
        } else if x < mid_x && y > mid_y {
            quadrants[3] += 1; // Southeast quadrant
        }
    }

    debug!(
        "Counts by quadrant: {{ NW: {}, NE: {}, SE: {}, SW: {} }}",
        quadrants[0], quadrants[1], quadrants[2], quadrants[3]
    );

    quadrants.iter().product()
}

#[derive(Copy, Clone)]
struct Quadrants {
    width: usize,
    height: usize,
}

impl RenderTileChar<usize> for Quadrants {
    fn render_tile_char(&self, tile: &usize, x: usize, y: usize) -> ColoredString {
        if x == self.width / 2 || y == self.height / 2 {
            " ".normal()
        } else if *tile == 0 {
            ".".bright_black()
        } else {
            tile.to_string().green()
        }
    }
}

fn maybe_xmas(robots: &Vec<Robot>, count_grid: &Grid<usize>) -> usize {
    let mut adjacency_score = 0;

    for robot in robots {
        let (x, y) = robot.wrapped_pos(count_grid.width(), count_grid.height());
        let addr = GridAddress(x, y);

        // check if every tile adjacent to this robot is occupied by at least one other robot
        for delta in GridDelta::CARDINALS_AND_DIAGONALS {
            if let Some(adjacent_addr) = addr.checked_add(delta) {
                if let Some(num_adjacent) = count_grid.get_at(adjacent_addr) {
                    if *num_adjacent > 0 {
                        adjacency_score += 1;
                    }
                }
            }
        }
    }

    adjacency_score
}
