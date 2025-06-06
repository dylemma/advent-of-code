use crate::geometry::{Cardinal, GridAddress};
use crate::helper::GenResult;
use colored::Colorize;
use log::{debug, info};
use pathfinding::prelude::{astar, astar_bag, astar_bag_collect};
use std::collections::HashMap;
use std::fmt::{Display, Formatter, Write};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;
use std::rc::Rc;
use std::vec::IntoIter;

pub fn run(input_path: &Path) -> GenResult<()> {
    let input_sequences = {
        let file = File::open(input_path)?;
        let reader = BufReader::new(file);
        reader.lines().collect::<Result<Vec<_>, _>>()?
    };

    let arrow_pad = Keypad::new_directional();
    let numpad = Keypad::new_numeric();

    for sequence in input_sequences {
        info!("Goal: {}", sequence);

        // moves that must be input by the first keypad robot in order to get the numpad robot to input the `sequence`
        let bot1_moves = solve_moves(sequence.chars(), &numpad, numpad.inv_map[&'A']).ok_or(
            format!("Couldn't determine moves for {} in num pad", sequence),
        )?;
        let bot1_keys = bot1_moves.to_string();
        debug!("  Sequence: {}", bot1_keys);

        // moves that must be input by the second keypad robot in order to get the first keypad robot to input its required sequence
        let bot2_moves = solve_moves(bot1_keys.chars(), &arrow_pad, arrow_pad.inv_map[&'A'])
            .ok_or(format!(
                "Couldn't determine moves for {} in arrow pad",
                bot1_keys
            ))?;
        let bot2_keys = bot2_moves.to_string();
        debug!("  Sequence 2: {}", bot2_keys);

        // moves that must be input by the protagonist in order for the second keypad robot to input its required sequence
        let solution_moves = solve_moves(bot2_keys.chars(), &arrow_pad, arrow_pad.inv_map[&'A'])
            .ok_or(format!(
                "Couldn't determine moves for {} in second arrow pad",
                bot2_keys
            ))?;
        let solution_keys = solution_moves.to_string();
        info!("  Sequence 3: {}", solution_keys);

        let num = sequence
            .chars()
            .filter(|c| c.is_numeric())
            .collect::<String>()
            .parse::<usize>()?;
        let score = num * solution_moves.0.len();
        info!(
            "  Score: {} * {} = {}",
            num,
            solution_moves.0.len(),
            score.to_string().green()
        );
    }

    Ok(())
}

fn solve_moves<G: IntoIterator<Item = char>>(
    goal: G,
    keypad: &Keypad,
    start: GridAddress,
) -> Option<RoboMoves> {
    let mut pos = start;
    let mut moves = Vec::new();
    for key in goal.into_iter() {
        let key_pos = keypad.inv_map.get(&key)?;
        let moves_to_key = keypad.get_moves(&pos, key_pos)?;
        for c in moves_to_key {
            moves.push(RoboMove::Shift(c));
        }
        moves.push(RoboMove::Press);
        pos = *key_pos;
    }
    Some(RoboMoves(moves))
}

#[derive(Debug, Copy, Clone)]
enum RoboMove {
    Shift(Cardinal),
    Press,
}

#[derive(Debug, Clone)]
struct RoboMoves(Vec<RoboMove>);

impl Display for RoboMoves {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for m in &self.0 {
            f.write_char(m.keypad_char())?;
        }
        Ok(())
    }
}

impl RoboMove {
    fn keypad_char(&self) -> char {
        match self {
            Self::Press => 'A',
            Self::Shift(Cardinal::North) => '^',
            Self::Shift(Cardinal::East) => '>',
            Self::Shift(Cardinal::South) => 'v',
            Self::Shift(Cardinal::West) => '<',
        }
    }
}

struct Keypad {
    map: HashMap<GridAddress, char>,
    inv_map: HashMap<char, GridAddress>,
}

impl FromIterator<(GridAddress, char)> for Keypad {
    fn from_iter<I: IntoIterator<Item = (GridAddress, char)>>(iter: I) -> Self {
        let mut map = HashMap::new();
        let mut inv_map = HashMap::new();
        for (addr, c) in iter.into_iter() {
            map.insert(addr, c);
            inv_map.insert(c, addr);
        }
        Self { map, inv_map }
    }
}

impl Keypad {
    /// ```
    /// +---+---+---+
    /// | 7 | 8 | 9 |
    /// +---+---+---+
    /// | 4 | 5 | 6 |
    /// +---+---+---+
    /// | 1 | 2 | 3 |
    /// +---+---+---+
    ///     | 0 | A |
    ///     +---+---+
    /// ```
    fn new_numeric() -> Self {
        Self::from_iter(vec![
            // first column
            (GridAddress(0, 0), '7'),
            (GridAddress(0, 1), '4'),
            (GridAddress(0, 2), '1'),
            // second column
            (GridAddress(1, 0), '8'),
            (GridAddress(1, 1), '5'),
            (GridAddress(1, 2), '2'),
            (GridAddress(1, 3), '0'),
            // third column
            (GridAddress(2, 0), '9'),
            (GridAddress(2, 1), '6'),
            (GridAddress(2, 2), '3'),
            (GridAddress(2, 3), 'A'),
        ])
    }

    /// ```
    ///     +---+---+
    ///     | ^ | A |
    /// +---+---+---+
    /// | < | v | > |
    /// +---+---+---+
    /// ```
    fn new_directional() -> Self {
        Self::from_iter(vec![
            // top row
            (GridAddress(1, 0), '^'),
            (GridAddress(2, 0), 'A'),
            // bottom row
            (GridAddress(0, 1), '<'),
            (GridAddress(1, 1), 'v'),
            (GridAddress(2, 1), '>'),
        ])
    }

    fn get_all_moves(&self, from: &GridAddress, to: &GridAddress) -> Vec<Vec<Cardinal>> {
        if let Some((solution, _)) = astar_bag(
            from,
            |here| {
                self.get_adjacent(here)
                    .iter()
                    .map(|there| (*there, 1))
                    .collect::<Vec<_>>()
            },
            |here| here.0.abs_diff(to.0) + here.1.abs_diff(to.1),
            |&here| here == *to,
        ) {
            let mut out = Vec::new();

            for path in solution.into_iter() {
                let mut steps = Vec::new();
                for i in 1..path.len() {
                    let GridAddress(x1, y1) = path[i - 1];
                    let GridAddress(x2, y2) = path[i];
                    steps.push(if x1 < x2 {
                        Cardinal::East
                    } else if x1 > x2 {
                        Cardinal::West
                    } else if y1 < y2 {
                        Cardinal::South
                    } else if y1 > y2 {
                        Cardinal::North
                    } else {
                        panic!("0-distance step in path? {},{} -> {},{}", x1, y1, x2, y2);
                    });
                }
                out.push(steps);
            }

            out
        } else {
            vec![]
        }
    }

    fn get_moves(&self, from: &GridAddress, to: &GridAddress) -> Option<Vec<Cardinal>> {
        // let (path, _) = astar(
        //     from,
        //     |here| {
        //         self.get_adjacent(here)
        //             .iter()
        //             .map(|there| (*there, 1))
        //             .collect::<Vec<_>>()
        //     },
        //     |here| here.0.abs_diff(to.0) + here.1.abs_diff(to.1),
        //     |&here| here == *to,
        // )?;

        let (path, _) = astar(
            &RoboPos::new(*from),
            |here| here.successors_in(self),
            |here| here.addr.manhatten_distance_to(to),
            |here| here.addr == *to,
        )?;

        let mut out = Vec::new();
        for i in 1..path.len() {
            let GridAddress(x1, y1) = path[i - 1].addr;
            let GridAddress(x2, y2) = path[i].addr;
            out.push(if x1 < x2 {
                Cardinal::East
            } else if x1 > x2 {
                Cardinal::West
            } else if y1 < y2 {
                Cardinal::South
            } else if y1 > y2 {
                Cardinal::North
            } else {
                panic!("0-distance step in path? {},{} -> {},{}", x1, y1, x2, y2);
            });
        }
        Some(out)
    }

    fn get_adjacent(&self, here: &GridAddress) -> Vec<GridAddress> {
        if self.map.contains_key(here) {
            Cardinal::ALL
                .into_iter()
                .flat_map(|dir| here.checked_add(dir.into()))
                .filter(|there| self.map.contains_key(there))
                .collect()
        } else {
            vec![]
        }
    }
}

struct Robot {
    pos: GridAddress,
    keypad: Rc<Keypad>,
    parent: Option<Box<Robot>>,
}

fn example() {
    let numpad = Rc::new(Keypad::new_numeric());
    let arrows = Rc::new(Keypad::new_directional());

    let outer_arrows_bot = Box::new(Robot {
        pos: arrows.inv_map[&'A'],
        keypad: arrows.clone(),
        parent: None,
    });

    let inner_arrows_bot = Box::new(Robot {
        pos: arrows.inv_map[&'A'],
        keypad: arrows,
        parent: Some(outer_arrows_bot),
    });

    let numpad_bot = Robot {
        pos: numpad.inv_map[&'A'],
        keypad: numpad,
        parent: Some(inner_arrows_bot),
    };
}

impl Robot {
    fn root(&self) -> &Robot {
        match &self.parent {
            Some(parent) => parent.root(),
            None => self
        }
    }

    fn path_to(&self, goal: &GridAddress) {
        
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
struct RoboPos {
    addr: GridAddress,
    last_dir: Option<Cardinal>,
}

impl RoboPos {
    fn new(addr: GridAddress) -> Self {
        Self {
            addr,
            last_dir: None,
        }
    }

    fn successors_in(&self, keypad: &Keypad) -> Vec<(RoboPos, usize)> {
        let mut out = Vec::new();
        if keypad.map.contains_key(&self.addr) {
            for dir in Cardinal::ALL {
                if let Some(neighbor) = self.addr.checked_add(dir.into()) {
                    if keypad.map.contains_key(&neighbor) {
                        let cost = match self.last_dir {
                            Some(d) if d == dir => 1,
                            None => 1,
                            Some(_) => 2, // penalize changing direction
                        };
                        out.push((
                            RoboPos {
                                addr: neighbor,
                                last_dir: Some(dir),
                            },
                            cost,
                        ));
                    }
                }
            }
        }
        out
    }
}
