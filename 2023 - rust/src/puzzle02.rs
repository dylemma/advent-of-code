use std::fs::File;
use std::io::{BufRead, BufReader};
use std::iter::Sum;
use std::path::Path;

use crate::helper::GenResult;

pub fn run(input: &Path) -> GenResult<()> {
    let file = File::open(input)?;
    let lines = BufReader::new(file).lines();

    let sum = lines
        .map(|line| {
            let line = line?;
            println!("{}", line);
            let game = Game::try_from(line.as_str())?;
            let values = Values::from(&game);
            Ok(values)
        })
        .sum::<GenResult<Values>>()?;

    println!("\nResults: {:?}", sum);

    Ok(())
}

const PART_1_LIMIT: Restrictions = Restrictions {
    max_r: 12,
    max_g: 13,
    max_b: 14,
};

/// Represents two separate interpretations of a `Game`,
/// for parts 1 and 2 of the puzzle, respectively.
///
/// The `id_sum` will be the Game's `id`, or 0, depending on whether the game's draws
/// are possible according to the `PART_1_LIMIT` (how many cubes are in the bag).
///
/// The `power_sum` is the sum of the minimum `power` required for each game to be
/// possible. I.e. for each game, find the minimum number of cubes of each color that
/// can make the draws in that game possible, then multiply those numbers.
#[derive(Debug)]
struct Values {
    id_sum: u32,
    power_sum: u32,
}

impl From<&Game> for Values {
    fn from(game: &Game) -> Self {
        let id_sum =
            if game.draws.iter().all(|d| PART_1_LIMIT.test(*d)) { game.id } else { 0 };

        let power_sum = game
            .draws
            .iter()
            .fold(Draw::ZERO, |l, r| Draw::saturate(l, *r))
            .power();

        Values { id_sum, power_sum }
    }
}

impl Sum for Values {
    fn sum<I: Iterator<Item=Self>>(iter: I) -> Self {
        let mut id_sum = 0;
        let mut power_sum = 0;
        for v in iter {
            id_sum += v.id_sum;
            power_sum += v.power_sum;
        }
        Values { id_sum, power_sum }
    }
}

/// Represents one line of the puzzle input
#[derive(Debug)]
struct Game {
    pub id: u32,
    pub draws: Vec<Draw>,
}

/// Represents one draw from a bag of cubes.
/// Several of these occur per game (i.e. line of puzzle input)
#[derive(Debug, Copy, Clone)]
struct Draw {
    pub r: u32,
    pub g: u32,
    pub b: u32,
}

impl Draw {
    const ZERO: Draw = Draw { r: 0, g: 0, b: 0 };

    fn saturate(i: Draw, j: Draw) -> Self {
        Draw {
            r: i.r.max(j.r),
            g: i.g.max(j.g),
            b: i.b.max(j.b),
        }
    }

    fn power(&self) -> u32 {
        self.r * self.g * self.b
    }
}

/// Represents knowledge of the maximum number of cubes in the bag
struct Restrictions {
    pub max_r: u32,
    pub max_g: u32,
    pub max_b: u32,
}

impl Restrictions {
    fn test(&self, draw: Draw) -> bool {
        draw.r <= self.max_r &&
            draw.g <= self.max_g &&
            draw.b <= self.max_b
    }
}

/// Parses a "game", i.e. an ID and a series of "draws".
///
/// Format is expected to start with "Game <id>:", followed by a series of
/// semicolon-separated, encoded draws, where each draw specifies a number
/// of red, green, or blue cubes pulled from a bag.
impl TryFrom<&str> for Game {
    type Error = String;

    fn try_from(line: &str) -> Result<Self, Self::Error> {
        let (id_part, games_part) = line.split_once(':').ok_or("Couldn't find ':' in input")?;

        let id = (
            if id_part.starts_with("Game ") {
                id_part[5..].parse::<u32>().map_err(|e| e.to_string())
            } else {
                Err(format!("Couldn't parse game id from {}", id_part))
            }
        )?;

        let draws = games_part
            // split into "3 blue, 1 green, 2 red" chunks
            .split(';')
            .map(|s| s.trim())
            // for each chunk, capture the RGB numbers
            .map(Draw::try_from)
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Game { id, draws })
    }
}


/// Parses a "draw", i.e. one turn of a game.
///
/// Format is expected to be a comma-separated list of `<number> <color>` statements,
/// where `<color>` is expected to be "red", "green", or "blue". The total number for
/// each color is computed and returned. An error is returned if one of the `<number>`s
/// couldn't be parsed, or if an unexpected color was found.
impl TryFrom<&str> for Draw {
    type Error = String;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let mut r = 0u32;
        let mut g = 0u32;
        let mut b = 0u32;

        for slice in s.split(',').map(str::trim) {
            let (num_part, color_part) = slice
                .split_once(' ')
                .ok_or(format!("Couldn't interpret '{}' as a number + color pair", slice))?;

            let num = num_part.parse::<u32>().map_err(|e| e.to_string())?;

            (match color_part {
                "red" => Ok(r += num),
                "green" => Ok(g += num),
                "blue" => Ok(b += num),
                c => Err(format!("Unrecognized color: '{}'", c))
            })?;
        }

        Ok(Draw { r, g, b })
    }
}
