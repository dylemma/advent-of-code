use std::fmt::{Display, Formatter, Write};
use std::ops::{Add, AddAssign};
use colored::ColoredString;

/// Represents one of the four cardinal directions
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum Cardinal {
    North,
    East,
    South,
    West,
}

impl Cardinal {
    /// Returns the direction 90 degrees clockwise from this direction
    pub fn turn_right(&self) -> Cardinal {
        match self {
            Cardinal::North => Cardinal::East,
            Cardinal::East => Cardinal::South,
            Cardinal::South => Cardinal::West,
            Cardinal::West => Cardinal::North,
        }
    }

    /// Returns the direction 90 degrees counter-clockwise from this direction
    #[allow(unused)]
    pub fn turn_left(&self) -> Cardinal {
        match self {
            Cardinal::North => Cardinal::West,
            Cardinal::West => Cardinal::South,
            Cardinal::South => Cardinal::East,
            Cardinal::East => Cardinal::North,
        }
    }

    /// Returns the opposite direction
    pub fn opposite(&self) -> Cardinal {
        match self {
            Cardinal::North => Cardinal::South,
            Cardinal::East => Cardinal::West,
            Cardinal::South => Cardinal::North,
            Cardinal::West => Cardinal::East,
        }
    }
}

impl Into<(isize, isize)> for Cardinal {
    fn into(self) -> (isize, isize) {
        match self {
            Cardinal::North => (0, -1),
            Cardinal::East => (1, 0),
            Cardinal::South => (0, 1),
            Cardinal::West => (-1, 0),
        }
    }
}

impl Into<(i32, i32)> for Cardinal {
    fn into(self) -> (i32, i32) {
        match self {
            Cardinal::North => (0, -1),
            Cardinal::East => (1, 0),
            Cardinal::South => (0, 1),
            Cardinal::West => (-1, 0),
        }
    }
}

/// A set of cardinal directions
#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub struct CardinalSet {
    pub north: bool,
    pub east: bool,
    pub south: bool,
    pub west: bool,
}

impl CardinalSet {
    /// Returns a bit-wise representation of this set
    pub fn as_bits(&self) -> u8 {
        let mut bits = 0;
        if self.north {
            bits += 8;
        }
        if self.east {
            bits += 4;
        }
        if self.south {
            bits += 2;
        }
        if self.west {
            bits += 1;
        }
        bits
    }

    #[allow(unused)]
    pub fn is_empty(&self) -> bool {
        !(self.north || self.east || self.south || self.west)
    }

    /// Checks if a given cardinal direction is contained in this set
    #[allow(unused)]
    pub fn contains(&self, cardinal: Cardinal) -> bool {
        match cardinal {
            Cardinal::North => self.north,
            Cardinal::East => self.east,
            Cardinal::South => self.south,
            Cardinal::West => self.west,
        }
    }

    /// Renders this set of directions as a special character,
    /// where each direction is represented as a segment emitted
    /// in that direction from the center of the character block.
    /// Uses the "Box Drawing" block's "light" characters.
    /// https://en.wikipedia.org/wiki/Box-drawing_characters
    pub fn to_box_drawing_char(&self) -> char {
        match self.as_bits() {
            // all directions
            0b1111 => '┼',

            // all but one direction
            0b1110 => '├', // missing west
            0b1101 => '┴', // missing south
            0b1011 => '┤', // missing east
            0b0111 => '┬', // missing north

            // corners
            0b1100 => '└', // north+east
            0b0110 => '┌', // east+south
            0b0011 => '┐', // south+west
            0b1001 => '┘', // west+north

            // across
            0b1010 => '│', // north+south
            0b0101 => '─', // east+west

            // singles
            0b1000 => '╵', // north only
            0b0100 => '╶', // east only
            0b0010 => '╷', // south only
            0b0001 => '╴', // west only

            0b0000 => ' ', // none
            _ => '?',      // unexpected
        }
    }
}

impl Add<Cardinal> for CardinalSet {
    type Output = CardinalSet;

    fn add(self, rhs: Cardinal) -> Self::Output {
        let mut out = self.clone();
        match rhs {
            Cardinal::North => out.north = true,
            Cardinal::East => out.east = true,
            Cardinal::South => out.south = true,
            Cardinal::West => out.west = true,
        }
        out
    }
}

impl AddAssign<Cardinal> for CardinalSet {
    fn add_assign(&mut self, rhs: Cardinal) {
        match rhs {
            Cardinal::North => self.north = true,
            Cardinal::East => self.east = true,
            Cardinal::South => self.south = true,
            Cardinal::West => self.west = true,
        }
    }
}

impl Default for CardinalSet {
    fn default() -> Self {
        CardinalSet {
            north: false,
            east: false,
            south: false,
            west: false,
        }
    }
}

/// Simple cartesian grid where each cell is represented as a `Tile`
#[derive(Clone, Debug)]
pub struct Grid<Tile> {
    pub rows: Vec<Vec<Tile>>,
}

impl<Tile: Default + Clone> Grid<Tile> {
    #[allow(unused)]
    pub fn new_default(width: usize, height: usize) -> Grid<Tile> {
        let mut rows = Vec::with_capacity(height);
        for _ in 0..height {
            rows.push(vec![Tile::default(); width]);
        }
        Grid { rows }
    }
}

impl<Tile> Grid<Tile> {
    pub fn get(&self, x: usize, y: usize) -> Option<&Tile> {
        let row = self.rows.get(y)?;
        row.get(x)
    }

    pub fn get_mut(&mut self, x: usize, y: usize) -> Option<&mut Tile> {
        let row = self.rows.get_mut(y)?;
        row.get_mut(x)
    }

    pub fn width(&self) -> usize {
        match self.rows.get(0) {
            Some(row) => row.len(),
            None => 0,
        }
    }

    pub fn height(&self) -> usize {
        self.rows.len()
    }
}

pub trait RenderTileChar<Tile> {
    fn render_tile_char(&self, tile: &Tile, x: usize, y: usize) -> ColoredString;
}

/// Wrapper struct used for displaying a Grid<Tile>
/// based on some style defined by a custom `style: S`.
pub struct CharGrid<'g, Tile, Style>(pub &'g Grid<Tile>, pub Style);

impl <'g, Tile, C> Display for CharGrid<'g, Tile, C>
where C: RenderTileChar<Tile> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (y, row) in self.0.rows.iter().enumerate() {
            for (x, tile) in row.iter().enumerate() {
                write!(f, "{}", self.1.render_tile_char(tile, x, y))?;
            }
            f.write_char('\n')?;
        }
        Ok(())
    }
}

