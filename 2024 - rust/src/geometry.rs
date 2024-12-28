use colored::ColoredString;
use std::fmt::{Display, Formatter, Write};
use std::ops::{Add, AddAssign, Index, IndexMut};

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

    pub const ALL: [Cardinal; 4] = [
        Cardinal::North,
        Cardinal::East,
        Cardinal::South,
        Cardinal::West,
    ];
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

impl Into<GridDelta> for Cardinal {
    fn into(self) -> GridDelta {
        let (dx, dy) = self.into();
        GridDelta(dx, dy)
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

    pub fn len(&self) -> usize {
        self.north as usize + self.east as usize + self.south as usize + self.west as usize
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

impl Index<Cardinal> for CardinalSet {
    type Output = bool;

    fn index(&self, index: Cardinal) -> &Self::Output {
        match index {
            Cardinal::North => &self.north,
            Cardinal::East => &self.east,
            Cardinal::South => &self.south,
            Cardinal::West => &self.west,
        }
    }
}

impl IndexMut<Cardinal> for CardinalSet {
    fn index_mut(&mut self, index: Cardinal) -> &mut Self::Output {
        match index {
            Cardinal::North => &mut self.north,
            Cardinal::East => &mut self.east,
            Cardinal::South => &mut self.south,
            Cardinal::West => &mut self.west,
        }
    }
}

/// A "velocity" in cartesian space, to use with a `Grid`
#[derive(Copy, Clone, Debug)]
pub struct GridDelta(isize, isize);

#[allow(unused)]
impl GridDelta {
    pub const UP: Self = Self(0, -1);
    pub const DOWN: Self = Self(0, 1);
    pub const LEFT: Self = Self(-1, 0);
    pub const RIGHT: Self = Self(1, 0);
    pub const UP_RIGHT: Self = Self(1, -1);
    pub const UP_LEFT: Self = Self(-1, -1);
    pub const DOWN_RIGHT: Self = Self(1, 1);
    pub const DOWN_LEFT: Self = Self(-1, 1);

    pub const CARDINALS: [Self; 4] = [
        GridDelta::UP,
        GridDelta::RIGHT,
        GridDelta::DOWN,
        GridDelta::LEFT,
    ];

    pub const CARDINALS_AND_DIAGONALS: [Self; 8] = [
        GridDelta::UP,
        GridDelta::UP_RIGHT,
        GridDelta::RIGHT,
        GridDelta::DOWN_RIGHT,
        GridDelta::DOWN,
        GridDelta::DOWN_LEFT,
        GridDelta::LEFT,
        GridDelta::UP_LEFT,
    ];

    pub const DIAGONALS: [Self; 4] = [
        GridDelta::UP_RIGHT,
        GridDelta::DOWN_RIGHT,
        GridDelta::DOWN_LEFT,
        GridDelta::UP_LEFT,
    ];

    pub fn inverted(&self) -> Self {
        GridDelta(-self.0, -self.1)
    }

    pub fn vector_between(start: GridAddress, end: GridAddress) -> Self {
        let GridAddress(ax, ay) = start;
        let GridAddress(bx, by) = end;

        let dx_abs = bx.abs_diff(ax);
        let dy_abs = by.abs_diff(ay);

        let dx = if bx > ax {
            dx_abs as isize
        } else {
            -(dx_abs as isize)
        };
        let dy = if by > ay {
            dy_abs as isize
        } else {
            -(dy_abs as isize)
        };

        Self(dx, dy)
    }
}

#[derive(Copy, Clone, Debug, Eq, Hash, PartialEq)]
pub struct GridAddress(pub usize, pub usize);

#[allow(unused)]
impl GridAddress {
    pub fn checked_add(&self, delta: GridDelta) -> Option<GridAddress> {
        let GridDelta(dx, dy) = delta;
        let x = self.0.checked_add_signed(dx)?;
        let y = self.1.checked_add_signed(dy)?;
        Some(GridAddress(x, y))
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

#[allow(unused)]
impl<Tile> Grid<Tile> {
    pub fn get(&self, x: usize, y: usize) -> Option<&Tile> {
        let row = self.rows.get(y)?;
        row.get(x)
    }

    pub fn get_mut(&mut self, x: usize, y: usize) -> Option<&mut Tile> {
        let row = self.rows.get_mut(y)?;
        row.get_mut(x)
    }

    pub fn get_at(&self, addr: GridAddress) -> Option<&Tile> {
        self.get(addr.0, addr.1)
    }

    pub fn get_mut_at(&mut self, addr: GridAddress) -> Option<&mut Tile> {
        self.get_mut(addr.0, addr.1)
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

impl<Tile> Index<GridAddress> for Grid<Tile> {
    type Output = Tile;

    fn index(&self, index: GridAddress) -> &Self::Output {
        self.get_at(index).unwrap_or_else(|| {
            panic!("Illegal address for grid: {:?}", index);
        })
    }
}

impl<Tile> IndexMut<GridAddress> for Grid<Tile> {
    fn index_mut(&mut self, index: GridAddress) -> &mut Self::Output {
        self.get_mut(index.0, index.1).unwrap_or_else(|| {
            panic!("Illegal address for grid: {:?}", index);
        })
    }
}

pub trait RenderTileChar<Tile> {
    fn render_tile_char(&self, tile: &Tile, x: usize, y: usize) -> ColoredString;
}

/// Wrapper struct used for displaying a Grid<Tile>
/// based on some style defined by a custom `style: S`.
pub struct CharGrid<'g, Tile, Style>(pub &'g Grid<Tile>, pub Style);

impl<'g, Tile, C> Display for CharGrid<'g, Tile, C>
where
    C: RenderTileChar<Tile>,
{
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
