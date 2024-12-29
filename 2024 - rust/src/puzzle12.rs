use crate::geometry::{
    Cardinal, CardinalSet, CharGrid, Grid, GridAddress, GridDelta, RenderTileChar,
};
use crate::helper::GenResult;
use colored::{Color, ColoredString, Colorize};
use log::{debug, info};
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub fn run(input_path: &Path) -> GenResult<()> {
    // Parse input
    let input_grid = {
        let mut rows = Vec::new();
        let file = File::open(input_path)?;
        let reader = BufReader::new(file);
        for line in reader.lines() {
            let line = line?;
            rows.push(line.chars().collect::<Vec<_>>());
        }
        Grid { rows }
    };

    info!("Initial Grid:\n{}", CharGrid(&input_grid, ()));

    // Initialize a grid of `Tile` from the original input
    let mut tiles = {
        let rows = input_grid
            .rows
            .iter()
            .map(|row| {
                row.iter()
                    .map(|c| Tile {
                        letter: *c,
                        fences: CardinalSet::default(),
                        area_id: None,
                    })
                    .collect()
            })
            .collect();
        Grid { rows }
    };

    // Assign `area_id`s to each tile
    let areas = flood(&mut tiles);

    info!("Tiles:\n{}", CharGrid(&tiles, FancyColors));

    // compute the total price of each area's fences/edges
    let mut part1_total = 0;
    let mut part2_total = 0;
    for (area_id, area) in &areas {
        let size = area.addresses.len();
        let fence_count = {
            let mut accum = 0;
            for addr in &area.addresses {
                accum += tiles[*addr].fences.len();
            }
            accum
        };
        let edge_count = count_edges(&tiles, area);

        let p1_price = size * fence_count;
        let p2_price = size * edge_count;

        info!(
            "Region {} {{ id: {:?}, size: {}, fences: {}, edges: {} }} - prices {} vs {}",
            area.letter.to_string().color(randomish_color(area_id)),
            area_id,
            size,
            fence_count,
            edge_count,
            p1_price.to_string().yellow(),
            p2_price.to_string().green(),
        );
        part1_total += p1_price;
        part2_total += p2_price;
    }

    info!("Part 1 Price: {}", part1_total.to_string().yellow());
    info!("Part 2 Price: {}", part2_total.to_string().green());

    Ok(())
}

impl RenderTileChar<char> for () {
    fn render_tile_char(&self, tile: &char, _: usize, _: usize) -> ColoredString {
        tile.to_string().normal()
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
struct AreaId(usize);

#[derive(Debug)]
struct Tile {
    letter: char,
    area_id: Option<AreaId>,
    fences: CardinalSet,
}

struct Area {
    letter: char,
    addresses: HashSet<GridAddress>,
}

/// For pretty debug output
fn randomish_color(area_id: &AreaId) -> Color {
    let id = area_id.0;
    Color::TrueColor {
        // just mashed keyboard to come up with a pseudo hashing function
        r: (((id + 83) * 283764) % 256) as u8,
        g: (((id + 3) * 9198723) % 256) as u8,
        b: (((id + 200) * 6523) % 256) as u8,
    }
}

/// CharGrid display style
struct FancyColors;

impl RenderTileChar<Tile> for FancyColors {
    fn render_tile_char(&self, tile: &Tile, _: usize, _: usize) -> ColoredString {
        let s = tile.letter.to_string();
        if let Some(area_id) = &tile.area_id {
            s.color(randomish_color(area_id))
        } else {
            s.normal()
        }
    }
}

/// Assigns an `area_id` to each `Tile` based on a flood-fill from each point.
/// I.e. consecutive tiles with the same letter will be assigned the same `area_id`.
/// The addresses of each area will be collect as an `Area` struct.
fn flood(grid: &mut Grid<Tile>) -> HashMap<AreaId, Area> {
    let mut next_id = 0usize;

    let mut areas = HashMap::new();

    for y in 0..grid.height() {
        for x in 0..grid.width() {
            let addr = GridAddress(x, y);
            if grid[addr].area_id.is_none() {
                let id = AreaId(next_id);
                areas.insert(
                    id,
                    Area {
                        letter: grid[addr].letter,
                        addresses: HashSet::new(),
                    },
                );
                next_id += 1;
                flood_from(addr, grid, id, &mut areas);
            }
        }
    }

    areas
}

/// Helper for `flood`
fn flood_from(
    addr: GridAddress,
    grid: &mut Grid<Tile>,
    area_id: AreaId,
    areas: &mut HashMap<AreaId, Area>,
) {
    grid[addr].area_id = Some(area_id);
    areas.entry(area_id).and_modify(|area| {
        area.addresses.insert(addr);
    });

    for cardinal in Cardinal::ALL {
        if let Some(neighbor_addr) = addr.checked_add(cardinal.into()) {
            if let Some(neighbor) = grid.get_at(neighbor_addr) {
                match neighbor.area_id {
                    None => {
                        if neighbor.letter == grid[addr].letter {
                            // area continues into this neighboring tile; continue the flood fill
                            flood_from(neighbor_addr, grid, area_id, areas);
                        } else {
                            // neighbor is a different area; add a fence
                            grid[addr].fences += cardinal;
                        }
                    }
                    Some(neighbor_area) => {
                        if neighbor_area != area_id {
                            // encountered another already-marked area that is separate from this one; add a fence
                            grid[addr].fences += cardinal;
                        }
                    }
                }
            } else {
                // address was at South/East edge of the grid and we're trying to go beyond the grid's bounds; add a fence
                grid[addr].fences += cardinal;
            }
        } else {
            // address was at the North/West edge of the grid and we're trying to go into negative address space; add a fence
            grid[addr].fences += cardinal;
        }
    }
}

/// Part 2 algorithm:
/// For any tile with a fence, try following the fence in either direction,
/// detecting an "edge" for any unbroken series of fences.
fn count_edges(grid: &Grid<Tile>, area: &Area) -> usize {
    let mut seen = HashSet::new();
    let mut edge_counts = 0;
    debug!("scanning for edges in {} zone", area.letter);
    for addr in &area.addresses {
        let tile = &grid[*addr];
        let area_id = tile.area_id;
        for cardinal in Cardinal::ALL {
            if tile.fences.contains(cardinal) && !seen.contains(&(*addr, cardinal)) {
                edge_counts += 1;
                seen.insert((*addr, cardinal));
                let mut length = 1;
                let d_plus: GridDelta = cardinal.turn_right().into();
                let d_minus: GridDelta = cardinal.turn_left().into();

                for delta in [d_plus, d_minus] {
                    let mut pos = *addr;
                    'edge_scan: while let Some(neighbor) =
                        pos.checked_add(delta).and_then(|addr| grid.get_at(addr))
                    {
                        if neighbor.area_id != area_id {
                            break 'edge_scan;
                        }
                        pos = pos.checked_add(delta).unwrap();
                        if neighbor.fences.contains(cardinal) {
                            seen.insert((pos, cardinal));
                            length += 1;
                        } else {
                            break 'edge_scan;
                        }
                    }
                }

                debug!(
                    " found edge on {:?} side of {:?} with length {}",
                    cardinal, addr, length
                );
            }
        }
    }
    edge_counts
}
