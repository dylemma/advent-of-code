use crate::geometry::{Cardinal, CardinalSet, CharGrid, Grid, GridAddress, RenderTileChar};
use crate::helper::GenResult;
use colored::{ColoredString, Colorize};
use log::info;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub fn run(input_path: &Path) -> GenResult<()> {
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
    let areas = flood(&mut tiles);

    info!("Tiles:\n{}", CharGrid(&tiles, FancyColors));

    let mut part1_score = 0;
    for (_, area) in &areas {
        let fence_count = {
            let mut accum = 0;
            for addr in &area.addresses {
                accum += tiles[*addr].fences.len();
            }
            accum
        };
        info!(
            "Region {} price {} * {} = {}",
            area.letter,
            area.addresses.len(),
            fence_count,
            area.addresses.len() * fence_count,
        );
        part1_score += area.addresses.len() * fence_count;
    }

    info!("Total price: {}", part1_score);

    Ok(())
}

impl RenderTileChar<char> for () {
    fn render_tile_char(&self, tile: &char, _: usize, _: usize) -> ColoredString {
        tile.to_string().normal()
    }
}

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
struct AreaId(usize);

struct Tile {
    letter: char,
    area_id: Option<AreaId>,
    fences: CardinalSet,
}

struct Area {
    letter: char,
    addresses: HashSet<GridAddress>,
}

struct FancyColors;

impl RenderTileChar<Tile> for FancyColors {
    fn render_tile_char(&self, tile: &Tile, _: usize, _: usize) -> ColoredString {
        let s = tile.letter.to_string();
        if let Some(AreaId(id)) = tile.area_id {
            s.truecolor(
                (((id + 83) * 283764) % 256) as u8,
                (((id + 3) * 9198723) % 256) as u8,
                (((id + 200) * 6523) % 256) as u8,
            )
        } else {
            s.normal()
        }
    }
}

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
