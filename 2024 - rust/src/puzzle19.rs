use crate::helper::GenResult;
use colored::Colorize;
use log::{debug, info};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

pub fn run(input_path: &Path) -> GenResult<()> {
    // Parse input
    let (tiles, targets) = {
        let file = File::open(input_path)?;
        let reader = BufReader::new(file);
        let mut lines = reader.lines();

        let tiles = lines
            .next()
            .ok_or("missing first line")??
            .split(", ")
            .map(str::to_string)
            .collect::<Vec<_>>();
        let mut targets = Vec::new();
        assert!(lines.next().ok_or("missing second line")??.is_empty());
        while let Some(line) = lines.next() {
            targets.push(line?);
        }

        (tiles, targets)
    };

    debug!("tiles: {:?}", tiles);
    debug!("targets: {:?}", targets);

    let tile_index = towels::TileIndex::from_iter(tiles);

    let mut fit_count = 0; // for part 1
    let mut total_paths = 0; // for part 2
    for target in targets {
        let d = towels::decompose(target, &tile_index);
        debug!("Decomposition: {:?}", d);
        if let Some(segments) = d.to_tiles() {
            fit_count += 1;

            let num_distinct_paths = d.how_many_ways();
            total_paths += num_distinct_paths;

            info!(
                "Fit '{}':\nexample path: {}\ntotal paths: {}",
                d.goal,
                format!("{:?}", segments).green(),
                num_distinct_paths.to_string().bright_blue()
            );
        } else {
            info!("Fit '{}':\n{}", d.goal, "no path".red());
        }
    }
    info!(
        "Successfully fit {} patterns!",
        fit_count.to_string().green()
    );
    info!(
        "Total {} unique arrangements",
        total_paths.to_string().bright_blue()
    );

    Ok(())
}

mod towels {
    use std::collections::{BTreeMap, BTreeSet, HashMap};
    use std::fmt::{Debug, Formatter};
    use std::str::Chars;

    /// Detect all substrings of `goal` that have corresponding tiles from the puzzle input.
    /// Result is effectively a `Set<(start_index, end_index)>`, represented by the `Decomposition` struct.
    pub fn decompose(goal: String, tiles: &TileIndex) -> Decomposition {
        let edges_by_index = (0..goal.len())
            .map(|offset| {
                let inner_goal = &goal[offset..];
                tiles
                    .available_prefix_lengths(inner_goal)
                    .into_iter()
                    .map(|len| offset + len)
                    .collect::<BTreeSet<_>>()
            })
            .collect::<Vec<_>>();

        Decomposition {
            goal,
            edges_by_index,
        }
    }

    /// Decomposition of a `goal` string into a pathfinding graph.
    /// Each entry at index `i` in the `edges_by_index` Vec represents a set of outgoing edges
    /// from `i` to the corresponding indexes. An edge from `i` to `j` represents the presence
    /// of a tile (from the puzzle input) matching the characters `&goal[i..j]`
    pub struct Decomposition {
        pub goal: String,
        edges_by_index: Vec<BTreeSet<usize>>,
    }

    impl Debug for Decomposition {
        fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
            write!(f, "For goal '{}'", self.goal)?;
            for (i, edges) in self.edges_by_index.iter().enumerate() {
                write!(f, "\n  from [{}] -> {:?}", i, edges)?;
            }
            Ok(())
        }
    }

    impl Decomposition {
        /// Finds a path from `0..self.goal.len()` based on this decomposition,
        /// where the path can be interpreted as a series of tiles that could
        /// be concatenated to form the `goal`.
        pub fn to_tiles(&self) -> Option<Vec<&str>> {
            let (path, _) = pathfinding::directed::astar::astar(
                &0usize,
                |i| {
                    self.edges_by_index
                        .get(*i)
                        .iter()
                        .flat_map(|set| set.iter())
                        .map(|dest| (*dest, *dest - *i))
                        .collect::<Vec<_>>()
                },
                |i| self.goal.len() - i,
                |&i| i == self.goal.len(),
            )?;

            let mut here = 0;
            let mut out = Vec::new();
            for &next in path[1..].iter() {
                let segment = &self.goal[here..next];
                here = next;
                out.push(segment);
            }
            Some(out)
        }

        /// Count the number of distinct paths from 0 to `self.goal.len()`.
        /// Since there can be many ways to reach any given index of the goal string,
        /// and many ways to reach the final goal from any given index, the total count
        /// ends up being exponentially large with respect to the goal length.
        /// A cache is used to avoid re-computing the path counts for any given index.
        pub fn how_many_ways(&self) -> u64 {
            self.how_many_ways_to(self.goal.len(), &mut HashMap::new())
        }

        fn how_many_ways_to(&self, destination: usize, cache: &mut HashMap<usize, u64>) -> u64 {
            if destination == 0 {
                1
            } else {
                match cache.get(&destination) {
                    Some(num_ways) => *num_ways,
                    None => {
                        // find edges in the decomposition that lead to `destination`
                        let mut count = 0;
                        for (from_idx, edges) in self.edges_by_index.iter().enumerate() {
                            if edges.contains(&destination) {
                                count += self.how_many_ways_to(from_idx, cache);
                            }
                        }
                        cache.insert(destination, count);
                        count
                    }
                }
            }
        }
    }

    /// Trie structure used to for fast prefix lookups quickly check whether a given goal string has
    /// prefixes represented in tiles from the puzzle input.
    #[derive(Debug)]
    pub(crate) struct TileIndex {
        /// Children. If this node represents "foo", then `m.get("d")` would represent "food".
        m: BTreeMap<char, TileIndex>,
        /// Whether this node is representative of an actual value in the set. If false,
        /// it is implied that `m` contains nodes which have `has_value: true`.
        has_value: bool,
    }

    impl TileIndex {
        pub fn new() -> Self {
            TileIndex {
                m: BTreeMap::new(),
                has_value: false,
            }
        }

        fn insert(&mut self, value: String) {
            let value_copy = value.clone();
            let chars = value_copy.chars();
            self.insert_r(value, chars);
        }

        fn insert_r(&mut self, value: String, mut path: Chars) {
            if let Some(c) = path.next() {
                let child = self.m.entry(c).or_insert_with(|| TileIndex::new());
                child.insert_r(value, path);
            } else {
                self.has_value = true;
            }
        }

        pub fn available_prefix_lengths(&self, target: &str) -> Vec<usize> {
            let mut out = Vec::new();
            self.collect_prefix_lengths(target.chars(), 0, &mut out);
            out
        }

        fn collect_prefix_lengths(&self, mut target: Chars, accum: usize, out: &mut Vec<usize>) {
            if self.has_value {
                if accum > 0 {
                    out.push(accum);
                }
            }
            if let Some(c) = target.next() {
                if let Some(child) = self.m.get(&c) {
                    child.collect_prefix_lengths(target, accum + 1, out);
                }
            }
        }
    }

    impl FromIterator<String> for TileIndex {
        fn from_iter<T: IntoIterator<Item = String>>(iter: T) -> Self {
            let mut root = TileIndex::new();
            for item in iter {
                root.insert(item);
            }
            root
        }
    }
}
