use std::collections::{HashMap, HashSet};
use crate::helper::GenResult;
use colored::Colorize;
use log::{debug, info};
use std::error::Error;
use std::fmt::{Debug, Display, Formatter, Write};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::mem::swap;
use std::path::PathBuf;
use std::str::FromStr;

pub fn run(input_path: &PathBuf) -> GenResult<()> {
    let (rules, updates) = parse_input(input_path)?;
    let rules = OrderingRules::new(rules);

    let mut sum_of_valid = 0u32;
    let mut sum_of_fixed = 0u32;
    for update in updates {
        let classified = classify(&update, &rules);
        info!("{:?}", classified);
        sum_of_valid += classified.value();
        if !classified.is_valid {
            let fixed = fix_order(&update, &rules);
            let mut reclassified = classify(&fixed, &rules);
            reclassified.was_fixed = true;
            info!(" fixed: {:?}", reclassified);
            sum_of_fixed += reclassified.value();
        }
    }
    info!("part 1 sum: {}", sum_of_valid.to_string().green());
    info!("part 2 sum: {}", sum_of_fixed.to_string().yellow());

    Ok(())
}

/// Parser for the puzzle input.
/// First section is a list of ordering rules, followed by a blank line,
/// followed by a list of updates.
fn parse_input(input_path: &PathBuf) -> GenResult<(Vec<OrderingRule>, Vec<Update>)> {
    let file = File::open(input_path)?;
    let reader = BufReader::new(file);
    let mut is_parsing_rules = true;
    let mut rules = Vec::new();
    let mut pages = Vec::new();
    for line in reader.lines() {
        let line = line?;
        if is_parsing_rules && !line.is_empty() {
            let rule = line.parse::<OrderingRule>()?;
            debug!("{}", rule);
            rules.push(rule);
        } else if is_parsing_rules && line.is_empty() {
            debug!("switch to parsing pages...");
            is_parsing_rules = false;
        } else {
            let mut p = Vec::new();
            for s in line.split_terminator(',') {
                p.push(s.parse::<u32>()?);
            }
            debug!("pages: {:?}", p);
            pages.push(Update(p));
        }
    }
    Ok((rules, pages))
}

/// An ordering rule from the puzzle input.
/// E.g. `12|75` would be `OrderingRule { prefix: 12, suffix: 75 }`
#[derive(Copy, Clone, Debug)]
struct OrderingRule {
    prefix: u32,
    suffix: u32,
}
impl Display for OrderingRule {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} must be before {}", self.prefix, self.suffix)
    }
}
impl FromStr for OrderingRule {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (l, r) = s.split_once('|').ok_or("missing '|'")?;
        let l = l.parse::<u32>()?;
        let r = r.parse::<u32>()?;
        Ok(OrderingRule {
            prefix: l,
            suffix: r,
        })
    }
}

/// Summary struct to represent the collection of `OrderingRule`s from the puzzle input,
/// but with an O(1) way to know if a given pair of page numbers is valid.
#[derive(Debug)]
struct OrderingRules {
    by_start: HashMap<u32, HashSet<u32>>,
}

impl OrderingRules {
    fn new(vec: Vec<OrderingRule>) -> OrderingRules {
        let mut by_start = HashMap::new();

        for OrderingRule { prefix, suffix } in &vec {
            by_start.entry(*prefix).or_insert_with(HashSet::new).insert(*suffix);
        }

        OrderingRules {
            by_start,
        }
    }

    fn has_rule(&self, prefix: u32, suffix: u32) -> bool {
        match self.by_start.get(&prefix) {
            None => false,
            Some(set) => set.contains(&suffix)
        }
    }

    fn is_legal(&self, prefix: u32, suffix: u32) -> bool {
        self.has_rule(prefix, suffix) || !self.has_rule(suffix, prefix)
    }
}

/// A list of "page numbers" from the puzzle input
#[derive(Debug)]
struct Update(Vec<u32>);


/// Represents a page in a `ClassifiedUpdate`
struct ClassifiedPage {
    num: u32,
    is_violation: bool,
}

/// Fancy representation of an `Update`'s classification,
/// including an overall `is_valid` flag, and the individual
/// violations that were found in the update.
///
/// Really this just exists so we can have pretty colored output.
struct ClassifiedUpdate {
    pages: Vec<ClassifiedPage>,
    is_valid: bool,
    was_fixed: bool,
}

impl ClassifiedUpdate {

    /// Get the value of the middle page in this update.
    /// Used for the scoring calculation for the puzzle.
    fn value(&self) -> u32 {
        if self.is_valid {
            self.pages[self.pages.len() / 2].num
        } else {
            0
        }
    }
}

// Fancy colors!
impl Debug for ClassifiedUpdate {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_char('[')?;

        let middle_index = self.pages.len() / 2;

        for (idx, page) in self.pages.iter().enumerate() {
            if idx > 0 {
                f.write_str(", ")?;
            }
            if idx == middle_index && self.is_valid {
                let num_str = page.num.to_string();
                if self.was_fixed {
                    write!(f, "{}", num_str.yellow())?;
                } else {
                    write!(f, "{}", num_str.green())?;
                }
            } else if page.is_violation {
                write!(f, "{}", page.num.to_string().red())?;
            } else {
                write!(f, "{}", page.num)?;
            }
        }

        f.write_char(']')?;

        Ok(())
    }
}

/// Part 1 helper:
/// Inspects pairs of "pages" (numbers) in the update. If any pair is "illegal",
/// the overall result will have `is_valid: false`. The individual violations will
/// also be marked, so we can have pretty debug output.
fn classify(update: &Update, rules: &OrderingRules) -> ClassifiedUpdate {
    let mut classified_pages = update.0.iter()
        .map(|num| ClassifiedPage { num: *num, is_violation: false })
        .collect::<Vec<_>>();

    // run the compliance check, marking `is_correct = false` on pages violating the order
    for i in 0..update.0.len() {
        let prefix = update.0[i];
        for j in (i+1)..update.0.len() {
            let suffix = update.0[j];
            if !rules.is_legal(prefix, suffix) {
                classified_pages[i].is_violation = true;
                classified_pages[j].is_violation = true;
            }
        }
    }

    let is_valid = classified_pages.iter().all(|page| !page.is_violation);

    ClassifiedUpdate {
        pages: classified_pages,
        is_valid,
        was_fixed: false,
    }
}

/// Part 2 helper:
/// Works as a kind of bubble sort. Visit each pair of values in the update,
/// swapping them if they are not legal according to the `rules`. Whenever a
/// swap occurs, re-inspect pairs after the swapped index, since a swap may
/// put a number in front of another number that it was supposed to be behind.
fn fix_order(update: &Update, rules: &OrderingRules) -> Update {
    debug!("Attempting to fix {:?}", update);
    let mut fixed_pages = update.0.clone();

    for i in 0..fixed_pages.len() {
        debug!("at index {}", i);
        let mut did_swap = true;
        let (fixed_so_far, to_be_fixed) = fixed_pages.split_at_mut(i + 1);
        while did_swap {
            did_swap = false;
            let prefix = fixed_so_far.last_mut().unwrap();
            for suffix in to_be_fixed.iter_mut() {
                if !rules.is_legal(*prefix, *suffix) {
                    debug!("swap {} and {}", prefix, suffix);
                    swap(prefix, suffix);
                    did_swap = true;
                }
            }
        }
        debug!("settled on {}", fixed_so_far.last().unwrap());
    }

    Update(fixed_pages)
}