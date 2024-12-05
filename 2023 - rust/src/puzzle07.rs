use std::collections::HashMap;
use std::fmt::{Debug, Display, Formatter, Write};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};
use crate::helper::{GenResult, parse_u32};

pub fn run(path: &Path, debug_on: bool) -> GenResult<()> {

    let hands = BufReader::new(File::open(path)?)
        .lines()
        .map(|l| l.map_err(|e| e.to_string()))
        .map(|l| Hand::try_from(l?.as_str()))
        .collect::<Result<Vec<_>, _>>()?;

    for hand in &hands {
        println!("{:?} -> {:?}", hand, calc_hand_type(hand));
    }
    Ok(())
}

struct Card {
    face: char,
    value: u32,
}

impl Debug for Card {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_char(self.face)
    }
}

impl TryFrom<char> for Card {
    type Error = String;

    fn try_from(c: char) -> Result<Self, Self::Error> {
        let value = match c {
            '2'..='9' => c.to_digit(10).unwrap(),
            'T' => 10,
            'J' => 11,
            'Q' => 12,
            'K' => 13,
            'A' => 14,
            _ => Err("Invalid card face")?,
        };
        Ok(Card { face: c, value })
    }
}

#[derive(Debug)]
struct Hand {
    cards: Vec<Card>,
    bid: u32,
}

impl TryFrom<&str> for Hand {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let (cards, bid) = value.split_once(' ').ok_or("missing space")?;
        let cards = cards.chars().map(Card::try_from).collect::<Result::<Vec<_>, _>>()?;
        let bid = parse_u32(bid)?;
        Ok(Hand { cards, bid })
    }
}

fn calc_hand_type(hand: &Hand) -> HandType {
    let mut num_per_face = HashMap::new();
    for Card { value, .. } in &hand.cards {
        num_per_face
            .entry(value)
            .and_modify(|n| { *n += 1; })
            .or_insert(1u32);
    }
    let sizes = num_per_face.iter().map(|(k, c)| *c).collect::<Vec<_>>();

    let num_fours = sizes.iter().filter(|&&c| c == 4).count();
    let num_triples = sizes.iter().filter(|&&c| c == 3).count();
    let num_pairs = sizes.iter().filter(|&&c| c == 2).count();

    if sizes.len() == 1 { HandType::FiveOfAKind }
    else if num_fours == 1 { HandType::FourOfAKind }
    else if num_triples == 1 && num_pairs == 1 { HandType::FullHouse }
    else if num_triples == 1 { HandType::ThreeOfAKind }
    else if num_pairs == 2 { HandType::TwoPair }
    else if num_pairs == 1 { HandType::OnePair }
    else { HandType::HighCard }
}

#[derive(Debug)]
enum HandType {
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
}

