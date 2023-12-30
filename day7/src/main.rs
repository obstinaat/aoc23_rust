use nom::{
    character::complete::{digit1, multispace0},
    bytes::complete::take_while,
    combinator::map,
    sequence::tuple,
    IResult,
};
use std::collections::HashMap;

#[derive(Debug)]
enum HandRank {
    FiveOfAKind,
    FourOfAKind,
    FullHouse,
    ThreeOfAKind,
    TwoPair,
    OnePair,
    HighCard,
}

// order allows us to sort the cards later.
const CAMEL_ORDER_CHAR_TABLE : [(char, char); 14] = [
    ('A', 'a'),
    ('K', 'b'),
    ('Q', 'c'),
    ('J', 'd'),
    ('T', 'e'),
    ('9', 'f'),
    ('8', 'g'),
    ('7', 'h'),
    ('6', 'i'),
    ('5', 'j'),
    ('4', 'k'),
    ('3', 'l'),
    ('2', 'm'),
    ('1', 'n')
];

fn lookup_camel_table(key: char) -> Option<char> {
        for (k, v) in CAMEL_ORDER_CHAR_TABLE.iter() {
            if *k == key {
                return Some(*v);
            }
        }
    panic!("A card rank was not found in the camel order table. {key}")
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
enum Rank {
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}

#[derive(Clone, Copy)]
struct Card{rank: Rank}

#[derive(Debug)]
struct ParsedData {
    string_part: String,
    integer_part: usize,
}

#[derive(Debug)]
struct Hand {
    cards: String,
    bid: usize,
    camel_rank: usize,
    hand_rank: HandRank,
    camel_code: String,
}   


fn parse_rank(input: &char) -> Option<Rank> {
    match input {
        '2' => Some(Rank::Two),
        '3' => Some(Rank::Three),
        '4' => Some(Rank::Four),
        '5' => Some(Rank::Five),
        '6' => Some(Rank::Six),
        '7' => Some(Rank::Seven),
        '8' => Some(Rank::Eight),
        '9' => Some(Rank::Nine),
        'T' => Some(Rank::Ten),
        'J' => Some(Rank::Jack),
        'Q' => Some(Rank::Queen),
        'K' => Some(Rank::King),
        'A' => Some(Rank::Ace),
        _ => panic!("A rank was unable to get parsed: {input}"),
    }
}

fn parse_hand(input: &str) -> [Card; 5] {
    let mut hand: Vec<Card> = Vec::new();
    for c in input.chars(){
        let rank = parse_rank(&c).unwrap();
        let card = Card{rank};
        hand.push(card);
    }

    [hand[0], hand[1], hand[2], hand[3], hand[4]]
}

fn determine_rank(hand: &[Card; 5]) -> HandRank {
    let mut ranks = HashMap::new();

    for card in hand {
        *ranks.entry(&card.rank).or_insert(0) += 1;
    }

    if let Some(_) = ranks.iter().find(|&(_, &count)| count == 5) {
        return HandRank::FiveOfAKind;
    }

    if let Some(_) = ranks.iter().find(|&(_, &count)| count == 4) {
        return HandRank::FourOfAKind;
    }

    if ranks.len() == 2 {
        let counts: Vec<_> = ranks.values().copied().collect();
        if counts.contains(&3) && counts.contains(&2) {
            return HandRank::FullHouse;
        }
    }

    if let Some(_) = ranks.iter().find(|&(_, &count)| count == 3) {
        return HandRank::ThreeOfAKind;
    }

    let pairs: Vec<_> = ranks.iter().filter(|&(_, &count)| count == 2).collect();
    match pairs.len() {
        2 => HandRank::TwoPair,
        1 => HandRank::OnePair,
        _ => HandRank::HighCard,
    }
}

fn parse_input(input: &str) -> IResult<&str, ParsedData> {
    // Define the parser for a string (take_while until a space is encountered)
    let string_parser = map(take_while(|c| c != ' '), |s: &str| s.to_string());

    // Define the main parser for the tuple
    let mut parser = tuple((
        string_parser,    // Parse the string part
        multispace0,
        map(digit1, |s: &str| s.parse::<usize>().unwrap()), // Parse an integer
    ));

    // Apply the parser and map the results
    let (remaining_input, (parsed_str, _, parsed_int)) = parser(input)?;

    // Return the parsed data
    Ok((
        remaining_input,
        ParsedData {
            string_part: parsed_str,
            integer_part: parsed_int,
        },
    ))
}

fn determine_camel_code(hand_as_input: &str, handrank: &HandRank) -> String {
    let mut camel_code = "".to_string();
    match handrank {
        HandRank::FiveOfAKind => camel_code.push('a'),
        HandRank::FourOfAKind => camel_code.push('b'),
        HandRank::FullHouse => camel_code.push('c'),
        HandRank::ThreeOfAKind => camel_code.push('d'),
        HandRank::TwoPair => camel_code.push('e'),
        HandRank::OnePair => camel_code.push('f'),
        HandRank::HighCard => camel_code.push('g'),
    };

    for card_as_input in hand_as_input.chars() {
        camel_code.push(lookup_camel_table(card_as_input).unwrap());
    }

    camel_code


}

fn determine_winnings(hands: &Vec<Hand>) -> usize{
    let mut winnings: usize = 0;
    for hand in hands{
        winnings += hand.bid*hand.camel_rank;
    }
    winnings
}

fn main() {
    let content = include_str!("../input/input.txt").lines();

    let mut hands: Vec<Hand> = vec![];
    for line in content{
        match parse_input(line) {
        Ok((_, parsed_data)) => {
            let parsed_hand = parse_hand(&parsed_data.string_part);
            let handrank = determine_rank(&parsed_hand);
            let camel_code = determine_camel_code(&parsed_data.string_part, &handrank);
            hands.push(Hand{cards: parsed_data.string_part, bid: parsed_data.integer_part, camel_rank: 0, hand_rank: handrank, camel_code: camel_code});
        }
        Err(err) => eprintln!("Parsing error: {err}"),
        }
    }

    //Sort the hands by camel code
    hands.sort_by_key(|hand| hand.camel_code.clone());
    let card_count = hands.len();
    for (index, hand) in hands.iter_mut().enumerate(){
        hand.camel_rank = card_count - index;
    }

    
    println!("{:?}", determine_winnings(&hands));
}