#![allow(dead_code)]

use nom::{
    bytes::complete::tag, IResult,
    character::complete::{digit1,char},
    bytes::complete::take_while, sequence::tuple, multi::many0,
};

#[derive(PartialEq, Debug)]
struct Game{
    id: usize,
    rounds: Vec<Round>,
}

#[derive(PartialEq, Debug)]
struct Round{
    draws: Vec<Draw>,
}

#[derive(PartialEq, Debug)]
struct Draw{
    color: Color,
    count: usize,
}

#[derive(PartialEq, Debug)]
enum Color{
    Red,
    Blue,
    Green
}

fn parse_digits_and_string(input: &str) -> IResult<&str, (&str, &str)> {
    // Parse one or more digits
    let (input, digits) = digit1(input)?;

    // Parse a space character
    let (input, _) = char(' ')(input)?;

    // Parse a string (take_while captures characters until a predicate is false)
    let (input, string_part) = take_while(|c: char| c.is_alphabetic())(input)?;

    Ok((input, (digits, string_part)))
}

//1
fn parse_integer(input: &str) -> IResult<&str, &str> {
    digit1(input)
}

//red
fn parse_color(input: &str) -> Color{
    match input {
        "red" => Color::Red,
        "blue" => Color::Blue,
        "green"=> Color::Green,
        _ => panic!("Error parsing color: {input}")
    }
}

// 1 red
fn parse_draw(input: &str) -> IResult<&str, Draw> {
    let (remaining, (digits, string_part)) = parse_digits_and_string(input)?;
    let color = parse_color(string_part);
    let count = digits.parse::<usize>().unwrap();
    Ok((remaining, Draw{color, count}))
}

// 4 red, 12 blue, 6 green
fn parse_round(input: &str) -> nom::IResult<&str, Round> {
    let split_parser = tag(", ");
    let mut combined_parser = tuple((parse_draw, many0(tuple((split_parser, parse_draw)))));
    let (remaining, (first_draw, rest)) = combined_parser(input)?;

    // Collect the results into a Draw
    let mut round = Round{draws: vec![first_draw]};
    for (_, draw) in rest {
        round.draws.push(draw);
    }

    Ok((remaining, round))
}

//Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
fn parse_game(input: &str) -> IResult<&str, Game> {
    let (remainder, _) = tag("Game ")(input)?;
    let (remainder, id) = parse_integer(remainder)?;
    let gameid = id.parse::<usize>().unwrap();
    let (remainder, _) = tag(": ")(remainder)?; // get rid of the ": " after Game 1

    let split_parser = tag("; ");
    let mut combined_parser = tuple((parse_round, many0(tuple((split_parser, parse_round)))));
    let (remainder, (first_round, rest)) = combined_parser(remainder)?;

    let mut game = Game{id: gameid, rounds: vec![first_round]};
    for (_, round) in rest {
        game.rounds.push(round);
    }

    Ok((remainder, game))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_draw_parser(){
        let exampledraw = Draw{color: Color::Red, count: 4};
        assert_eq!(parse_draw("4 red"), Ok(("", exampledraw)));
    }

    #[test]
    fn test_round_parser(){
        let exampleround1 = Round{draws:vec![Draw{color: Color::Red, count: 4}]};
        assert_eq!(parse_round("4 red"), Ok(("", exampleround1)));
        let exampleround2 = Round{draws:vec![Draw{color: Color::Red, count: 4}, Draw{color: Color::Blue, count:12}, Draw{color: Color::Green, count: 6}]};
        assert_eq!(parse_round("4 red, 12 blue, 6 green"), Ok(("", exampleround2)));
    }

    #[test]
    fn test_game_parser(){
        let round1 = Round{draws:vec![Draw{color: Color::Blue, count: 3}, Draw{color: Color::Red, count:4}]};
        let round2 = Round{draws:vec![Draw{color: Color::Red, count: 1}, Draw{color: Color::Green, count:2}, Draw{color: Color::Blue, count: 6}]};
        let round3 = Round{draws:vec![Draw{color: Color::Green, count:2}, Draw{color: Color::Green, count:2}]};

        let examplegame = Game{
            id:1,
            rounds: vec![round1, round2, round3]
        };
        assert_eq!(parse_game("Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green, 2 green"), Ok(("", examplegame)));
    }
}

fn is_possible_game(game: &Game) -> bool {
    for round in &game.rounds{
        for draw in &round.draws {
            match draw.color {
                Color::Red => if draw.count > 12 { return false},
                Color::Green => if draw.count > 13 { return false},
                Color::Blue => if draw.count > 14 { return false},
            }
        }
    }
    true
}

fn minimum_set(game: &Game) -> Vec<Draw> {
    let mut min_red = 0;
    let mut min_blue = 0;
    let mut min_green = 0;

    for round in &game.rounds{
        for draw in &round.draws {
            match draw.color {
                Color::Red => if draw.count > min_red { min_red = draw.count},
                Color::Green => if draw.count > min_green { min_green = draw.count},
                Color::Blue => if draw.count > min_blue { min_blue = draw.count},
            }
        }
    }
    vec![Draw{color: Color::Red, count: min_red}, Draw{color: Color::Blue, count:min_blue}, Draw{color: Color::Green, count: min_green}]
}

fn power(draws: &[Draw]) -> usize{
    return draws.iter().map(|draw| draw.count).product();
}

fn main() {
    //let content = include_str!("../input/example.txt");
    let content = include_str!("../input/input.txt");

    let mut part1_answer = 0;
    let mut part2_answer = 0;

    for line in content.lines() {
        let (_, game) = parse_game(line).unwrap();
        if is_possible_game(&game){
            part1_answer += game.id;
        }
        part2_answer += power(&minimum_set(&game));
    }

    println!("The answer to part 1 is {part1_answer:?}");
    println!("The answer to part 2 is {part2_answer:?}");
}
