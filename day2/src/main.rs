use nom::{
    IResult,
    bytes::complete::{tag},
    sequence::separated_pair,
    character::streaming::char,
    character::streaming::digit1, error::Error,
    sequence::preceded, combinator::map,
    combinator::all_consuming,
    Finish
};

struct Game(i32);

// fn parse_Game(i: &str) -> IResult<&str, Game> {
//     map(preceded(tag("Game "), digit1), Game)(i)
// }

fn parse_line(i: &str) -> IResult<&str, &str>{
    Ok(("hi!~", "rest"))
}
  
fn main(){
    println!("help!");

    let lines = include_str!("input.txt")
    .lines()
    .map(|l| all_consuming(parse_line)(l).finish().unwrap().1);

    for line in lines {
        println!("{line:?}");
    }
}