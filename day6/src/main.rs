use std::time::Instant;

use regex::Regex;


fn extract_integers_from_string(input_string: &str) -> Vec<usize> {
    // Create a regular expression to find all integer occurrences in the string
    let re = Regex::new(r"\b\d+\b").unwrap();

    // Use the find_iter method to iterate over all matches in the string
    let integers: Vec<usize> = re
        .find_iter(input_string)
        .map(|m| m.as_str().parse::<usize>().unwrap())
        .collect();

    integers
}

#[test]
fn test_extraction() {
    assert_eq!(vec![7, 15, 30], extract_integers_from_string("Time:      7  15   30"));
}

fn part1() {
    let mut content = include_str!("../input/input.txt").lines();

    let firstline = content.next().unwrap();
    let secondline = content.next().unwrap();

    let times = extract_integers_from_string(firstline);
    let distances = extract_integers_from_string(secondline);

    let races: Vec<_> = times.iter().zip(distances.iter()).collect();

    let mut ways_to_win: Vec<usize>= vec![];
    for (t, d) in races{
        let mut possibilities = 0;
        for i in 0..t+1 {
            if i * (t-i) > *d {
                possibilities +=1;
            }
        }
        ways_to_win.push(possibilities);
    }

    let margin: usize = ways_to_win.iter().product();
    println!("The answer to part 1 is {:?}", margin);
}


fn part2_brute(){
    let t = 59688274; // gotten manually from input
    let d:u64 = 543102016641022; // should be fine for now ;)

    let mut possibilities: u64 = 0;
    for i in 0..t+1 {
        if i * (t-i) > d {
            possibilities +=1;
        }
    }

    println!("The answer to part 2 is {:?}", possibilities);
}

fn part2_quick(){
    let t:u64 = 59688274; 
    let d:u64 = 543102016641022;

    let disc:f64 = (t.pow(2) - 4 * 1 * d) as f64; //b^2 - 4ac 
    let (x1,x2) = (t as f64 - disc.sqrt() / 2.0, t as f64 + disc.sqrt() / 2.0);

    println!{"The answer to part 2 yet again is {:?}",(x2.floor() - x1.floor()) as usize};
}

fn main() {
    part1();
    let start = Instant::now();
    part2_brute();
    part2_quick();
    println!("--- Time: {:1.3?} ---\n", start.elapsed());
}
