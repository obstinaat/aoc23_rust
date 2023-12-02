fn first_digit(mut line: &str) -> u32{
    let mut digit = 0;
    while line.len() > 0 {
        for word in WORDS{
            if line.starts_with(word.0) || line.starts_with(&word.1.to_string()){
                digit = word.1;
                return digit;
            }
        }
        line = &line[1..];
    }
    digit
}

fn last_digit(line: &str) -> u32{
    let reverse: String = line.to_string().chars().rev().collect();
    let mut reversed_line: &str = &reverse;
    let mut digit = 0;

    while reversed_line.len() > 0{ 
        for reversed_word in WORDS.into_iter().map(|x| reverse_word(x)){
            if reversed_line.starts_with(&reversed_word.0) || reversed_line.starts_with(&reversed_word.1.to_string()){
                digit = reversed_word.1;
                return digit;
            }
        }
        reversed_line = &reversed_line[1..];
    }
    digit
}

const WORDS: [(&str, u32); 9] =
    [("one", 1), ("two", 2), ("three", 3), ("four", 4), ("five", 5), ("six", 6), ("seven", 7), ("eight", 8), ("nine", 9)];

fn reverse_word(tuple:(&str, u32)) -> (String, u32){
    (tuple.0.chars().rev().collect(), tuple.1)
}

fn main() {
    let mut totalsum = 0;

    for line in  include_str!("input.txt").lines()
    {
        let sum = first_digit(line)*10 + last_digit(line);
        println!("{} {}", first_digit(line), last_digit(line));
        totalsum += sum;
    }

    println!("{totalsum}");
}
