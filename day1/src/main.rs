fn first_digit(line: &str) -> u32{
    let mut digit = 0;
    for c in line.chars(){
        if c.is_digit(10){
            digit = c.to_digit(10).unwrap();
            return digit;
        }
    }
    digit
}

fn last_digit(line: &str) -> u32{
    let reversed:String = line.to_string().chars().rev().collect();

    first_digit(&reversed)
}

fn main() {

    let mut totalsum = 0;

    for line in  include_str!("input.txt")
        .replace("\r\n", "\n")
        .split("\n")
    {
        let sum = first_digit(line)*10 + last_digit(line);
        totalsum += sum;
    }

    println!("{totalsum}");
}
