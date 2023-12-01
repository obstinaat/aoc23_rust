use std::fs;

fn get_floor_num_and_basement_position(line: &str) -> (i32,i32){
    let mut floor = 0;
    let mut position = 0;
    let mut basement_position = 0;

    for c in line.chars() {
        position +=1;
        if c == '(' {
            floor+=1;
        } else if c == ')' {
            floor-=1;
        }
        
        if floor == -1 && basement_position == 0 {
            //We have entered the basement
            //For the first time, as basement_position was never set to a nonzero value.
            basement_position = position;
        }
   }    

    (floor, basement_position)
}

fn main() {
    let content = fs::read_to_string("input.txt").expect("Was unable to read the file.");

    let (floor,basement_position) = get_floor_num_and_basement_position(&content);
    println!("{floor}, {basement_position}")
}
