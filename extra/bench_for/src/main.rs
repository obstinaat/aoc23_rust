const COUNT:usize = 1_000_000_000;

use std::time::Instant;

fn main() {
    let start_time = Instant::now();
    let mut i = 0;
    while i < COUNT{
        i+=1;
    }

    let end_time = Instant::now();
    let elapsed_time = end_time - start_time;
    let milliseconds = elapsed_time.subsec_millis();
    println!("Elapsed time: {} milliseconds", milliseconds);

}
