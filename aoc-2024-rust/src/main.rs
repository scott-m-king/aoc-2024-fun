#![allow(unused_variables)]
#![allow(dead_code)]
mod day9;
mod utils;

fn main() {
    let arg = utils::load_file("data/day-9.txt");
    println!("Part 1: {}", day9::part1(&arg));
    println!("Part 2: {}", day9::part2(&arg));
}
