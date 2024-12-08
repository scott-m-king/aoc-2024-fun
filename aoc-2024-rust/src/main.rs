#![allow(unused_variables)]
#![allow(dead_code)]
mod utils;
mod day3;

fn main() {
    let arg = utils::load_file("data/day-3.txt");
    println!("Part 1: {}", day3::part1(&arg));
    println!("Part 2: {}", day3::part2(&arg));
}