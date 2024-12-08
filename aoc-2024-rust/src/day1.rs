fn parse_input(input: &str) -> (Vec<i32>, Vec<i32>) {
    input
        .split("\n")
        .filter_map(|x| {
            let mut nums = x.split_whitespace().filter_map(|y| y.parse::<i32>().ok());
            Some((nums.next()?, nums.next()?))
        })
        .unzip()
}

pub fn part1(input: &str) -> i32 {
    let (mut left, mut right) = parse_input(input);
    left.sort();
    right.sort();
    left.iter()
        .zip(right.iter())
        .map(|(l, r)| (l - r).abs())
        .sum()
}

pub fn part2(input: &str) -> i32 {
    let (left, right) = parse_input(input);
    left.iter().fold(0, |acc, curr| {
        let counts= right.iter().filter(|y| **y == *curr).count() as i32;
        acc + (counts * curr)
    })
}
