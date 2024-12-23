#[derive(Debug)]
struct Position {
    id: u32,
    start: usize,
    file_size: usize,
    free_start: usize,
    free_space: usize,
    items: Vec<u32>,
}

fn parse_input(input: &str) -> Vec<Position> {
    input
        .chars()
        .map(|x| x.to_digit(10).unwrap_or(0) as usize)
        .collect::<Vec<usize>>()
        .chunks_exact(2)
        .enumerate()
        .map(|(i, x)| (i as u32, x[0], x[1]))
        .fold(Vec::new(), |mut acc, (id, file_size, free_space)| {
            match acc.as_slice() {
                [] => {
                    acc.push(Position {
                        id,
                        start: 0,
                        file_size,
                        free_start: file_size,
                        free_space,
                        items: vec![id; file_size],
                    });
                }
                [.., last] => {
                    let last: &Position = acc.last().unwrap();
                    let start = last.start + last.file_size + last.free_space;
                    acc.push(Position {
                        id,
                        start,
                        file_size,
                        free_start: start + file_size,
                        free_space,
                        items: vec![id; file_size],
                    });
                }
            };
            acc
        })
}

fn solve(left: &mut [Position], right: &mut [Position]) {
    if let (Some(first), Some(second)) = (left.first_mut(), right.last_mut()) {
        while first.free_space > 0 && !second.items.is_empty() {
            first.items.push(second.items.pop().unwrap());
            first.free_space -= 1;
        }

        let (next_start, next_end) = match (first.free_space, &second.items) {
            (x, y) if x > 0 && y.is_empty() => (left, right.split_at_mut(right.len() - 1).0),
            (x, y) if x == 0 && !y.is_empty() => (&mut left[1..], right),
            (x, y) => (&mut left[1..], right.split_at_mut(right.len() - 1).0),
        };
        solve(next_start, next_end);
    }
}

// 6287922469061 too low
// 6291345473089 too high
pub fn part1(input: &str) -> u64 {
    let mut parsed = parse_input(input);
    let len = parsed.len();
    let (left, right) = parsed.split_at_mut(len / 2);
    solve(left, right);

    parsed
        .iter()
        .flat_map(|Position { items, .. }| items.iter().copied())
        .enumerate()
        .fold(0, |acc, (i, curr)| acc + (i as u32 * curr) as u64)
}

pub fn part2(input: &str) -> u32 {
    0
}
