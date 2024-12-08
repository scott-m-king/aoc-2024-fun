use regex::Regex;

fn parse_input(input: &str) -> Vec<(i32, i32)> {
    Regex::new(r"mul\(([0-9]{1,3}),([0-9]{1,3})\)")
        .unwrap()
        .find_iter(input)
        .map(|m| m.as_str())
        .map(|line| {
            if let (Some(start), Some(end)) = (line.find("("), line.find(")")) {
                let result: Vec<&str> = line[start + 1..end].split(",").collect();
                return (result[0].parse().unwrap(), result[1].parse().unwrap());
            }
            (0, 0)
        })
        .collect::<Vec<(i32, i32)>>()
}

pub fn part1(input: &str) -> i32 {
    parse_input(input).iter().map(|(l, r)| l * r).sum()
}

fn get_by_regex<'a>(re: &'a str, input: &'a str, action: &'a str) -> Vec<(&'a str, usize)> {
    Regex::new(re)
        .unwrap()
        .find_iter(input)
        .map(|m| (action, m.start()))
        .collect::<Vec<(&str, usize)>>()
}

pub fn part2(input: &str) -> i32 {
    let mut activations = get_by_regex(r"do\(\)", input, "activate");
    let deactivations = get_by_regex(r"don't\(\)", input, "deactivate");

    activations.insert(0, ("activate".into(), 0));
    activations.extend(deactivations);
    activations.sort_by(|(_, a), (_, b)| a.cmp(b));

    let mut curr = "start";
    let mut filtered: Vec<usize> = activations
        .iter()
        .filter_map(|(action, idx)| {
            if curr == "start" || *action != curr {
                curr = action;
                Some(*idx)
            } else {
                None
            }
        })
        .collect();

    filtered.push(input.len() - 1);

    filtered
        .windows(2)
        .step_by(2)
        .flat_map(|w| parse_input(&input[w[0]..w[1]]))
        .map(|(l, r)| l * r)
        .sum()
}
