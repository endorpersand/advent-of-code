fn main() {
    let input = std::fs::read_to_string("inputs/07.txt").unwrap();
    soln1(&input);
}

struct Data {
    inputs: Vec<isize>,
    output: isize
}
#[allow(dead_code)]
fn soln1(input: &str) {
    fn data_possible(data: &Data) -> bool {
        let mut possible_paths = vec![data.output];
        for &input in data.inputs.iter().rev() {
            for output in std::mem::take(&mut possible_paths) {
                if output % input == 0 { possible_paths.push(output / input); }
                possible_paths.push(output - input);
            }
        }
        
        possible_paths.into_iter().any(|o| o == 0)
    }
    fn data_possible2(data: &Data) -> bool {
        let mut possible_paths = vec![data.output];
        for &input in data.inputs.iter().rev() {
            for output in std::mem::take(&mut possible_paths) {
                let digits = input.checked_ilog10().unwrap_or(0) + 1;
                if output % 10isize.pow(digits) == input { possible_paths.push(output / 10isize.pow(digits)); }
                if output % input == 0 { possible_paths.push(output / input); }
                possible_paths.push(output - input);
            }
        }
        
        possible_paths.into_iter().any(|o| o == 0)
    }

    let data: Vec<_> = input.lines().map(|line| {
        let (output_str, inputs_str) = line.split_once(": ").unwrap();
        let output = output_str.parse().unwrap();
        let inputs = inputs_str.split(' ').map(|i| i.parse().unwrap()).collect();

        Data { inputs, output }
    })
    .collect();

    let (possible, not_possible): (Vec<_>, _) = data.iter().partition(|d| data_possible(d));

    let p1: isize = possible.into_iter()
        .map(|d| d.output)
        .sum();
    println!("{p1}");

    let p2p: isize = not_possible.into_iter()
        .filter(|d| data_possible2(d))
        .map(|d| d.output)
        .sum();
    let p2 = p1 + p2p;
    println!("{p2}");
}