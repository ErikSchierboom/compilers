use std::io::Read;

fn main() {
    let code = "++       Cell c0 = 2
> +++++  Cell c1 = 5

[        Start your loops with your cell pointer on the loop counter (c1 in our case)
< +      Add 1 to c0
> -      Subtract 1 from c1
]        End your loops with the cell pointer on the loop counter

At this point our program has added 5 to 2 leaving 7 in c0 and 0 in c1
but we cannot output this value to the terminal since it is not ASCII encoded

To display the ASCII character '7' we must add 48 to the value 7
We use a loop to compute 48 = 6 * 8

++++ ++++  c1 = 8 and this will be our loop counter again
[
< +++ +++  Add 6 to c0
> -        Subtract 1 from c1
]
< .        Print out c0 which has the value 55 which translates to '7'!";

    let instructions = code.as_bytes();
    let mut instruction_pointer = 0usize;
    let mut data = [0u8; 100];
    let mut data_pointer = 0usize;
    let mut stdin = std::io::stdin();

    while instruction_pointer < instructions.len() {
        match instructions[instruction_pointer] {
            b'>' => data_pointer += 1,
            b'<' => data_pointer -= 1,
            b'+' => data[data_pointer] += 1,
            b'-' => data[data_pointer] -= 1,
            b'.' => print!("{}", data[data_pointer] as char),
            b',' => stdin.read_exact(&mut data[data_pointer..=data_pointer]).unwrap(),
            b'[' => {
                if data[data_pointer] == 0 {
                    while instruction_pointer < instructions.len() && instructions[instruction_pointer] != b']' {
                        instruction_pointer += 1
                    }
                }
            },
            b']' => {
                if data[data_pointer] != 0 {
                    while instruction_pointer < instructions.len() && instructions[instruction_pointer] != b'[' {
                        instruction_pointer -= 1
                    }
                }
            }
            _ => {}
        }

        instruction_pointer += 1
    }
}
