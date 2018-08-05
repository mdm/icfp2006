use std::env;
use std::fs::File;
use std::io::Read;
use std::io::Write;
use std::num::Wrapping;
use std::process;


fn main() {
    let filename = parse_args(env::args()).unwrap_or_else(|err| {
        println!("{}", err);
        process::exit(1);
    });

    let program = match load_program(&filename) {
        Ok(program) => program,
        Err(err) => {
            println!("Could not load program: {}", err);
            process::exit(1);
        }
    };

    run_program(program);   
}

fn parse_args(mut args: env::Args) -> Result<String, &'static str> {
    args.next();

    let filename = match args.next() {
        Some(arg) => arg,
        None => return Err("The file to execute must be passed as the first parameter."),
    };

    Ok(filename)
}

fn load_program(filename: &str) -> Result<Vec<u32>, &'static str> {
    let file = match File::open(filename) {
        Ok(file) => file,
        Err(_) => return Err("Error opening file."),
    };
    let mut program = Vec::new();

    let mut success = false;
    let mut platter: u32 = 0;

    for (pos, byte) in (0..4).cycle().zip(file.bytes()) {
        if let Ok(value) = byte {
            platter = if pos == 3 {
                success = true;
                program.push(platter + value as u32);
                    0
            } else {
                success = false;
                (platter + value as u32) << 8
            }
        } else {
            return Err("Error reading file.");
        }
    }

    if success {
        Ok(program)
    } else {
        Err("Invalid program size.")
    }
}

fn run_program(program: Vec<u32>) {
    let mut registers: [u32; 8] = [0, 0, 0 ,0 ,0 ,0 ,0 ,0];
    let mut heap: Vec<Option<Vec<u32>>> = Vec::new();
    let mut freelist: Vec<usize> = Vec::new();
    let mut finger: usize = 0;

    heap.push(Some(program));

    loop {
        let next_platter = get_next_platter(&heap, finger);
        let operator = next_platter >> 28 as u32;
        let register_a: usize = ((next_platter >> 6) & 7 as u32) as usize;
        let register_b: usize = ((next_platter >> 3) & 7 as u32) as usize;
        let register_c: usize = (next_platter & 7 as u32) as usize;

        match operator {
            0 => if registers[register_c] != 0 {
                registers[register_a] = registers[register_b]
            },
            1 => {
                let source_array = &heap[registers[register_b] as usize];
                let source_platter = match source_array {
                    Some(raw_array) => &raw_array[registers[register_c] as usize],
                    None => panic!("Tried to read from invalid array. Halting!"),
                };
                registers[register_a] = *source_platter;
            },
            2 => {
                let destination_array = &mut heap[registers[register_a] as usize];
                match destination_array {
                    Some(raw_array) => raw_array[registers[register_b] as usize] = registers[register_c],
                    None => panic!("Tried to write to invalid array. Halting!"),
                }
            },
            3 => registers[register_a] = (Wrapping(registers[register_b]) + Wrapping(registers[register_c])).0,
            4 => registers[register_a] = (Wrapping(registers[register_b]) * Wrapping(registers[register_c])).0,
            5 => registers[register_a] = registers[register_b] / registers[register_c],
            6 => registers[register_a] = !(registers[register_b] & registers[register_c]),
            7 => break,
            8 => {
                let new_array = vec![0_u32; registers[register_c] as usize];
                match freelist.pop() {
                    Some(free_index) => {
                        heap[free_index] = Some(new_array);
                        registers[register_b] = free_index as u32;
                    },
                    None => {
                        heap.push(Some(new_array));
                        registers[register_b] = heap.len() as u32 - 1;
                    },
                }
            },
            9 => {
                heap[registers[register_c] as usize] = None;
                freelist.push(registers[register_c] as usize);
            },
            10 => {
                std::io::stdout().write(&[registers[register_c] as u8]).unwrap();
            },
            11 => {
                let mut buffer: [u8; 1] = [0];
                std::io::stdin().read(&mut buffer).unwrap();
                registers[register_c] = buffer[0] as u32;
            },
            12 => {
                if registers[register_b] > 0 {
                    heap[0] = clone_array(&heap, registers[register_b] as usize);
                }
                finger = registers[register_c] as usize;
            },
            13 => {
                let register_a = ((next_platter >> 25) & 7 as u32) as usize;
                registers[register_a] = next_platter & 0x1ffffff;
            },
            _ => panic!("Invalid operator. Halting!"),
        }

        if operator != 12 {
            finger += 1;
        }
    }
}

fn get_next_platter(heap: &Vec<Option<Vec<u32>>>, finger: usize) -> u32 {
    return match &heap[0] {
        Some(raw_array) => raw_array[finger],
        None => panic!("Cannot read program from memory. Halting!"),
    };
}

fn clone_array(heap: &Vec<Option<Vec<u32>>>, array: usize) -> Option<Vec<u32>> {
    return match &heap[array] {
        Some(raw_array) => Some(raw_array.clone()),
        None => panic!("Tried to copy invalid array. Halting!"),
    };
}