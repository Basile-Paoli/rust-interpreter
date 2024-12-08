use std::env;
use std::fs::File;
use std::io::Read;

mod error;
mod interpreter;
mod lexer;
mod parser;

fn main() {
    let args = env::args().collect::<Vec<_>>();
    let input_file = args.get(1).map(|s| s.as_str()).unwrap_or("input.txt");
    let input = read_input(input_file).unwrap_or_else(|e| {
        eprintln!("Error reading input file: {}", e);
        std::process::exit(1);
    });

    let mut p = parser::Parser::new(&input);
    let tokens = p.lexer.clone().collect::<Vec<_>>();
    for token in tokens {
        println!("{}", token);
    }

    let instructions = p.parse().unwrap_or_else(|e| {
        eprintln!("{}", e);
        std::process::exit(1);
    });
    for instruction in &instructions {
        print!("{}", instruction);
    }

    let mut i = interpreter::Interpreter::new();
    i.run(instructions).unwrap_or_else(|e| {
        eprintln!("{}", e);
        std::process::exit(1);
    });
}

fn read_input(file_name: &str) -> Result<String, std::io::Error> {
    let mut file = File::open(file_name)?;
    let mut input = String::new();
    file.read_to_string(&mut input)?;
    Ok(input)
}
