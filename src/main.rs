use crate::parser::Parser;
mod error;
mod interpreter;
mod lexer;
mod parser;

fn main() {
    println!("Hello, world!");
    let mut p = Parser::new(
        "a = (1 + 2) * 3;
        b = a + 3;
        b += 1;
        c = b + 4;
        c;",
    );
    let tokens = p.lexer.clone().collect::<Vec<_>>();
    for token in tokens {
        println!("{}", token);
    }

    let instructions = p.parse().unwrap();
    for instruction in &instructions {
        println!("{}", instruction);
    }

    let mut i = interpreter::Interpreter::new();
    i.run(instructions).unwrap()
}
