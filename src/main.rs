use crate::lexer::Lexer;

mod lexer;
mod parser;

fn main() {
    println!("Hello, world!");
    let l = Lexer::new(
        "
a 32
b 5
if a
b 3
else
b 4
",
    );
    for token in l {
        println!("{:?}", token);
    }
}
