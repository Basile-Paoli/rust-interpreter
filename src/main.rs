use crate::lexer::Lexer;
use crate::lexer::TokenKind::EOF;

mod lexer;
mod parser;

fn main() {
    println!("Hello, world!");
    let mut l = Lexer::new(
        "
a 32
b 5
if a
b 3
else
b 4
",
    );
    loop {
        let token = l.next_token();
        println!("{:?}", token);
        if token.kind == EOF {
            break;
        }
    }
}
