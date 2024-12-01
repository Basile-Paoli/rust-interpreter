use crate::lexer::Lexer;

mod lexer;

fn main() {
    println!("Hello, world!");
    let mut l = Lexer::new("a 32 な");
    for res in l {
        match res {
            Ok(token) => println!("{}", token),
            Err(e) => println!("{}", e),
        }
    }
}
