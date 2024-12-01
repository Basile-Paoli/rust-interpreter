use crate::lexer::Lexer;

mod lexer;

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
    for res in l {
        match res {
            Ok(token) => println!("{}", token),
            Err(e) => println!("{}", e),
        }
    }
}
