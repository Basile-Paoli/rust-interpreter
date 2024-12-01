#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Position {
    pub line: i32,
    pub column: i32,
}

impl Position {
    pub fn new() -> Self {
        Position { line: 1, column: 1 }
    }
}
