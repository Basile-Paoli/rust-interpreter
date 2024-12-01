use std::fmt::Display;

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Position {
    pub line: i32,
    pub column: i32,
}

impl Position {
    pub fn new() -> Position {
        Position { line: 1, column: 1 }
    }
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line: {}, column: {}", self.line, self.column)
    }
}
