use crate::lexer::position::Position;
use crate::lexer::token::{Op, Token, KEYWORDS};
use std::iter::Peekable;
use std::str::Chars;
use unicode_segmentation::UnicodeSegmentation;
use Op::*;

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    position: Position,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        let position = Position::new();
        let chars = input.chars().peekable();
        Lexer { position, chars }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        self.chars.next().and_then(|char| match char {
            ' ' => self.whitespace(),
            '\n' | '\r' => self.line_break(char),
            ';' | '(' | ')' | '[' | ']' | ',' => Some(self.single_char_token(char)),
            '0'..='9' => Some(self.number(char)),
            '"' => Some(self.string()),
            '+' | '-' | '*' | '/' => Some(self.operator(char)),
            '=' => Some(self.equal_sign()),
            _ => Some(self.word(char)),
        })
    }
}

impl<'a> Lexer<'a> {
    fn whitespace(&mut self) -> Option<Token> {
        self.position.column += 1;
        self.next()
    }

    fn line_break(&mut self, char: char) -> Option<Token> {
        //Handle CRLF
        if char == '\r' {
            self.chars.next_if(|c| *c == '\n');
        }
        self.position.line += 1;
        self.position.column = 1;
        self.next()
    }

    fn single_char_token(&mut self, char: char) -> Token {
        let position = self.position;
        self.position.column += 1;
        match char {
            ';' => Token::Semicolon(position),
            '(' => Token::LParen(position),
            ')' => Token::RParen(position),
            '[' => Token::LBracket(position),
            ']' => Token::RBracket(position),
            ',' => Token::Comma(position),
            _ => unreachable!(),
        }
    }

    fn number(&mut self, char: char) -> Token {
        let position = self.position;

        let mut num = char.to_string();
        while let Some(c) = self.chars.next_if(|c| c.is_ascii_digit()) {
            num.push(c);
        }

        if let Some('.') = self.chars.peek() {
            num.push(self.chars.next().unwrap());
            while let Some(c) = self.chars.next_if(|c| c.is_ascii_digit()) {
                num.push(c);
            }
            self.position.column += num.graphemes(true).count() as i32;
            return Token::Float(num.parse().unwrap(), position);
        }
        self.position.column += num.graphemes(true).count() as i32;

        Token::Int(num.parse().unwrap(), position)
    }

    fn string(&mut self) -> Token {
        let position = self.position;
        let mut res = String::new();
        while let Some(c) = self.chars.next_if(|c| *c != '"') {
            res.push(c);
        }
        match self.chars.next() {
            Some('"') => {
                self.position.column += res.graphemes(true).count() as i32 + 2;
                Token::String(res, position)
            }
            _ => Token::InvalidToken(position),
        }
    }

    fn operator(&mut self, char: char) -> Token {
        let position = self.position;
        self.position.column += 1;
        let op = match char {
            '+' => ADD,
            '-' => SUB,
            '*' => MUL,
            '/' => DIV,
            _ => unreachable!(),
        };

        // Check for assignment operators like +=, -=, *=, /=
        match self.chars.next_if(|c| *c == '=') {
            Some(_) => {
                self.position.column += 1;
                Token::Assignment(Some(op), position)
            }
            None => Token::Op(op, position),
        }
    }

    fn word(&mut self, char: char) -> Token {
        let position = self.position;
        let mut word = char.to_string();
        while let Some(c) = self.chars.next_if(|c| is_valid_variable_char(*c)) {
            word.push(c);
        }
        self.position.column += word.graphemes(true).count() as i32;

        match KEYWORDS.get(word.as_str()) {
            Some(keyword) => Token::Keyword(*keyword, position),
            None => Token::Identifier(word, position),
        }
    }

    fn equal_sign(&mut self) -> Token {
        let position = self.position;
        self.position.column += 1;
        match self.chars.next_if(|c| *c == '=') {
            Some(_) => {
                self.position.column += 1;
                Token::Op(EQ, position)
            }
            None => Token::Assignment(None, position),
        }
    }
}

// A variable can contain any character that is not a reserved character
fn is_valid_variable_char(c: char) -> bool {
    let reserved_chars = [
        '+', '-', '*', '/', '=', '(', ')', ';', '\n', '\r', ' ', '"', '[', ']', ',',
    ];
    !reserved_chars.contains(&c)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::token::Keyword;

    #[test]
    fn int() {
        let mut l = Lexer::new("234");
        assert_eq!(l.next().unwrap(), Token::Int(234, Position::new()));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn float() {
        let mut l = Lexer::new("234.567");
        assert_eq!(l.next().unwrap(), Token::Float(234.567, Position::new()));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn operator() {
        let mut l = Lexer::new("+");
        assert_eq!(l.next().unwrap(), Token::Op(ADD, Position::new()));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn assignment_operator() {
        let mut l = Lexer::new("+=");
        assert_eq!(
            l.next().unwrap(),
            Token::Assignment(Some(ADD), Position::new())
        );
        assert_eq!(l.next(), None);
    }

    #[test]
    fn identifier() {
        let mut l = Lexer::new("abc");
        assert_eq!(
            l.next().unwrap(),
            Token::Identifier("abc".to_string(), Position::new())
        );
        assert_eq!(l.next(), None);
    }

    #[test]
    fn identifier_with_number() {
        let mut l = Lexer::new("abc123");
        assert_eq!(
            l.next().unwrap(),
            Token::Identifier("abc123".to_string(), Position::new())
        );
        assert_eq!(l.next(), None);
    }

    #[test]
    fn identifier_with_japanese() {
        let mut l = Lexer::new("あいうえお");
        assert_eq!(
            l.next().unwrap(),
            Token::Identifier("あいうえお".to_string(), Position::new())
        );
        assert_eq!(l.next(), None);
    }

    #[test]
    fn keyword() {
        let mut l = Lexer::new("if");
        assert_eq!(
            l.next().unwrap(),
            Token::Keyword(Keyword::IF, Position::new())
        );
        assert_eq!(l.next(), None);
    }
}
