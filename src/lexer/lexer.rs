use crate::lexer::position::Position;
use crate::lexer::token::{Keyword, Op, Token, KEYWORDS};
use std::iter::Peekable;
use std::str::Chars;
use unicode_segmentation::UnicodeSegmentation;
use Op::*;
use Token::*;

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
            '0'..='9' => Some(self.number(char)),
            '+' | '-' | '*' | '/' => Some(self.operator(char)),
            'a'..='z' | 'A'..='Z' | '_' => Some(self.word(char)),
            '=' => Some(self.equal_sign()),
            _ => Some(self.unknown_token(char)),
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

    fn unknown_token(&self, char: char) -> Token {
        Unknown(char, self.position)
    }

    fn number(&mut self, char: char) -> Token {
        let position = self.position;

        let mut num = char.to_string();
        while let Some(c) = self.chars.next_if(|c| c.is_ascii_digit()) {
            num.push(c);
        }
        self.position.column += num.graphemes(true).count() as i32;

        Number(num.parse().unwrap(), position)
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
                Assignment(Some(op), position)
            }
            None => Op(op, position),
        }
    }

    fn word(&mut self, char: char) -> Token {
        let position = self.position;
        let mut word = char.to_string();
        while let Some(c) = self
            .chars
            .next_if(|c| c.is_ascii_alphanumeric() || *c == '_')
        {
            word.push(c);
        }
        self.position.column += word.graphemes(true).count() as i32;

        match KEYWORDS.get(word.as_str()) {
            Some(keyword) => Keyword(*keyword, position),
            None => Identifier(word, position),
        }
    }

    fn equal_sign(&mut self) -> Token {
        let position = self.position;
        self.position.column += 1;
        match self.chars.next_if(|c| *c == '=') {
            Some(_) => {
                self.position.column += 1;
                Op(EQ, position)
            }
            None => Assignment(None, position),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn number() {
        let mut l = Lexer::new("234");
        assert_eq!(l.next().unwrap(), Number(234, Position::new()));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn operator() {
        let mut l = Lexer::new("+");
        assert_eq!(l.next().unwrap(), Op(ADD, Position::new()));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn assignment_operator() {
        let mut l = Lexer::new("+=");
        assert_eq!(l.next().unwrap(), Assignment(Some(ADD), Position::new()));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn identifier() {
        let mut l = Lexer::new("abc");
        assert_eq!(
            l.next().unwrap(),
            Identifier("abc".to_string(), Position::new())
        );
        assert_eq!(l.next(), None);
    }

    #[test]
    fn keyword() {
        let mut l = Lexer::new("if");
        assert_eq!(
            l.next().unwrap(),
            Keyword(Keyword::IF, Position::new())
        );
        assert_eq!(l.next(), None);
    }

    #[test]
    fn unknown_token() {
        let mut l = Lexer::new("命");
        assert_eq!(
            l.next().unwrap(),
            Unknown('命', Position::new())
        );
    }
}
