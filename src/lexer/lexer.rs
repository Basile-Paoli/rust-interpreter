use crate::lexer::position::Position;
use crate::lexer::token::{Keyword, Op, Token, TokenKind, KEYWORDS};
use std::iter::Peekable;
use std::str::Chars;
use unicode_segmentation::UnicodeSegmentation;
use Op::*;
use TokenKind::*;

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

    pub fn next_token(&mut self) -> Token {
        match self.chars.next() {
            Some(char) => match char {
                ' ' => self.whitespace(),
                '\n' | '\r' => self.line_break(char),
                '0'..='9' => self.number(char),
                '+' | '-' | '*' | '/' => self.operator(char),
                'a'..='z' | 'A'..='Z' | '_' => self.word(char),
                '=' => self.equal_sign(),
                _ => self.unknown_token(char),
            },
            None => self.eof(),
        }
    }

    pub fn peek_token(&mut self) -> Token {
        let mut peek = self.clone();
        peek.next_token()
    }

    fn whitespace(&mut self) -> Token {
        self.position.column += 1;
        self.next_token()
    }

    fn line_break(&mut self, char: char) -> Token {
        //Handle CRLF
        if char == '\r' {
            self.chars.next_if(|c| *c == '\n');
        }
        self.position.line += 1;
        self.position.column = 1;
        self.next_token()
    }

    fn eof(&self) -> Token {
        Token {
            kind: EOF,
            position: self.position,
        }
    }

    fn unknown_token(&self, char: char) -> Token {
        Token {
            kind: Unknown(char),
            position: self.position,
        }
    }

    fn number(&mut self, char: char) -> Token {
        let position = self.position;

        let mut num = char.to_string();
        while let Some(c) = self.chars.next_if(|c| c.is_ascii_digit()) {
            num.push(c);
        }
        self.position.column += num.graphemes(true).count() as i32;

        let kind = Number(num.parse().unwrap());
        Token { kind, position }
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
        let kind = match self.chars.next_if(|c| *c == '=') {
            Some(_) => {
                self.position.column += 1;
                Assignment(Some(op))
            }
            None => Op(op),
        };

        Token { kind, position }
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

        let kind = match KEYWORDS.get(word.as_str()) {
            Some(keyword) => Keyword(*keyword),
            None => Identifier(word),
        };
        Token { kind, position }
    }

    fn equal_sign(&mut self) -> Token {
        let position = self.position;
        self.position.column += 1;
        let kind = match self.chars.next_if(|c| *c == '=') {
            Some(_) => {
                self.position.column += 1;
                Op(EQ)
            }
            None => Assignment(None),
        };
        Token { kind, position }
    }
}
#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn number() {
        let mut l = Lexer::new("234");
        assert_eq!(l.next_token().kind, Number(234));
        assert_eq!(l.next_token().kind, EOF);
    }

    #[test]
    fn operator() {
        let mut l = Lexer::new("+");
        assert_eq!(l.next_token().kind, Op(ADD));
        assert_eq!(l.next_token().kind, EOF);
    }

    #[test]
    fn assignment_operator() {
        let mut l = Lexer::new("+=");
        assert_eq!(l.next_token().kind, Assignment(Some(ADD)));
        assert_eq!(l.next_token().kind, EOF);
    }

    #[test]
    fn identifier() {
        let mut l = Lexer::new("abc");
        assert_eq!(
            l.next_token(),
            Token {
                kind: Identifier("abc".to_string()),
                position: Position::new()
            }
        );
        assert_eq!(l.next_token().kind, EOF);
    }

    #[test]
    fn keyword() {
        let mut l = Lexer::new("if");
        assert_eq!(
            l.next_token(),
            Token {
                kind: Keyword(Keyword::IF),
                position: Position::new()
            }
        );
        assert_eq!(l.next_token().kind, EOF);
    }

    #[test]
    fn unknown_token() {
        let mut l = Lexer::new("命");
        assert_eq!(
            l.next_token(),
            Token {
                kind: Unknown('命'),
                position: Position::new()
            }
        );
    }
}
