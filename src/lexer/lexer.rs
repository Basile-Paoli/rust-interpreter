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
        assert_eq!(l.next().unwrap().kind, Number(234));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn operator() {
        let mut l = Lexer::new("+");
        assert_eq!(l.next().unwrap().kind, Op(ADD));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn assignment_operator() {
        let mut l = Lexer::new("+=");
        assert_eq!(l.next().unwrap().kind, Assignment(Some(ADD)));
        assert_eq!(l.next(), None);
    }

    #[test]
    fn identifier() {
        let mut l = Lexer::new("abc");
        assert_eq!(
            l.next().unwrap(),
            Token {
                kind: Identifier("abc".to_string()),
                position: Position::new()
            }
        );
        assert_eq!(l.next(), None);
    }

    #[test]
    fn keyword() {
        let mut l = Lexer::new("if");
        assert_eq!(
            l.next().unwrap(),
            Token {
                kind: Keyword(Keyword::IF),
                position: Position::new()
            }
        );
        assert_eq!(l.next(), None);
    }

    #[test]
    fn unknown_token() {
        let mut l = Lexer::new("命");
        assert_eq!(
            l.next().unwrap(),
            Token {
                kind: Unknown('命'),
                position: Position::new()
            }
        );
    }
}
