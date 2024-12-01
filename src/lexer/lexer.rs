use crate::lexer::position::Position;
use crate::lexer::token::{Keyword, Op, Token, TokenKind, KEYWORDS};
use std::fmt::Display;
use std::iter::Peekable;
use std::str::Chars;
use unicode_segmentation::UnicodeSegmentation;
use Op::*;
use TokenKind::*;

pub struct Lexer<'a> {
    chars: Peekable<Chars<'a>>,
    position: Position,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct LexerError {
    pub position: Position,
    pub character: char,
}

impl Display for LexerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Unknown character {} at {}",
            self.character, self.position
        )
    }
}
impl<'a> Lexer<'a> {
    pub fn new(input: &str) -> Lexer {
        let position = Position::new();
        let chars = input.chars().peekable();
        Lexer { position, chars }
    }

    pub fn next_token(&mut self) -> Result<Token, LexerError> {
        match self.chars.next() {
            Some(char) => match char {
                ' ' => self.whitespace(),
                '\n' | '\r' => self.line_break(char),
                '0'..='9' => Ok(self.number(char)),
                '+' | '-' | '*' | '/' => Ok(self.operator(char)),
                'a'..='z' | 'A'..='Z' | '_' => Ok(self.word(char)),
                '=' => Ok(self.equal_sign()),
                _ => self.error(char),
            },
            None => self.eof(),
        }
    }

    fn whitespace(&mut self) -> Result<Token, LexerError> {
        self.position.column += 1;
        self.next_token()
    }

    fn line_break(&mut self, char: char) -> Result<Token, LexerError> {
        //Handle CRLF
        if char == '\r' {
            self.chars.next_if(|c| *c == '\n');
        }
        self.position.line += 1;
        self.position.column = 1;
        self.next_token()
    }

    fn error(&self, character: char) -> Result<Token, LexerError> {
        Err(LexerError {
            character,
            position: self.position,
        })
    }

    fn eof(&self) -> Result<Token, LexerError> {
        Ok(Token {
            kind: EOF,
            position: self.position,
        })
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

impl Iterator for Lexer<'_> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Token { kind: EOF, .. }) => None,
            Ok(token) => Some(Ok(token)),
            Err(e) => Some(Err(e)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn number() {
        let mut l = Lexer::new("234");
        assert_eq!(l.next_token().unwrap().kind, Number(234));
        assert_eq!(l.next_token().unwrap().kind, EOF);
    }

    #[test]
    fn operator() {
        let mut l = Lexer::new("+");
        assert_eq!(l.next_token().unwrap().kind, Op(ADD));
        assert_eq!(l.next_token().unwrap().kind, EOF);
    }

    #[test]
    fn assignment_operator() {
        let mut l = Lexer::new("+=");
        assert_eq!(l.next_token().unwrap().kind, Assignment(Some(ADD)));
        assert_eq!(l.next_token().unwrap().kind, EOF);
    }

    #[test]
    fn identifier() {
        let mut l = Lexer::new("abc");
        assert_eq!(
            l.next_token(),
            Ok(Token {
                kind: Identifier("abc".to_string()),
                position: Position::new()
            })
        );
        assert_eq!(l.next_token().unwrap().kind, EOF);
    }

    #[test]
    fn keyword() {
        let mut l = Lexer::new("if");
        assert_eq!(
            l.next_token(),
            Ok(Token {
                kind: Keyword(Keyword::IF),
                position: Position::new()
            })
        );
        assert_eq!(l.next_token().unwrap().kind, EOF);
    }

    #[test]
    fn unknown_token() {
        let mut l = Lexer::new("命");
        assert_eq!(
            l.next_token(),
            Err(LexerError {
                character: '命',
                position: Position::new()
            })
        );
    }
}
