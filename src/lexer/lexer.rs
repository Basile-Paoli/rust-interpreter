use crate::lexer::position::Position;
use crate::lexer::token::{OpKind, Token, TokenKind};
use std::fmt::Display;
use std::iter::Peekable;
use std::str::Chars;

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
            "Unknown character {} at line: {}, column: {}",
            self.character, self.position.line, self.position.column
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
        match self.next_kind() {
            Ok(kind) => Ok(Token {
                position: self.position,
                kind,
            }),
            Err(e) => Err(e),
        }
    }

    pub fn next_kind(&mut self) -> Result<TokenKind, LexerError> {
        match self.chars.next() {
            Some(char) => match char {
                ' ' | '\n' | '\r' => self.next_kind(),
                '0'..='9' => Ok(self.number(char)),
                '+' | '-' | '*' | '/' => Ok(self.operator(char)),
                'a'..='z' | 'A'..='Z' | '_' => Ok(self.word(char)),
                _ => Err(LexerError {
                    position: self.position,
                    character: char,
                }),
            },
            None => Ok(TokenKind::EOF),
        }
    }

    fn number(&mut self, char: char) -> TokenKind {
        let mut num = char.to_string();
        while let Some(c) = self.chars.next_if(|c| c.is_ascii_digit()) {
            num.push(c);
        }
        TokenKind::NUMBER(num.parse().unwrap())
    }

    fn operator(&mut self, char: char) -> TokenKind {
        match char {
            '+' => TokenKind::OP(OpKind::ADD),
            '-' => TokenKind::OP(OpKind::SUB),
            '*' => TokenKind::OP(OpKind::MUL),
            '/' => TokenKind::OP(OpKind::DIV),
            _ => unreachable!(),
        }
    }

    fn word(&mut self, char: char) -> TokenKind {
        let mut word = char.to_string();
        while let Some(c) = self
            .chars
            .next_if(|c| c.is_ascii_alphanumeric() || *c == '_')
        {
            word.push(c);
        }
        TokenKind::IDENTIFIER(word)
    }
}

impl Iterator for Lexer<'_> {
    type Item = Result<Token, LexerError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.next_token() {
            Ok(Token {
                kind: TokenKind::EOF,
                ..
            }) => None,
            Ok(token) => Some(Ok(token)),
            Err(e) => Some(Err(e)),
        }
    }
}

#[cfg(test)]
mod test {
    use super::{Lexer, TokenKind};
    #[test]
    fn number() {
        let mut l = Lexer::new("234");
        assert_eq!(l.next_token().unwrap().kind, TokenKind::NUMBER(234));
        assert_eq!(l.next_token().unwrap().kind, TokenKind::EOF);
    }

    #[test]
    fn operator() {
        let mut l = Lexer::new("+");
        assert_eq!(
            l.next_token().unwrap().kind,
            TokenKind::OP(super::OpKind::ADD)
        );
        assert_eq!(l.next_token().unwrap().kind, TokenKind::EOF);
    }

    #[test]
    fn identifier() {
        let mut l = Lexer::new("abc");
        assert_eq!(
            l.next_token().unwrap().kind,
            TokenKind::IDENTIFIER("abc".to_string())
        );
        assert_eq!(l.next_token().unwrap().kind, TokenKind::EOF);
    }

    #[test]
    fn unknown_token() {
        let mut l = Lexer::new("命");
        assert!(matches!(
            l.next_token(),
            Err(super::LexerError {
                character: '命',
                ..
            })
        ));
    }
}
