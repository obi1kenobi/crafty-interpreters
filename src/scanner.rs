#![allow(dead_code)]

use std::{rc::Rc, str::CharIndices};

use phf::phf_map;

use crate::{
    token::{Keyword, Token},
    util::MultiPeekable,
};

static KEYWORDS: phf::Map<&'static str, Keyword> = phf_map! {
    "and" => Keyword::And,
    "class" => Keyword::Class,
    "else" => Keyword::Else,
    "for" => Keyword::For,
    "fun" => Keyword::Fun,
    "if" => Keyword::If,
    "or" => Keyword::Or,
    "print" => Keyword::Print,
    "return" => Keyword::Return,
    "super" => Keyword::Super,
    "this" => Keyword::This,
    "var" => Keyword::Var,
    "while" => Keyword::While,
};

#[derive(Debug, Clone)]
pub struct Scanner<'a> {
    source: &'a str,
    position: MultiPeekable<CharIndices<'a>, 2>,
    line_number: usize,
    finished: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            position: MultiPeekable::new(source.char_indices()),
            line_number: 1,
            finished: false,
        }
    }

    /// Attempt to match against the next character. If it matched, consume it
    /// and return a slice from the given start index up to and including the matched char.
    fn match_char(&mut self, expected: char, start_index: usize) -> Option<&'a str> {
        self.position.peek().and_then(|(index, found_char)| {
            if found_char == expected {
                let (next_index, next_char) = self.position.next().unwrap();
                assert_eq!(next_index, index);
                assert_eq!(next_char, found_char);

                // Get the next character's index, if any, and use it to slice.
                match self.position.peek() {
                    Some((boundary_idx, _)) => Some(&self.source[start_index..boundary_idx]),
                    None => Some(&self.source[start_index..]),
                }
            } else {
                None
            }
        })
    }

    fn advance_until(&mut self, predicate: impl Fn(char) -> bool, start_index: usize) -> &'a str {
        loop {
            match self.position.peek() {
                Some((index, c)) if predicate(c) => {
                    break &self.source[start_index..index];
                }
                Some((_, c)) => {
                    if c == '\n' {
                        self.line_number += 1;
                    }
                    self.position.next().unwrap();
                }
                None => break &self.source[start_index..],
            }
        }
    }

    fn new_lexeme(&self, token: Token, content: &'a str) -> Lexeme<'a> {
        Lexeme::new(token, content, self.line_number)
    }

    fn new_identifier(&self, content: &'a str) -> Lexeme<'a> {
        let token = KEYWORDS
            .get(content)
            .map(|keyword| Token::Keyword(*keyword))
            .unwrap_or_else(|| match content {
                // Handle literals that look like keywords, and handle identifiers.
                "nil" => Token::Nil,
                "false" => Token::False,
                "true" => Token::True,
                identifier => Token::Identifier(Rc::new(String::from(identifier))),
            });
        self.new_lexeme(token, content)
    }

    fn new_comment(&mut self) -> Lexeme<'a> {
        let content = {
            match self.position.peek() {
                Some((comment_start_index, _)) => {
                    let content = self.advance_until(|c| c == '\n', comment_start_index);

                    // advance_until() doesn't munch the newline, so we need to munch it now.
                    // There might not be a newline if we are at the end of the file, though.
                    if let Some((_, c)) = self.position.next() {
                        assert!(c == '\n');
                        self.line_number += 1;
                    }
                    content
                }
                None => {
                    // The comment started right at the end of the file,
                    // so it has no content.
                    ""
                }
            }
        };
        self.new_lexeme(Token::Comment(Rc::new(content.to_string())), content)
    }

    fn new_string_literal(&mut self, opening_quote_index: usize) -> Lexeme<'a> {
        let content = self.advance_until(|c| c == '"', opening_quote_index);

        // Check if we found the closing quote or not.
        match self.position.next() {
            Some((_, '"')) => {
                // The content doesn't include the closing quote, if one exists.
                let string_value = &content[1..];
                self.new_lexeme(
                    Token::String(Rc::new(string_value.to_string())),
                    string_value,
                )
            }
            None => self.new_lexeme(Token::UnterminatedString, content),
            _ => unreachable!(),
        }
    }

    fn new_number_literal(&mut self, start_index: usize) -> Lexeme<'a> {
        let mut content = self.advance_until(|c| !matches!(c, '0'..='9'), start_index);

        let peek_two = (self.position.peek(), self.position.peek_nth(1));
        if let (Some((_, '.')), Some((_, '0'..='9'))) = peek_two {
            // Found a decimal point and a digit behind it, continue advancing.
            let (_, dot) = self.position.next().unwrap(); // consume the '.'
            assert_eq!(dot, '.');
            content = self.advance_until(|c| !matches!(c, '0'..='9'), start_index);
        }

        let number = content.parse();
        match number {
            Ok(number) => self.new_lexeme(Token::Number(number), content),
            Err(_) => self.new_lexeme(Token::InvalidNumber, content),
        }
    }
}

impl<'a> Iterator for Scanner<'a> {
    type Item = Lexeme<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        loop {
            let maybe_next = self.position.next();
            if let Some((start_index, start_char)) = maybe_next {
                let content = &self.source[start_index..=start_index];
                let maybe_lexeme = match start_char {
                    '(' => Some(self.new_lexeme(Token::LeftParen, content)),
                    ')' => Some(self.new_lexeme(Token::RightParen, content)),
                    '{' => Some(self.new_lexeme(Token::LeftBrace, content)),
                    '}' => Some(self.new_lexeme(Token::RightBrace, content)),
                    ',' => Some(self.new_lexeme(Token::Comma, content)),
                    '.' => Some(self.new_lexeme(Token::Dot, content)),
                    '-' => Some(self.new_lexeme(Token::Minus, content)),
                    '+' => Some(self.new_lexeme(Token::Plus, content)),
                    ';' => Some(self.new_lexeme(Token::Semicolon, content)),
                    '*' => Some(self.new_lexeme(Token::Star, content)),
                    '!' => {
                        if let Some(content) = self.match_char('=', start_index) {
                            Some(self.new_lexeme(Token::BangEqual, content))
                        } else {
                            Some(self.new_lexeme(Token::Bang, content))
                        }
                    }
                    '=' => {
                        if let Some(content) = self.match_char('=', start_index) {
                            Some(self.new_lexeme(Token::EqualEqual, content))
                        } else {
                            Some(self.new_lexeme(Token::Equal, content))
                        }
                    }
                    '<' => {
                        if let Some(content) = self.match_char('=', start_index) {
                            Some(self.new_lexeme(Token::LessEqual, content))
                        } else {
                            Some(self.new_lexeme(Token::Less, content))
                        }
                    }
                    '>' => {
                        if let Some(content) = self.match_char('=', start_index) {
                            Some(self.new_lexeme(Token::GreaterEqual, content))
                        } else {
                            Some(self.new_lexeme(Token::Greater, content))
                        }
                    }
                    '/' => {
                        if self.match_char('/', start_index).is_some() {
                            Some(self.new_comment())
                        } else {
                            Some(self.new_lexeme(Token::Slash, content))
                        }
                    }
                    '"' => Some(self.new_string_literal(start_index)),
                    '0'..='9' => Some(self.new_number_literal(start_index)),
                    'a'..='z' | 'A'..='Z' | '_' => {
                        let content = self.advance_until(
                            |c| !matches!(c, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'),
                            start_index,
                        );
                        Some(self.new_identifier(content))
                    }
                    ' ' | '\r' | '\t' => None, // ignore whitespace chars
                    '\n' => {
                        self.line_number += 1;
                        None
                    }
                    _ => Some(self.new_lexeme(Token::Unknown, content)),
                };

                // If we found a lexeme, return it. If not, keep looking.
                if let Some(lexeme) = maybe_lexeme {
                    break Some(lexeme);
                }
            } else {
                let eof_lexeme = Lexeme::new(Token::Eof, "", self.line_number);
                self.finished = true;
                break Some(eof_lexeme);
            }
        }
    }
}

pub struct Lexeme<'a> {
    pub token: Token,
    pub content: &'a str,
    pub line: usize,
}

impl<'a> Lexeme<'a> {
    fn new(token: Token, content: &'a str, line: usize) -> Self {
        Self {
            token,
            content,
            line,
        }
    }
}
