#![allow(dead_code)]

use std::{str::{CharIndices}, rc::Rc};

use crate::{token::{Token, Keyword}, util::MultiPeekable};

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
    /// and return Some(Some(index)) where the index is the byte offset of
    /// the _subsequent_ character, or Some(None) if the consumed character
    /// was the last one in the string.
    fn match_char(&mut self, expected: char) -> Option<Option<usize>> {
        self.position.peek().and_then(|(index, found_char)| {
            if found_char == expected {
                let (next_index, next_char) = self.position.next().unwrap();
                assert_eq!(next_index, index);
                assert_eq!(next_char, found_char);

                // Get the next character's index, if any, and return it.
                Some(self.position.peek().map(|(idx, _)| idx))
            } else {
                None
            }
        })
    }

    fn new_lexeme_on_this_line(&self, token: Token, content: &'a str) -> Lexeme<'a> {
        Lexeme::new(token, content, self.line_number)
    }

    fn new_identifier(&self, content: &'a str) -> Lexeme<'a> {
        let token = match content {
            "and" => Token::Keyword(Keyword::And),
            "class" => Token::Keyword(Keyword::Class),
            "else" => Token::Keyword(Keyword::Else),
            "false" => Token::False,
            "for" => Token::Keyword(Keyword::For),
            "fun" => Token::Keyword(Keyword::Fun),
            "if" => Token::Keyword(Keyword::If),
            "nil" => Token::Nil,
            "or" => Token::Keyword(Keyword::Or),
            "print" => Token::Keyword(Keyword::Print),
            "return" => Token::Keyword(Keyword::Return),
            "super" => Token::Keyword(Keyword::Super),
            "this" => Token::Keyword(Keyword::This),
            "true" => Token::True,
            "var" => Token::Keyword(Keyword::Var),
            "while" => Token::Keyword(Keyword::While),
            identifier => Token::Identifier(Rc::new(String::from(identifier))),
        };
        self.new_lexeme_on_this_line(token, content)
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
                    '(' => Some(self.new_lexeme_on_this_line(Token::LeftParen, content)),
                    ')' => Some(self.new_lexeme_on_this_line(Token::RightParen, content)),
                    '{' => Some(self.new_lexeme_on_this_line(Token::LeftBrace, content)),
                    '}' => Some(self.new_lexeme_on_this_line(Token::RightBrace, content)),
                    ',' => Some(self.new_lexeme_on_this_line(Token::Comma, content)),
                    '.' => Some(self.new_lexeme_on_this_line(Token::Dot, content)),
                    '-' => Some(self.new_lexeme_on_this_line(Token::Minus, content)),
                    '+' => Some(self.new_lexeme_on_this_line(Token::Plus, content)),
                    ';' => Some(self.new_lexeme_on_this_line(Token::Semicolon, content)),
                    '*' => Some(self.new_lexeme_on_this_line(Token::Star, content)),
                    '!' => {
                        if let Some(next_index) = self.match_char('=') {
                            let content = slice_str(self.source, start_index, next_index);
                            Some(self.new_lexeme_on_this_line(Token::BangEqual, content))
                        } else {
                            Some(self.new_lexeme_on_this_line(Token::Bang, content))
                        }
                    }
                    '=' => {
                        if let Some(next_index) = self.match_char('=') {
                            let content = slice_str(self.source, start_index, next_index);
                            Some(self.new_lexeme_on_this_line(Token::EqualEqual, content))
                        } else {
                            Some(self.new_lexeme_on_this_line(Token::Equal, content))
                        }
                    }
                    '<' => {
                        if let Some(next_index) = self.match_char('=') {
                            let content = slice_str(self.source, start_index, next_index);
                            Some(self.new_lexeme_on_this_line(Token::LessEqual, content))
                        } else {
                            Some(self.new_lexeme_on_this_line(Token::Less, content))
                        }
                    }
                    '>' => {
                        if let Some(next_index) = self.match_char('=') {
                            let content = slice_str(self.source, start_index, next_index);
                            Some(self.new_lexeme_on_this_line(Token::GreaterEqual, content))
                        } else {
                            Some(self.new_lexeme_on_this_line(Token::Greater, content))
                        }
                    }
                    '/' => {
                        if self.match_char('/').is_some() {
                            let content = loop {
                                match self.position.next() {
                                    None => break &self.source[start_index..],
                                    Some((end_index, '\n')) => {
                                        self.line_number += 1;
                                        break &self.source[start_index..end_index]
                                    }
                                    Some(_) => {},
                                }
                            };
                            Some(self.new_lexeme_on_this_line(Token::Comment(Rc::new(content.to_string())), content))
                        } else {
                            Some(self.new_lexeme_on_this_line(Token::Slash, content))
                        }
                    }
                    '"' => {
                        let (content, terminating_index) = loop {
                            match self.position.next() {
                                None => break (&self.source[start_index..], None),
                                Some((_, '\n')) => self.line_number += 1,
                                Some((end_index, '"')) => break (&self.source[start_index..=end_index], Some(end_index)),
                                Some(_) => {},
                            }
                        };
                        if let Some(terminating_index) = terminating_index {
                            let string_value = self.source[start_index + 1..terminating_index].to_string();
                            Some(self.new_lexeme_on_this_line(Token::String(Rc::new(string_value)), content))
                        } else {
                            Some(self.new_lexeme_on_this_line(Token::UnterminatedString, content))
                        }
                    }
                    '0'..='9' => {
                        let maybe_exclusive_end_index = loop {
                            match self.position.peek() {
                                None => {
                                    break None;
                                }
                                Some((_, '0'..='9')) => {
                                    self.position.next().unwrap();
                                }
                                Some((index, '.')) => {
                                    if matches!(self.position.peek_nth(1), Some((_, '0'..='9'))) {
                                        self.position.next().unwrap();  // consume the '.'
                                        self.position.next().unwrap();  // consume the first digit
                                        break loop {
                                            if let Some((index, c)) = self.position.peek() {
                                                if matches!(c, '0'..='9') {
                                                    self.position.next().unwrap();
                                                } else {
                                                    break Some(index);
                                                }
                                            } else {
                                                break None;
                                            }
                                        }
                                    } else {
                                        // We found a period not followed by a digit.
                                        // The period should be its own lexeme.
                                        break Some(index);
                                    }
                                }
                                Some((index, _)) => {
                                    // We found a non-numeric character, so end the number lexeme.
                                    break Some(index);
                                }
                            }
                        };

                        let content = slice_str(self.source, start_index, maybe_exclusive_end_index);
                        let number = content.parse();
                        match number {
                            Ok(number) => Some(self.new_lexeme_on_this_line(Token::Number(number), content)),
                            Err(e) => Some(self.new_lexeme_on_this_line(Token::InvalidNumber(e), content)),
                        }
                    }
                    'a'..='z' | 'A'..='Z' | '_' => {
                        let maybe_exclusive_end_index = loop {
                            if matches!(self.position.peek(), Some((_, 'a'..='z' | 'A'..='Z' | '_' | '0'..='9'))) {
                                self.position.next().unwrap();
                            } else {
                                break self.position.peek().map(|(index, _)| index);
                            }
                        };
                        let content = slice_str(self.source, start_index, maybe_exclusive_end_index);
                        Some(self.new_identifier(content))
                    }
                    ' ' | '\r' | '\t' => None,  // ignore whitespace chars
                    '\n' => {
                        self.line_number += 1;
                        None
                    }
                    _ => Some(self.new_lexeme_on_this_line(Token::Unknown, content)),
                };

                // If we found a lexeme, return it. If not, keep looking.
                if let Some(lexeme) = maybe_lexeme {
                    break Some(lexeme);
                }
            } else {
                let eof_lexeme = Lexeme::eof(self.line_number);
                self.finished = true;
                break Some(eof_lexeme);
            }
        }
    }
}

fn slice_str(content: &str, start_index: usize, maybe_exclusive_end_index: Option<usize>) -> &str {
    match maybe_exclusive_end_index {
        Some(exclusive_end_index) => &content[start_index..exclusive_end_index],
        None => &content[start_index..],
    }
}

pub struct Lexeme<'a> {
    token: Token,
    content: &'a str,
    line: usize,
}

impl<'a> Lexeme<'a> {
    fn new(token: Token, content: &'a str, line: usize) -> Self {
        Self {
            token, content, line,
        }
    }

    fn eof(line: usize) -> Self {
        Lexeme::new(Token::Eof, "", line)
    }
}
