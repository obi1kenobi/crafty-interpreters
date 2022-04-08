#![allow(dead_code)]

use std::{rc::Rc};

use serde::{Serialize, Deserialize};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Keyword {
    And,
    Class,
    Else,
    Fun,
    For,
    If,
    Or,
    Print,
    Return,
    Super,
    This,
    Var,
    While,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Token {
    // Guaranteed single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Slash,  // Double slash means the start of a comment, so we don't emit a '//' token by itself.

    // Literals.
    Identifier(Rc<String>),
    String(Rc<String>),
    Number(f64),
    True,
    False,
    Nil,

    // Keywords.
    Keyword(Keyword),

    // Comments.
    Comment(Rc<String>),

    // Unexpected content.
    Unknown,
    UnterminatedString,
    InvalidNumber,

    // End-of-file token.
    Eof,
}
