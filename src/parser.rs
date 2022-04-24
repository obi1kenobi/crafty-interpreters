#![allow(dead_code)]

use std::iter::Peekable;

use serde::{Deserialize, Serialize};

use crate::{
    expr::{BinaryExpr, Expr, Literal, UnaryExpr},
    scanner::Lexeme,
    token::{Keyword, Token},
};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ParseError {
    kind: ParseErrorKind,
    next_content: String,
    line: usize,
}

impl ParseError {
    pub(crate) fn new<S: Into<String>>(kind: ParseErrorKind, next_content: S, line: usize) -> Self {
        Self {
            kind,
            next_content: next_content.into(),
            line,
        }
    }
}

#[non_exhaustive]
#[derive(Debug, Clone, Copy, Serialize, Deserialize, thiserror::Error)]
pub enum ParseErrorKind {
    #[error("Expected ')' after expression.")]
    ExpectedRightParenAfterExpr,

    #[error("Expected expression.")]
    ExpectedExpression,
}

pub struct Parser<'a, I>
where
    I: Iterator<Item = Lexeme<'a>>,
{
    tokens: Peekable<I>,
}

macro_rules! binary_expression_impl {
    ($self:ident, $token_iter:ident, $inner_parser:ident, $matcher:pat) => {{
        let mut current = $self.$inner_parser()?;

        while let $matcher = &$self
            .$token_iter
            .peek()
            .expect("skipped past EOF token")
            .token
        {
            let operator = $self
                .$token_iter
                .next()
                .expect("peeked item already consumed")
                .token;
            let right = $self.$inner_parser()?;

            current = Expr::Binary(Box::new(BinaryExpr {
                left: current,
                operator,
                right,
            }));
        }

        Ok(current)
    }};
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Lexeme<'a>>,
{
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    pub fn parse(&mut self) -> Result<Expr, ParseError> {
        self.expression()
    }

    fn synchronize(&mut self) {
        loop {
            match &self.tokens.peek().expect("skipped past EOF token").token {
                Token::Eof => break,
                Token::Semicolon => {
                    self.tokens.next().expect("peeked item already consumed");
                    break;
                }
                _ => {
                    self.tokens.next().expect("peeked item already consumed");
                    if let Token::Keyword(
                        Keyword::Class
                        | Keyword::For
                        | Keyword::Fun
                        | Keyword::If
                        | Keyword::Print
                        | Keyword::Return
                        | Keyword::Var
                        | Keyword::While,
                    ) = &self.tokens.peek().expect("skipped past EOF token").token
                    {
                        break;
                    }
                }
            }
        }
    }

    fn ensure_next_token(
        &mut self,
        token: Token,
        error_on_mismatch: ParseErrorKind,
    ) -> Result<(), ParseError> {
        let lex = self.tokens.next().expect("skipped past EOF token");
        if lex.token == token {
            Ok(())
        } else {
            Err(ParseError::new(error_on_mismatch, lex.content, lex.line))
        }
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParseError> {
        binary_expression_impl!(
            self,
            tokens,
            comparison,
            Token::BangEqual | Token::EqualEqual
        )
    }

    fn comparison(&mut self) -> Result<Expr, ParseError> {
        binary_expression_impl!(
            self,
            tokens,
            logical,
            Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual
        )
    }

    fn logical(&mut self) -> Result<Expr, ParseError> {
        binary_expression_impl!(
            self,
            tokens,
            term,
            Token::Keyword(Keyword::And | Keyword::Or)
        )
    }

    fn term(&mut self) -> Result<Expr, ParseError> {
        binary_expression_impl!(self, tokens, factor, Token::Minus | Token::Plus)
    }

    fn factor(&mut self) -> Result<Expr, ParseError> {
        binary_expression_impl!(self, tokens, unary, Token::Slash | Token::Star)
    }

    fn unary(&mut self) -> Result<Expr, ParseError> {
        let next_token = self.tokens.peek();
        if let Some(Token::Bang | Token::Minus) = next_token.map(|lex| &lex.token) {
            let operator = self
                .tokens
                .next()
                .expect("peeked item already consumed")
                .token;
            let unary = UnaryExpr {
                operator,
                right: self.unary()?,
            };
            return Ok(Expr::Unary(Box::new(unary)));
        };

        self.primary()
    }

    fn primary(&mut self) -> Result<Expr, ParseError> {
        let next_token = self.tokens.peek().expect("peeked past EOF token");
        let maybe_parsed = match &next_token.token {
            Token::Number(n) => Some(Expr::Literal(Literal::Number(*n))),
            Token::String(s) => Some(Expr::Literal(Literal::String(s.clone()))),
            Token::True => Some(Expr::Literal(Literal::Boolean(true))),
            Token::False => Some(Expr::Literal(Literal::Boolean(false))),
            Token::Nil => Some(Expr::Literal(Literal::Nil)),
            _ => None,
        };

        if let Some(parsed) = maybe_parsed {
            self.tokens.next().expect("peeked item already consumed");
            return Ok(parsed);
        }

        if next_token.token == Token::LeftParen {
            self.tokens.next().expect("peeked item already consumed");
            let inner_expr = self.expression()?;
            self.ensure_next_token(
                Token::RightParen,
                ParseErrorKind::ExpectedRightParenAfterExpr,
            )?;
            return Ok(Expr::Grouping(Box::new(inner_expr)));
        }

        Err(ParseError::new(
            ParseErrorKind::ExpectedExpression,
            next_token.content,
            next_token.line,
        ))
    }
}
