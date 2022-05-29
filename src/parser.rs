#![allow(dead_code)]

use std::iter::Peekable;

use serde::{Deserialize, Serialize};

use crate::{
    expr::{BinaryExpr, Expr, Literal, UnaryExpr},
    scanner::Lexeme,
    stmt::{IfStatement, Stmt, VarDeclaration, WhileStatement},
    token::{Keyword, Token},
};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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
#[allow(clippy::enum_variant_names)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, thiserror::Error)]
pub enum ParseErrorKind {
    #[error("Expected ')' after expression.")]
    ExpectedRightParenAfterExpr,

    #[error("Expected '}}' after block.")]
    ExpectedRightBraceAfterBlock,

    #[error("Expected expression.")]
    ExpectedExpression,

    #[error("Expected ';' at the end of the statement.")]
    ExpectedSemicolon,

    #[error("Expected variable name after 'var'.")]
    ExpectedVariableName,

    #[error("Expected '(' after 'if'.")]
    ExpectedLeftParenAfterIf,

    #[error("Expected ')' after if condition.")]
    ExpectedRightParentAfterIfCondition,

    #[error("Expected '(' after 'while'.")]
    ExpectedLeftParenAfterWhile,

    #[error("Expected ')' after while condition.")]
    ExpectedRightParentAfterWhileCondition,

    #[error("Expected '(' after 'for'.")]
    ExpectedLeftParenAfterFor,

    #[error("Expected ')' after for clauses.")]
    ExpectedRightParentAfterForClauses,

    #[error("Expected ';' after loop condition.")]
    ExpectedSemicolonAfterLoopCondition,

    #[error("Invalid assignment target.")]
    InvalidAssignmentTarget,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, derive_new::new)]
pub struct ParseWithErrors {
    partial_program: Vec<Stmt>,
    errors: Vec<ParseError>,
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

    pub fn parse(&mut self) -> Result<Vec<Stmt>, ParseWithErrors> {
        self.program()
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

    fn program(&mut self) -> Result<Vec<Stmt>, ParseWithErrors> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();
        while self.tokens.peek().expect("skipped past EOF token").token != Token::Eof {
            match self.declaration() {
                Ok(statement) => statements.push(statement),
                Err(e) => {
                    errors.push(e);
                    self.synchronize();
                }
            }
        }

        let last_token = self.tokens.next().expect("skipped past EOF token").token;
        assert_eq!(last_token, Token::Eof);

        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(ParseWithErrors::new(statements, errors))
        }
    }

    fn declaration(&mut self) -> Result<Stmt, ParseError> {
        let next_token = self.tokens.peek().expect("skipped past EOF token");
        if let Token::Keyword(Keyword::Var) = next_token.token {
            self.variable_declaration()
        } else {
            self.statement()
        }
    }

    fn variable_declaration(&mut self) -> Result<Stmt, ParseError> {
        let next_token = self
            .tokens
            .next()
            .expect("peeked item already consumed")
            .token;
        assert_eq!(next_token, Token::Keyword(Keyword::Var));

        let next_lexeme = self.tokens.peek().expect("skipped past EOF token");
        let identifier = match &next_lexeme.token {
            Token::Identifier(ident) => {
                let identifier = ident.clone();
                self.tokens.next().expect("peeked item already consumed");
                identifier
            }
            _ => {
                return Err(ParseError::new(
                    ParseErrorKind::ExpectedVariableName,
                    next_lexeme.content,
                    next_lexeme.line,
                ));
            }
        };

        let initializer =
            if self.tokens.peek().expect("skipped past EOF token").token == Token::Equal {
                self.tokens.next().expect("peeked item already consumed");
                Some(self.expression()?)
            } else {
                None
            };

        self.ensure_next_token(Token::Semicolon, ParseErrorKind::ExpectedSemicolon)?;

        Ok(Stmt::VarDeclaration(VarDeclaration::new(
            identifier,
            initializer,
        )))
    }

    fn statement(&mut self) -> Result<Stmt, ParseError> {
        let next_token = self.tokens.peek().expect("skipped past EOF token");
        match next_token.token {
            Token::Keyword(Keyword::Print) => {
                self.tokens.next().expect("peeked item already consumed");
                let expr = self.expression()?;
                self.ensure_next_token(Token::Semicolon, ParseErrorKind::ExpectedSemicolon)?;

                Ok(Stmt::Print(expr))
            }
            Token::Keyword(Keyword::If) => {
                self.tokens.next().expect("peeked item already consumed");
                self.ensure_next_token(Token::LeftParen, ParseErrorKind::ExpectedLeftParenAfterIf)?;

                let condition = self.expression()?;

                self.ensure_next_token(
                    Token::RightParen,
                    ParseErrorKind::ExpectedRightParentAfterIfCondition,
                )?;

                let then_branch = self.statement()?;
                let else_branch = if let Token::Keyword(Keyword::Else) =
                    self.tokens.peek().expect("skipped past EOF token").token
                {
                    self.tokens.next().expect("peeked item already consumed");
                    Some(Box::new(self.statement()?))
                } else {
                    None
                };

                Ok(Stmt::If(IfStatement::new(
                    condition,
                    Box::new(then_branch),
                    else_branch,
                )))
            }
            Token::Keyword(Keyword::While) => {
                self.tokens.next().expect("peeked item already consumed");
                self.ensure_next_token(
                    Token::LeftParen,
                    ParseErrorKind::ExpectedLeftParenAfterWhile,
                )?;

                let condition = self.expression()?;

                self.ensure_next_token(
                    Token::RightParen,
                    ParseErrorKind::ExpectedRightParentAfterWhileCondition,
                )?;

                let body = self.statement()?;

                Ok(Stmt::While(WhileStatement::new(condition, Box::new(body))))
            }
            Token::Keyword(Keyword::For) => {
                self.tokens.next().expect("peeked item already consumed");
                self.ensure_next_token(
                    Token::LeftParen,
                    ParseErrorKind::ExpectedLeftParenAfterFor,
                )?;

                let initializer = match self.tokens.peek().expect("skipped past EOF token").token {
                    Token::Semicolon => {
                        self.tokens.next().expect("peeked item already consumed");
                        None
                    }
                    Token::Keyword(Keyword::Var) => Some(self.variable_declaration()?),
                    _ => Some(self.expression()?.into()),
                };

                let condition = if self.tokens.peek().expect("skipped past EOF token").token
                    == Token::Semicolon
                {
                    Expr::Literal(Literal::Boolean(true))
                } else {
                    self.expression()?
                };
                self.ensure_next_token(
                    Token::Semicolon,
                    ParseErrorKind::ExpectedSemicolonAfterLoopCondition,
                )?;

                let increment = if self.tokens.peek().expect("skipped past EOF token").token
                    == Token::RightParen
                {
                    None
                } else {
                    Some(self.expression()?)
                };

                self.ensure_next_token(
                    Token::RightParen,
                    ParseErrorKind::ExpectedRightParentAfterForClauses,
                )?;

                let mut body = self.statement()?;
                if let Some(increment) = increment {
                    body = Stmt::Block(vec![body, Stmt::Expression(increment)]);
                }

                body = Stmt::While(WhileStatement::new(condition, Box::new(body)));

                if let Some(initializer) = initializer {
                    body = Stmt::Block(vec![initializer, body]);
                }

                Ok(body)
            }
            Token::LeftBrace => {
                self.tokens.next().expect("peeked item already consumed");
                let mut inner_statements = vec![];

                loop {
                    let peeked_token = &self.tokens.peek().expect("skipped past EOF token").token;
                    if peeked_token == &Token::RightBrace || peeked_token == &Token::Eof {
                        break;
                    }
                    inner_statements.push(self.declaration()?);
                }

                self.ensure_next_token(
                    Token::RightBrace,
                    ParseErrorKind::ExpectedRightBraceAfterBlock,
                )?;

                Ok(Stmt::Block(inner_statements))
            }
            _ => {
                let expr = self.expression()?;
                self.ensure_next_token(Token::Semicolon, ParseErrorKind::ExpectedSemicolon)?;
                Ok(Stmt::Expression(expr))
            }
        }
    }

    fn expression(&mut self) -> Result<Expr, ParseError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.logical_or()?;

        if self.tokens.peek().expect("skipped past EOF token").token == Token::Equal {
            let lex = self.tokens.next().expect("peeked item already consumed");

            let value = self.assignment()?;

            if let Expr::Variable(var_name) = expr {
                Ok(Expr::Assignment(var_name, Box::new(value)))
            } else {
                Err(ParseError::new(
                    ParseErrorKind::InvalidAssignmentTarget,
                    lex.content,
                    lex.line,
                ))
            }
        } else {
            Ok(expr)
        }
    }

    fn logical_or(&mut self) -> Result<Expr, ParseError> {
        binary_expression_impl!(self, tokens, logical_and, Token::Keyword(Keyword::Or))
    }

    fn logical_and(&mut self) -> Result<Expr, ParseError> {
        binary_expression_impl!(self, tokens, equality, Token::Keyword(Keyword::And))
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
            term,
            Token::Greater | Token::GreaterEqual | Token::Less | Token::LessEqual
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
            Token::Identifier(ident) => Some(Expr::Variable(ident.clone())),
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
