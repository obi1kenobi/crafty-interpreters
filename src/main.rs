use ron::ser::PrettyConfig;

use crate::{parser::Parser, scanner::Scanner, interpreter::Interpreter};

mod expr;
mod parser;
mod scanner;
mod token;
mod util;
pub mod value;
pub mod interpreter;
pub mod stmt;

fn main() {
    let content = include_str!("lox_program.lox");
    let scanner = Scanner::new(content);
    let mut parser = Parser::new(scanner);

    let parse_outcome = parser.parse();

    println!(
        "{}",
        ron::ser::to_string_pretty(&parse_outcome, PrettyConfig::new()).unwrap()
    );

    if let Ok(program) = parse_outcome {
        let mut interpreter = Interpreter::new();
        for statement in &program {
            let outcome = interpreter.evaluate_stmt(statement);
            if let Err(e) = outcome {
                println!("error: {e}");
                break;
            }
        }
    }
}
