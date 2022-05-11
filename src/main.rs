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
    let content = r#"
        true and !false and !(false and true) == 123 + 45 * 2 + "bar"
    "#;
    let scanner = Scanner::new(content);
    let mut parser = Parser::new(scanner);

    let parse_outcome = parser.parse();

    println!(
        "{}",
        ron::ser::to_string_pretty(&parse_outcome, PrettyConfig::new()).unwrap()
    );

    if let Ok(expr) = parse_outcome {
        let mut interpreter = Interpreter::new();
        let eval_outcome = interpreter.evaluate_expr(&expr);
        match eval_outcome {
            Ok(val) => println!("output: {}", val),
            Err(e) => println!("error: {}", e),
        }
    }
}
