use ron::ser::PrettyConfig;

use crate::{parser::Parser, scanner::Scanner};

mod expr;
mod parser;
mod scanner;
mod token;
mod util;

fn main() {
    let content = r#"
        123.456 + "abc" * (17 * 123) == nil
    "#;
    let scanner = Scanner::new(content);
    let mut parser = Parser::new(scanner);

    let parse_outcome = parser.parse();

    println!(
        "{}",
        ron::ser::to_string_pretty(&parse_outcome, PrettyConfig::new()).unwrap()
    );
}
