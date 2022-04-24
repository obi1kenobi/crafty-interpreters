use ron::ser::PrettyConfig;

use crate::{parser::Parser, scanner::Scanner};

mod expr;
mod parser;
mod scanner;
mod token;
mod util;

fn main() {
    let content = r#"
        true and !false and !(false and true) == 123 + 45 * 2
    "#;
    let scanner = Scanner::new(content);
    let mut parser = Parser::new(scanner);

    let parse_outcome = parser.parse();

    println!(
        "{}",
        ron::ser::to_string_pretty(&parse_outcome, PrettyConfig::new()).unwrap()
    );
}
