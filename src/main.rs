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
        print "precedence and operations";
        print "";
        print true and !false and !(false and true) != 123 + 45 * 2;
        var foo = 123;
        print foo;
        print foo + 456;

        print "";
        print "scopes test";
        print "";
        var a = "global a";
        var b = "global b";
        var c = "global c";
        {
        var a = "outer a";
        var b = "outer b";
        {
            var a = "inner a";
            print a;
            print b;
            print c;
        }
        print a;
        print b;
        print c;
        }
        print a;
        print b;
        print c;

        print "";
        print "nesting scopes";
        print "";
        var a = 1;
        {
            var a = a + 2;
            print a;
        }

        print "";
        print "empty block";
        print "";
        { }
    "#;
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
