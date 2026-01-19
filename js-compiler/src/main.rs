mod lexer;
mod parser;
mod ast;

use lexer::Lexer;
use lexer::token::Token;
use parser::Parser;
use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: js-compiler <file> [--json] [--ast]");
        process::exit(1);
    }

    let filename = &args[1];
    let content = fs::read_to_string(filename).expect("Something went wrong reading the file");

    if args.contains(&String::from("--ast")) {
        let lexer = Lexer::new(&content);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let json_output = serde_json::to_string(&program).unwrap();
        println!("{}", json_output);
    } else {
        let mut lexer = Lexer::new(&content);
        let mut tokens = Vec::new();

        loop {
            let token = lexer.next_token();
            if token == Token::EOF {
                break;
            }
            tokens.push(token);
        }

        if args.contains(&String::from("--json")) {
            let json_output = serde_json::to_string(&tokens).unwrap();
            println!("{}", json_output);
        } else {
            for token in tokens {
                println!("{:?}", token);
            }
        }
    }
}
