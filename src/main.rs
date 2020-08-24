#![warn(clippy::all)]
use std::io;

mod lex;
mod parse;
mod eval;

use crate::lex::*;
use crate::parse::*;
use crate::eval::*;

fn main() {
    let mut input = io::empty();
    let mut output = io::stdout();
    let mut paste = Evaluator::with_std();

    while !paste.done() {
        paste.print_state();
        paste.step(&mut input, &mut output).unwrap();
    }

    paste.print_state();

    loop {
        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)
            .expect("failed to read input");

        let tokens = lex(buffer.as_str());
        let program = parse(tokens).unwrap();

        paste.extend_program(program);

        while !paste.done() {
            paste.print_state();
            paste.step(&mut input, &mut output).unwrap();
        }

        paste.print_state();

        //print!("{}", str::from_utf8(&output).unwrap());
    }
}
