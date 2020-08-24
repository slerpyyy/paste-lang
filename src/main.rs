#![warn(clippy::all)]
use std::io;
use std::str;
use std::process;
use pico_args::*;

mod lex;
mod parse;
mod eval;

use crate::lex::*;
use crate::parse::*;
use crate::eval::*;
use io::Write;

fn show_usage() {
    println!("Usage: paste FILE [OPTION...]")
}

fn show_help() {
    show_usage();
    println!("\n\
An esoteric programming language build around macros

Options:
    -h, --help      Print this help message and exit.
    -i, --interact  Enter interactive mode after the script terminated.
    -d, --debug     Show all internal computations during evaluation.
\
    ");
}

fn main() {
    let mut args = Arguments::from_env();

    if args.contains(["-h", "--help"]) {
        show_help();
        process::exit(0);
    }

    let debug = args.contains(["-d", "--debug"]);

    let mut paste = Evaluator::with_std();

    while !paste.done() {
        paste.step(&mut io::empty(), &mut io::stdout()).unwrap();
    }

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)
            .expect("failed to read input");

        let tokens = lex(buffer.as_str());
        let program = parse(tokens).unwrap();
        paste.extend_program(program);

        let mut newline = false;
        while !paste.done() {
            if debug {
                println!("{}", paste);
            }
            let mut output = Vec::new();
            paste.step(&mut io::empty(), &mut output).unwrap();

            if !output.is_empty() {
                newline = true;
                print!("{}", str::from_utf8(&output)
                    .expect("failed to read output"));
            }
        }

        if debug {
            println!("{}", paste);
        } else if newline {
            println!();
        }
    }
}
