#![warn(clippy::all)]
use std::io;
use std::io::prelude::*;
use std::fs::File;
use std::str;
use std::process;
use pico_args::Arguments;

mod lex;
mod parse;
mod eval;

use crate::lex::*;
use crate::parse::*;
use crate::eval::*;
use io::Write;

fn show_usage() {
    println!("Usage: paste [OPTIONS] INPUT")
}

fn show_help() {
    show_usage();
    println!(
"
An esoteric programming language build around macros

Options:
    -h, --help    Print this help message and exit.
    -r, --repl    Enter interactive mode after the script terminated.
    -d, --debug   Show all internal computations during evaluation.
"
    );
}

fn main() {
    let mut args = Arguments::from_env();

    // output help message
    if args.contains(["-h", "--help"]) {
        show_help();
        process::exit(0);
    }

    // store options
    let repl = args.contains(["-r", "--repl"]);
    let debug = args.contains(["-d", "--debug"]);

    // init evaluator
    let mut paste = Evaluator::with_std();
    paste.run(&mut io::empty(), &mut io::sink())
        .unwrap();

    // load scripts
    let mut no_script = true;
    for filename in args.free().unwrap() {
        let mut file = File::open(filename)
            .expect("failed to open file");

        let mut content = String::new();
        file.read_to_string(&mut content)
            .expect("failed to read file");

        match paste.extend_code(content.as_str()) {
            Ok(s) => s,
            Err(s) => {
                eprintln!("Error: {}\n", s);
                process::exit(-1);
            }
        }

        no_script = false;
    }

    // always repl if no script is entered
    let repl = repl || no_script;

    loop {
        // evaluate
        let mut newline = false;
        while !paste.done() {
            if debug {
                println!("   ~>  {}", paste);
            }

            let mut output = Vec::new();
            if let Err(s) = paste.step(&mut io::empty(), &mut output) {
                eprintln!("Error: {}", s);
                eprintln!("# {}\n", paste);
                process::exit(-1);
            }

            if !output.is_empty() {
                newline = true;
                print!("{}", str::from_utf8(&output)
                    .expect("failed to read output"));
            }
        }

        // trailing output
        if debug {
            println!("   ~>  {}", paste);
        } else if newline {
            println!();
        }

        // finish if in interactive mode
        if !repl {
            break;
        }

        // ask for input
        print!("> ");
        io::stdout().flush().unwrap();

        let mut buffer = String::new();
        io::stdin().read_line(&mut buffer)
            .expect("failed to read input");

        // append to program
        let tokens = lex(buffer.as_str());
        let program = match parse(tokens) {
            Ok(s) => s,
            Err(s) => {
                eprintln!("Error: {}\n", s);
                process::exit(-1);
            }
        };
        paste.extend_program(program);
    }
}
