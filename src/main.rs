#![warn(clippy::all)]
use pico_args::Arguments;
use std::{
    fs,
    io::{self, Write},
    process, str,
};

use paste_lang::eval::*;
use paste_lang::lex::*;
use paste_lang::parse::*;

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
    paste.run(&mut io::empty(), &mut io::sink()).unwrap();

    // load scripts
    let mut no_script = true;
    for filename in args.free().unwrap() {
        let content = fs::read_to_string(filename).expect("failed to read file");

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
                print!(
                    "{}",
                    str::from_utf8(&output).expect("failed to read output")
                );
            }
        }

        // trailing output
        if debug {
            println!("   ~>  {}", paste);
        } else if newline {
            println!();
        }

        // finish if not in interactive mode
        if !repl {
            break;
        }

        // ask for input
        print!("> ");
        io::stdout().flush().unwrap();
        let mut buffer = String::new();

        loop {
            io::stdin()
                .read_line(&mut buffer)
                .expect("failed to read input");

            if check_complete(lex(buffer.as_str())) != Ok(false) {
                break;
            }

            print!(". ");
            io::stdout().flush().unwrap();
        }

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
