use std::io::{Read, Write};
use std::process::exit;
use std::collections::{HashMap, VecDeque};
use noisy_float::prelude::*;
use crate::parse::*;
use crate::lex::*;

#[derive(Debug)]
pub struct Evaluator {
    macros: HashMap<Inst, Inst>,
    stack: Vec<Inst>,
    work: VecDeque<Inst>,
}

impl Evaluator {
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            stack: Vec::new(),
            work: VecDeque::new(),
        }
    }

    pub fn with_std() -> Self {
        let mut new = Self::new();
        new.load_std();
        new
    }

    pub fn load_std(&mut self) {
        let std_lib = include_str!("std.p");
        self.extend_code(std_lib).unwrap();
    }

    pub fn extend_code(&mut self, source: &str) -> Result<(), &str> {
        let prog = parse(lex(source))?;
        self.work.extend(prog);
        Ok(())
    }

    pub fn extend_program(&mut self, prog: Vec<Inst>) {
        self.work.extend(prog);
    }

    fn eval_native<R, W>(&mut self, inst: Native, _input: &mut R, output: &mut W) -> Result<(), &str>
    where R: Read, W: Write {
        match inst {
            Native::Assign => {
                let key = self.stack.pop().ok_or("out of stack")?;
                let val = self.stack.pop().ok_or("out of stack")?;

                if key != val && key != Inst::Text("_".to_string()) {
                    self.macros.insert(key, val);
                }
            },

            Native::Do => {
                let x = self.stack.pop().ok_or("out of stack")?;
                self.work.push_front(x);
            },

            Native::If => {
                let stmt = self.stack.pop().ok_or("out of stack")?;
                let cond = self.stack.pop().ok_or("out of stack")?;

                match cond {
                    Inst::Int(0) => (),
                    Inst::Int(_) => self.work.push_front(stmt),
                    _ => return Err("invalid condition"),
                }
            },

            Native::Tern => {
                let else_stmt = self.stack.pop().ok_or("out of stack")?;
                let then_stmt = self.stack.pop().ok_or("out of stack")?;
                let cond   = self.stack.pop().ok_or("out of stack")?;

                match cond {
                    Inst::Int(0) => self.work.push_front(else_stmt),
                    Inst::Int(_) => self.work.push_front(then_stmt),
                    _ => return Err("invalid condition"),
                }
            },

            Native::While => {
                let stmt = self.stack.pop().ok_or("out of stack")?;
                let cond = self.stack.pop().ok_or("out of stack")?;

                match cond {
                    Inst::Int(0) => (),
                    Inst::Int(_) => {
                        self.work.push_front(Inst::Native(Native::While));
                        self.work.push_front(Inst::Defer(Box::new(stmt.clone())));
                        self.work.push_front(stmt);
                    },
                    _ => return Err("invalid condition"),
                }
            },

            Native::Copy => {
                let num = self.stack.pop().ok_or("out of stack")?;
                let len = self.stack.len();

                match num {
                    Inst::Int(amount) => {
                        if amount < 0 || amount as usize > len {
                            return Err("invalid copy amount");
                        }

                        for i in len.saturating_sub(amount as _)..len {
                            self.stack.push(self.stack[i].clone());
                        }
                    },
                    _ => return Err("invalid copy"),
                }
            },

            Native::Put => {
                let inst = self.stack.pop().ok_or("out of stack")?;

                let buffer = match inst {
                    Inst::Text(s) => s,
                    Inst::Int(s) => format!("{}", s),
                    Inst::Float(s) => format!("{}", s),
                    s => format!("{:?}", s),
                };

                output.write(buffer.as_bytes())
                    .map_err(|_| "failed to write to output")?;
            },

            Native::Add |
            Native::Sub |
            Native::Mul |
            Native::Div |
            Native::Eq  |
            Native::Less => {
                let a = self.stack.pop().ok_or("out of stack")?;
                let b = self.stack.pop().ok_or("out of stack")?;
                let c = match (inst, a, b) {
                    (Native::Add, Inst::Text(mut x), Inst::Text(y)) => { x.push_str(y.as_str()); Inst::Text(x) },
                    (Native::Add, Inst::Text(mut s), Inst::Int(y)) => { s.push_str(y.to_string().as_str()); Inst::Text(s) },
                    (Native::Add, Inst::Text(mut s), Inst::Float(y)) => { s.push_str(y.to_string().as_str()); Inst::Text(s) },
                    (Native::Add, Inst::Int(x), Inst::Text(mut s)) => { s.push_str(x.to_string().as_str()); Inst::Text(s) },
                    (Native::Add, Inst::Int(x), Inst::Int(y)) => { Inst::Int(x + y) },
                    (Native::Add, Inst::Int(x), Inst::Float(y)) => { Inst::Float(r32(x as _) + y) },
                    (Native::Add, Inst::Float(x), Inst::Text(mut s)) => { s.push_str(x.to_string().as_str()); Inst::Text(s) },
                    (Native::Add, Inst::Float(x), Inst::Int(y)) => { Inst::Float(x + r32(y as _)) },
                    (Native::Add, Inst::Float(x), Inst::Float(y)) => { Inst::Float(x + y) },

                    (Native::Sub, Inst::Int(x), Inst::Int(y)) => { Inst::Int(x - y) },
                    (Native::Sub, Inst::Int(x), Inst::Float(y)) => { Inst::Float(r32(x as _) - y) },
                    (Native::Sub, Inst::Float(x), Inst::Int(y)) => { Inst::Float(x - r32(y as _)) },
                    (Native::Sub, Inst::Float(x), Inst::Float(y)) => { Inst::Float(x - y) },

                    (Native::Mul, Inst::Int(x), Inst::Int(y)) => { Inst::Int(x * y) },
                    (Native::Mul, Inst::Int(x), Inst::Float(y)) => { Inst::Float(r32(x as _) * y) },
                    (Native::Mul, Inst::Float(x), Inst::Int(y)) => { Inst::Float(x * r32(y as _)) },
                    (Native::Mul, Inst::Float(x), Inst::Float(y)) => { Inst::Float(x * y) },

                    (Native::Div, Inst::Int(x), Inst::Int(y)) => { Inst::Int(x / y) },
                    (Native::Div, Inst::Int(x), Inst::Float(y)) => { Inst::Float(r32(x as _) / y) },
                    (Native::Div, Inst::Float(x), Inst::Int(y)) => { Inst::Float(x / r32(y as _)) },
                    (Native::Div, Inst::Float(x), Inst::Float(y)) => { Inst::Float(x / y) },

                    (Native::Eq, x, y) => { Inst::Int((x == y) as _) },

                    (Native::Less, Inst::Int(x), Inst::Int(y)) => { Inst::Int((x < y) as _) },
                    (Native::Less, Inst::Int(x), Inst::Float(y)) => { Inst::Int((r32(x as _) < y) as _) },
                    (Native::Less, Inst::Float(x), Inst::Int(y)) => { Inst::Int((x < r32(y as _)) as _) },
                    (Native::Less, Inst::Float(x), Inst::Float(y)) => { Inst::Int((x < y) as _) },

                    _ => return Err("operation is undefined"),
                };

                self.stack.push(c);
            }

            Native::Floor => {
                let num = self.stack.pop().ok_or("out of stack")?;

                match num {
                    Inst::Int(_) => self.stack.push(num),
                    Inst::Float(x) => self.stack.push(Inst::Int(x.floor().raw() as _)),
                    _ => return Err("operation is undefined"),
                }
            },

            Native::Exit  => exit(0),
            Native::Abort => exit(-1),
        }

        Ok(())
    }

    fn macro_replace(&mut self, inst: Inst) -> Inst {
        if !self.macros.contains_key(&inst)
        || matches!(inst, Inst::Defer(_)) {
            return inst;
        }

        let mut next = inst.clone();
        let mut iter = 0;

        while let Some(val) = self.macros.get(&next) {
            next = val.clone();
            iter += 1;

            debug_assert_ne!(inst, next);
        }

        if iter > 1 {
            self.macros.insert(inst, next.clone());
        }

        next
    }

    pub fn done(&self) -> bool {
        self.work.is_empty()
    }

    pub fn step<R, W>(&mut self, input: &mut R, output: &mut W) -> Result<bool, &str>
    where R: Read, W: Write {
        let inst = self.work.pop_front()
            .map(|old| self.macro_replace(old));

        match inst {
            Some(Inst::Native(n)) => { self.eval_native(n, input, output)?; },
            Some(Inst::Defer(s)) => { self.stack.push(*s); },
            Some(Inst::Block(s)) => {
                s.iter().rev().for_each(
                    |inst| self.work.push_front(inst.clone())
                );
            },
            Some(s) => { self.stack.push(s); },
            None => return Ok(true),
        }

        Ok(false)
    }

    pub fn run<R, W>(&mut self, input: &mut R, output: &mut W) -> Result<(), &str>
    where R: Read, W: Write {
        while !self.done() {
            // TODO: Fix this
            self.step(input, output).unwrap();
        }

        Ok(())
    }

    pub fn print_state(&self) {
        print!("  ->  ");
        for x in self.stack.iter() {
            print!("{} ", &x);
        }
        print!("| ");
        for x in self.work.iter() {
            print!("{} ", &x);
        }
        print!("\n");
    }
}

#[allow(dead_code)]
fn eval<R, W>(prog: Vec<Inst>, input: &mut R, output: &mut W) -> Result<(), &'static str>
where R: Read, W: Write {
    let mut eval = Evaluator::with_std();
    eval.extend_program(prog);
    // TODO: Fix this
    eval.run(input, output).unwrap();
    Ok(())
}

#[cfg(test)]
mod test {
    use std::io::*;
    use crate::lex::*;
    use crate::parse::*;
    use super::*;

    fn eval_helper(code: &str, expected: &str) {
        let prog = parse(lex(code)).unwrap();
        let mut output = Vec::new();
        // TODO: Fix this
        eval(prog, &mut empty(), &mut output).unwrap();
        assert_eq!(output.as_slice(), expected.as_bytes());
    }

    #[test]
    fn eval_sanity_check() {
        eval_helper("", "");
    }

    #[test]
    fn eval_hello() {
        let code = "1 ;{ \"hello\" ;put do } if";
        eval_helper(code, "hello");
    }

    #[test]
    fn eval_fibonacci() {
        let code = "(fib = ;{;n = 0 1 (n > 0) ;{xch over + (;n = (n - 1)) (n != 0)} while pop}) (put (fib 42))";
        eval_helper(code, "267914296");
    }

    #[test]
    fn eval_gcd() {
        let code = "(gcd = ;{1 ;{(copy 2) < ;xch if over xch - (0 != over)} while xch pop}) (put (35 gcd 91))";
        eval_helper(code, "7");
    }

    #[test]
    fn eval_do_do_do() {
        let code = "test ;;put do ;;do ;do do do";
        eval_helper(code, "test");
    }

    #[test]
    fn eval_quick_maths() {
        let code = "(9 - ((3 + 1) * 2)) put";
        eval_helper(code, "1");
    }

    #[test]
    fn eval_macro_simple() {
        let code = "(5 = 3) (n = 5) (put n)";
        eval_helper(code, "3");
    }

    #[test]
    fn eval_macro_circle() {
        let code = "(5 = 3) (3 = 5) (put 3) (put 5)";
        eval_helper(code, "33");
    }

    #[test]
    fn eval_macro_override() {
        let code = "(2 = 1) (2 = 3) (1 = 2) (put 1)";
        eval_helper(code, "3");
    }

    #[test]
    fn eval_macro_blocks() {
        let code = "0 1 5 2 3 7 9 ;{ dup put ;b if } b = b";
        eval_helper(code, "9732510");
    }

    #[test]
    fn eval_while_loop() {
        let code = "0 1 1 1 1 ;{ a put } while";
        eval_helper(code, "aaaa");
    }

    #[test]
    fn eval_ternary() {
        let code = "(3 == 4) ;{ same } ;{ different } ? put";
        eval_helper(code, "different");
        let code = "(2 == 2) ;{ same } ;{ different } ? put";
        eval_helper(code, "same");
    }
}
