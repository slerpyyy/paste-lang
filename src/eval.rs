use crate::lex::*;
use crate::parse::*;
use noisy_float::prelude::*;
use std::collections::{HashMap, VecDeque};
use std::io::{Read, Write};
use std::{fmt, process};

#[derive(Debug, Clone)]
pub struct Evaluator {
    macros: HashMap<Sym, Sym>,
    stack: Vec<Sym>,
    work: VecDeque<Sym>,
}

impl fmt::Display for Evaluator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for sym in &self.stack {
            write!(f, "{} ", sym)?;
        }
        write!(f, "|")?;
        for sym in &self.work {
            write!(f, " {}", sym)?;
        }
        Ok(())
    }
}

macro_rules! pop_stack {
    ($self:ident, $($var:ident),*) => {
        $self.stack_assert(pop_stack!(@COUNT; $($var),*))?;
        $(let $var = $self.stack.pop().unwrap();)*
    };

    (@COUNT; $($var:ident),*) => {
        <[()]>::len(&[$(pop_stack!(@SUBST; $var)),*])
    };

    (@SUBST; $_i:ident) => { () };
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
        // SAFETY: The standard library does not change depending
        // on user input and is tested explicitly
        let std_lib = include_str!("std.paste");
        self.extend_code(std_lib).unwrap();
    }

    pub fn extend_code(&mut self, source: &str) -> Result<(), String> {
        let prog = parse(lex(source))?;
        self.work.extend(prog);
        Ok(())
    }

    pub fn extend_program(&mut self, prog: Vec<Sym>) {
        self.work.extend(prog);
    }

    fn stack_assert(&self, len: usize) -> Result<(), String> {
        if len > self.stack.len() {
            return Err(format!(
                "expected {} items on the stack, found {}",
                len,
                self.stack.len()
            ));
        }

        Ok(())
    }

    fn eval_native<R, W>(
        &mut self,
        sym: Native,
        _input: &mut R,
        output: &mut W,
    ) -> Result<(), String>
    where
        R: Read,
        W: Write,
    {
        match sym {
            Native::Assign => {
                pop_stack!(self, key, val);

                if key != val && key != Sym::Text("_".into()) {
                    self.macros.insert(key, val);
                }
            }

            Native::Do => {
                pop_stack!(self, x);
                self.work.push_front(x);
            }

            Native::Tern => {
                pop_stack!(self, else_stmt, then_stmt, cond);

                match cond {
                    Sym::Int(0) => self.work.push_front(else_stmt),
                    Sym::Int(_) => self.work.push_front(then_stmt),
                    _ => {
                        self.stack.push(cond);
                        self.stack.push(then_stmt);
                        self.stack.push(else_stmt);
                        return Err("invalid condition".into());
                    }
                }
            }

            Native::While => {
                pop_stack!(self, stmt, cond);

                match cond {
                    Sym::Int(0) => (),
                    Sym::Int(_) => {
                        self.work.push_front(Sym::Native(Native::While));
                        self.work.push_front(Sym::Defer(Box::new(stmt.clone())));
                        self.work.push_front(stmt);
                    }
                    _ => {
                        self.stack.push(cond);
                        self.stack.push(stmt);
                        return Err("invalid condition".into());
                    }
                }
            }

            Native::Copy => {
                pop_stack!(self, num);
                let len = self.stack.len();

                match num {
                    Sym::Int(amount) => {
                        if amount < 0 || amount as usize > len {
                            self.stack.push(num);
                            return Err("invalid copy amount".into());
                        }

                        for i in len.saturating_sub(amount as _)..len {
                            self.stack.push(self.stack[i].clone());
                        }
                    }

                    _ => {
                        self.stack.push(num);
                        return Err("invalid copy".into());
                    }
                }
            }

            Native::Put => {
                pop_stack!(self, sym);

                let string;
                let buffer = match &sym {
                    Sym::Text(s) => s.as_ref(),
                    x => {
                        string = match x {
                            Sym::Int(s) => format!("{}", s),
                            Sym::Float(s) => format!("{}", s),
                            s => format!("{:?}", s),
                        };
                        &string
                    }
                };

                output
                    .write(buffer.as_bytes())
                    .map_err(|_| "failed to write to output")?;
            }

            Native::Add | Native::Sub | Native::Mul | Native::Div | Native::Eq | Native::Less => {
                pop_stack!(self, a, b);

                #[inline]
                fn concat(a: impl fmt::Display, b: impl fmt::Display) -> String {
                    format!("{}{}", a, b)
                }

                let c = match (sym, a, b) {
                    (Native::Add, Sym::Text(x), Sym::Text(y)) => Sym::text(concat(x, y)),
                    (Native::Add, Sym::Text(s), Sym::Int(y)) => Sym::text(concat(s, y)),
                    (Native::Add, Sym::Text(s), Sym::Float(y)) => Sym::text(concat(s, y)),
                    (Native::Add, Sym::Int(x), Sym::Text(s)) => Sym::text(concat(x, s)),
                    (Native::Add, Sym::Int(x), Sym::Int(y)) => Sym::Int(x + y),
                    (Native::Add, Sym::Int(x), Sym::Float(y)) => Sym::Float(r64(x as _) + y),
                    (Native::Add, Sym::Float(x), Sym::Text(s)) => Sym::text(concat(x, s)),
                    (Native::Add, Sym::Float(x), Sym::Int(y)) => Sym::Float(x + r64(y as _)),
                    (Native::Add, Sym::Float(x), Sym::Float(y)) => Sym::Float(x + y),

                    (Native::Sub, Sym::Int(x), Sym::Int(y)) => Sym::Int(x - y),
                    (Native::Sub, Sym::Int(x), Sym::Float(y)) => Sym::Float(r64(x as _) - y),
                    (Native::Sub, Sym::Float(x), Sym::Int(y)) => Sym::Float(x - r64(y as _)),
                    (Native::Sub, Sym::Float(x), Sym::Float(y)) => Sym::Float(x - y),

                    (Native::Mul, Sym::Int(x), Sym::Int(y)) => Sym::Int(x * y),
                    (Native::Mul, Sym::Int(x), Sym::Float(y)) => Sym::Float(r64(x as _) * y),
                    (Native::Mul, Sym::Float(x), Sym::Int(y)) => Sym::Float(x * r64(y as _)),
                    (Native::Mul, Sym::Float(x), Sym::Float(y)) => Sym::Float(x * y),

                    (Native::Div, Sym::Int(x), Sym::Int(y)) => Sym::Int(x / y),
                    (Native::Div, Sym::Int(x), Sym::Float(y)) => Sym::Float(r64(x as _) / y),
                    (Native::Div, Sym::Float(x), Sym::Int(y)) => Sym::Float(x / r64(y as _)),
                    (Native::Div, Sym::Float(x), Sym::Float(y)) => Sym::Float(x / y),

                    (Native::Eq, x, y) => Sym::Int((x == y) as _),

                    (Native::Less, Sym::Int(x), Sym::Int(y)) => Sym::Int((x < y) as _),
                    (Native::Less, Sym::Int(x), Sym::Float(y)) => Sym::Int((r64(x as _) < y) as _),
                    (Native::Less, Sym::Float(x), Sym::Int(y)) => Sym::Int((x < r64(y as _)) as _),
                    (Native::Less, Sym::Float(x), Sym::Float(y)) => Sym::Int((x < y) as _),

                    (_, a, b) => {
                        self.stack.push(b);
                        self.stack.push(a);
                        return Err("operation is undefined".into());
                    }
                };

                self.stack.push(c);
            }

            Native::Floor => {
                pop_stack!(self, num);

                match num {
                    Sym::Int(_) => self.stack.push(num),
                    Sym::Float(x) => self.stack.push(Sym::Int(x.floor().raw() as _)),
                    _ => {
                        self.stack.push(num);
                        return Err("operation is undefined".into());
                    }
                }
            }

            Native::Exit => {
                let code = match self.stack.pop() {
                    Some(Sym::Int(n)) => n as _,
                    _ => -1,
                };

                process::exit(code)
            }
        }

        Ok(())
    }

    fn macro_replace(&mut self, sym: Sym) -> Sym {
        if !self.macros.contains_key(&sym) || matches!(sym, Sym::Defer(_)) {
            return sym;
        }

        let mut next = sym.clone();
        let mut iter = 0;

        while let Some(val) = self.macros.get(&next) {
            next = val.clone();
            iter += 1;

            debug_assert_ne!(sym, next);
        }

        if iter > 1 {
            self.macros.insert(sym, next.clone());
        }

        next
    }

    pub fn done(&self) -> bool {
        self.work.is_empty()
    }

    pub fn step<R, W>(&mut self, input: &mut R, output: &mut W) -> Result<(), String>
    where
        R: Read,
        W: Write,
    {
        let sym_opt = self.work.pop_front().map(|old| self.macro_replace(old));

        let sym = match sym_opt {
            Some(s) => s,
            None => return Ok(()),
        };

        match sym {
            Sym::Native(n) => {
                if let e @ Err(_) = self.eval_native(n, input, output) {
                    self.work.push_front(sym);
                    return e;
                }
            }
            Sym::Block(s) => {
                s.iter()
                    .rev()
                    .for_each(|sym| self.work.push_front(sym.clone()));
            }
            Sym::Defer(s) => self.stack.push(*s),
            s => self.stack.push(s),
        }

        Ok(())
    }

    pub fn run<R, W>(&mut self, input: &mut R, output: &mut W) -> Result<(), String>
    where
        R: Read,
        W: Write,
    {
        while !self.done() {
            self.step(input, output)?;
        }

        Ok(())
    }
}

#[allow(dead_code)]
fn eval<R, W>(prog: Vec<Sym>, input: &mut R, output: &mut W) -> Result<(), String>
where
    R: Read,
    W: Write,
{
    let mut eval = Evaluator::with_std();
    eval.extend_program(prog);
    eval.run(input, output)?;
    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io::*;

    fn eval_helper(code: &str, expected: &str) {
        let prog = parse(lex(code)).unwrap();
        let mut output = Vec::new();
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
        let code = "\
        (fib =' ;{;n = 0 1 (n >' 0) ;{xch over + (;n =' (n -' 1)) (n !=' 0)} while pop})
        (put (fib 42))";
        eval_helper(code, "267914296");
    }

    #[test]
    fn eval_gcd() {
        let code = "\
        (gcd =' ;{1 ;{(copy 2) < ;xch if over xch - (0 !=' over)} while xch pop})
        (put (35 gcd' 91))";
        eval_helper(code, "7");
    }

    #[test]
    fn eval_power() {
        let code = "\
        (pow =' ;{;n = ;k = (k >' 1) ;((n pow' (k -' 1)) *' n) ;n ?})
        (0.9 pow' 100) put";
        eval_helper(code, "0.000026561398887587544");
    }

    #[test]
    fn eval_do_do_do() {
        let code = "test ;;put do ;;do ;do do do";
        eval_helper(code, "test");
    }

    #[test]
    fn eval_quick_maths() {
        let code = "put (9 -' ((3 +' 1) *' 2))'";
        eval_helper(code, "1");
    }

    #[test]
    fn eval_macro_simple() {
        let code = "(5 =' 3) (n =' 5) (put n)";
        eval_helper(code, "3");
    }

    #[test]
    fn eval_macro_circle() {
        let code = "(5 =' 3) (3 =' 5) (put 3) (put 5)";
        eval_helper(code, "33");
    }

    #[test]
    fn eval_macro_override() {
        let code = "(2 =' 1) (2 =' 3) (1 =' 2) (put 1)";
        eval_helper(code, "3");
    }

    #[test]
    fn eval_macro_blocks() {
        let code = "0 1 5 2 3 7 9 ;{ dup put ;b if } b = b";
        eval_helper(code, "9732510");
    }

    #[test]
    fn eval_while_loop() {
        let code = "0 1 1 1 1 ;(put a) while";
        eval_helper(code, "aaaa");
    }

    #[test]
    fn eval_ternary() {
        let code = "\
        (test =' ;{== ;a ;b ? put})
        (test 3 5) (test 4 4)";
        eval_helper(code, "ba");
    }
}
