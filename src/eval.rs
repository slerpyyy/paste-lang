use crate::lex::*;
use crate::parse::*;
use noisy_float::prelude::*;
use std::{
    collections::{HashMap, VecDeque},
    fmt,
    io::{Read, Write},
    process,
};

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
    ($self: ident, $($var: ident),*) => {
        $self.stack_assert(pop_stack!(@COUNT; $($var),*))?;
        $(let $var = $self.stack.pop().unwrap();)*
    };

    (@COUNT; $($var: ident),*) => {
        <[()]>::len(&[$(pop_stack!(@SUBST; $var)),*])
    };

    (@SUBST; $_: ident) => { () };
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl Evaluator {
    /// Creates a new empty evaluator.
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self {
            macros: HashMap::new(),
            stack: Vec::new(),
            work: VecDeque::new(),
        }
    }

    /// Creates a new evaluator with preloaded standard library.
    #[inline]
    #[must_use]
    pub fn with_std() -> Self {
        let mut new = Self::new();
        new.load_std();
        new
    }

    /// Loads and evaluated the standard library.
    #[inline]
    pub fn load_std(&mut self) {
        // SAFETY: The standard library does not change depending
        // on user input and is tested explicitly
        let std_lib = include_str!("std.paste");
        self.extend_code(std_lib).unwrap();
    }

    /// Extends the program to be evaluated.
    ///
    /// The program is given as a &str containing paste source code, which is
    /// parsed internally.
    ///
    /// # Errors
    /// Any errors produced during parsing are forwarded
    /// in the result.
    #[inline]
    pub fn extend_code(&mut self, source: &str) -> Result<(), String> {
        let prog = parse(lex(source))?;
        self.work.extend(prog);
        Ok(())
    }

    /// Extends the program to be evaluated.
    #[inline]
    pub fn extend_program(&mut self, prog: Vec<Sym>) {
        self.work.extend(prog);
    }

    // Asserts that the stack holds at least `len` elements.
    #[inline]
    fn stack_assert(&self, len: usize) -> Result<(), String> {
        if len > self.stack.len() {
            return Err(format!(
                "expected {} items on the stack, found {}",
                len, self.stack.len()
            ));
        }

        Ok(())
    }

    #[inline]
    fn eval_native(
        &mut self,
        sym: Native,
        _input: &mut dyn Read,
        output: &mut dyn Write,
    ) -> Result<(), String> {
        match sym {
            Native::Assign => {
                pop_stack!(self, key, val);
                if key != val && key != Sym::Text("_".into()) {
                    self.macros.insert(key, val);
                }
            }

            Native::Comma => {
                pop_stack!(self, x, y);
                self.stack.push(Sym::Block(vec![x, y]));
            }

            Native::Do => {
                pop_stack!(self, x);
                self.work.push_front(x);
            }

            Native::Defer => {
                pop_stack!(self, x);
                self.stack.push(Sym::Defer(x.into()));
            }

            Native::If => {
                pop_stack!(self, stmt, cond);

                match cond {
                    Sym::Int(0) => (),
                    Sym::Int(_) => self.work.push_front(stmt),
                    _ => {
                        self.stack.push(cond);
                        self.stack.push(stmt);
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

            Native::Copy => {
                pop_stack!(self, num);
                let len = self.stack.len();

                if let Sym::Int(amount) = num {
                    if amount < 0 || amount as usize > len {
                        self.stack.push(num);
                        return Err("invalid copy amount".into());
                    }

                    let amount = amount as _;
                    self.stack.reserve(amount);
                    for i in len.saturating_sub(amount)..len {
                        self.stack.push(self.stack[i].clone());
                    }
                } else {
                    self.stack.push(num);
                    return Err("invalid copy".into());
                }
            }

            Native::Xch => {
                self.stack_assert(2)?;
                let len = self.stack.len();
                self.stack.swap(len - 2, len - 1)
            }

            Native::Put => {
                pop_stack!(self, sym);

                let string;
                let buffer = match &sym {
                    Sym::Text(s) => s.as_ref(),
                    s => {
                        string = format!("{}", s);
                        &string
                    }
                };

                output
                    .write(buffer.as_bytes())
                    .map_err(|_| "failed to write to output")?;
            }

            Native::Add | Native::Sub | Native::Mul | Native::Div | Native::Eq | Native::Less => {
                pop_stack!(self, a, b);

                let c = match (sym, a, b) {
                    (Native::Add, Sym::Int(x), Sym::Int(y)) => Sym::int(x + y),
                    (Native::Add, Sym::Int(x), Sym::Float(y)) => Sym::Float(r64(x as _) + y),
                    (Native::Add, Sym::Float(x), Sym::Int(y)) => Sym::Float(x + r64(y as _)),
                    (Native::Add, Sym::Float(x), Sym::Float(y)) => Sym::Float(x + y),
                    (Native::Add, Sym::Block(mut x), Sym::Block(y)) => Sym::Block({x.extend(y); x}),
                    (Native::Add, Sym::Block(mut x), y) => Sym::Block({x.push(y); x}),
                    (Native::Add, x, Sym::Block(mut y)) => Sym::Block({y.insert(0, x); y}),
                    (Native::Add, x, y) => Sym::concat(x, y),

                    (Native::Sub, Sym::Int(x), Sym::Int(y)) => Sym::int(x - y),
                    (Native::Sub, Sym::Int(x), Sym::Float(y)) => Sym::Float(r64(x as _) - y),
                    (Native::Sub, Sym::Float(x), Sym::Int(y)) => Sym::Float(x - r64(y as _)),
                    (Native::Sub, Sym::Float(x), Sym::Float(y)) => Sym::Float(x - y),

                    (Native::Mul, Sym::Int(x), Sym::Int(y)) => Sym::int(x * y),
                    (Native::Mul, Sym::Int(x), Sym::Float(y)) => Sym::Float(r64(x as _) * y),
                    (Native::Mul, Sym::Float(x), Sym::Int(y)) => Sym::Float(x * r64(y as _)),
                    (Native::Mul, Sym::Float(x), Sym::Float(y)) => Sym::Float(x * y),

                    (Native::Div, Sym::Int(x), Sym::Int(y)) => Sym::int(x / y),
                    (Native::Div, Sym::Int(x), Sym::Float(y)) => Sym::Float(r64(x as _) / y),
                    (Native::Div, Sym::Float(x), Sym::Int(y)) => Sym::Float(x / r64(y as _)),
                    (Native::Div, Sym::Float(x), Sym::Float(y)) => Sym::Float(x / y),

                    (Native::Eq, x, y) => Sym::int(x == y),

                    (Native::Less, x, y) => Sym::int(x.lt(&y)),

                    (op, a, b) => {
                        let error = Err(format!(
                            "operation `{}` is undefined for `{}` and `{}`",
                            op, a, b
                        ));
                        self.stack.push(b);
                        self.stack.push(a);
                        return error;
                    }
                };

                self.stack.push(c);
            }

            Native::Floor => {
                self.stack_assert(1)?;

                // SAFETY: The unwrap cannot panic because the
                // stack_assert above fails if the stack is empty.
                let s = self.stack.last_mut().unwrap();
                match s {
                    Sym::Int(_) => (),
                    Sym::Float(x) => *s = Sym::Int(x.floor().raw() as _),
                    _ => return Err("operation is undefined".into()),
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

    #[inline]
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

    /// Checks if the evaluator has terminated.
    ///
    /// The function returns true if no more symbols are left
    /// in the work queue and false otherwise.
    #[inline]
    #[must_use]
    pub fn done(&self) -> bool {
        self.work.is_empty()
    }

    /// Advances the state of the evaluator by one step.
    ///
    /// # Errors
    /// This function may terminate in an error if the current symbol
    /// in the working queue could not be evaluated. This may happen if too
    /// few symbols or the wrong kind of symbols are on the stack.
    pub fn step(&mut self, input: &mut dyn Read, output: &mut dyn Write) -> Result<(), String> {
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

    /// Runs the evaluator until the program terminates.
    ///
    /// # Errors
    /// This function forwards all errors from [`step`].
    #[inline]
    pub fn run(&mut self, input: &mut dyn Read, output: &mut dyn Write) -> Result<(), String> {
        while !self.done() {
            self.step(input, output)?;
        }

        Ok(())
    }

    /// Runs the evaluator until it terminates or the timeout is reached.
    ///
    /// # Errors
    /// This function forwards all errors from [`step`].
    #[inline]
    pub fn run_with_timeout(
        &mut self,
        input: &mut dyn Read,
        output: &mut dyn Write,
        steps: usize,
    ) -> Result<usize, String> {
        for curr in 0..steps {
            if self.done() {
                return Ok(curr);
            }
            self.step(input, output)?;
        }

        Ok(steps)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::io;
    use std::str;

    #[inline]
    fn eval_helper(code: &str, expected: &str) {
        let prog = parse(lex(code)).unwrap();
        let mut output = Vec::new();

        let mut eval = Evaluator::with_std();
        eval.run(&mut io::empty(), &mut io::sink()).unwrap();

        eval.extend_program(prog);
        eval.run_with_timeout(&mut io::empty(), &mut output, 100_000)
            .unwrap();
        assert!(eval.done(), "Evaluator wasn't done yet\n{}", eval);

        let result = str::from_utf8(output.as_slice()).unwrap();
        assert_eq!(result, expected);
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
        (1.01 pow' 100) put";
        eval_helper(code, "2.7048138294215294");
    }

    #[test]
    fn eval_do_do_do() {
        let code = "test ;;put do ;;do ;do do do";
        eval_helper(code, "test");
    }

    #[test]
    fn eval_do_rep_chain() {
        let code = "0 ;{ x put do } 10 rep do";
        eval_helper(code, "xxxxxxxxxx");
    }

    #[test]
    fn eval_quick_maths() {
        let code = "put (9 -' ((3 +' 1) *' 2))'";
        eval_helper(code, "1");
    }

    #[test]
    fn eval_comp_ass() {
        let code = "(;n =' 5) (;n +=' 3) \
        (;n /=' 4) (;n *=' 3) (;n -=' 4) (put n)";
        eval_helper(code, "2");
    }

    #[test]
    fn eval_and_or_exhaustive() {
        let code = "\
        (0 &&' 0) put (0 &&' 1) put (1 &&' 0) put (1 &&' 1) put \
        (0 ||' 0) put (0 ||' 1) put (1 ||' 0) put (1 ||' 1) put ";
        eval_helper(code, "00010111");
    }

    #[test]
    fn eval_and_or_lazy() {
        let code = "\
        (0 &&' ;{a put 1}) put \
        (1 &&' ;{b put 1}) put \
        (0 ||' ;{c put 0}) put \
        (1 ||' ;{d put 0}) put";
        eval_helper(code, "0b1c01");
    }

    #[test]
    fn eval_and_lazy_chain() {
        let code = "(n =' 5) \
        ((n >' 0 put a) &&' ;(n <' 10 put b) &&' ;(n ==' 3 put c) &&' ;(1 put d)) put";
        eval_helper(code, "abc0");
    }

    #[test]
    fn eval_floor_simple() {
        let code = "0 \
        (3.0 2.3 4.5 -7.3 1.7 4.999 5.0) \
        { 1 ;(put dup floor) while }";
        eval_helper(code, "324-81450");
    }

    #[test]
    fn eval_add_all_the_things() {
        let code = "(+ + + + 1 ;+ 2 a (+ + 5.2 b 2)) put";
        eval_helper(code, "1+2a5.2b2");
    }

    #[test]
    fn eval_add_blocks() {
        let code = "(put + 1 + + , 2 3 ;{4 5} 6)";
        eval_helper(code, "{ 1 2 3 4 5 6 }");
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
    fn eval_twice_twice() {
        let code = "\
        0 5 4 (copy 2) 7 6 (copy 3)
        1 ;{dup put} while";
        eval_helper(code, "6746745450");
    }

    #[test]
    fn eval_ternary() {
        let code = "\
        (test =' ;{== ;a ;b ? put})
        (test 3 5) (test 4 4)";
        eval_helper(code, "ba");
    }
}
