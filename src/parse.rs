use std::fmt;
use std::hash::*;
use noisy_float::prelude::*;

use crate::lex::*;

macro_rules! impl_native_enum {
    ($($tok: ident => $text: literal),*) => {
        #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
        pub enum Native {
            Assign, Do, If, Tern, While, Copy, Put,
            Add, Sub, Mul, Div, Eq, Less, Floor,
            Exit, Abort
        }

        impl Native {
            pub fn from_str(string: &str) -> Option<Native> {
                match string {
                    $($text => Some(Native::$tok),)*
                    _ => None,
                }
            }

            pub fn to_str(&self) -> &'static str {
                match self {
                    $(Native::$tok => $text,)*
                }
            }
        }
    };
}

impl_native_enum!(
    Assign => "=",
    Do => "do",
    If => "if",
    Tern => "?",
    While => "while",
    Copy => "copy",
    Put => "put",
    Add => "+",
    Sub => "-",
    Mul => "*",
    Div => "/",
    Eq => "==",
    Less => "<",
    Floor => "floor",
    Exit => "exit",
    Abort => "abort"
);

impl fmt::Display for Native {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Inst {
    Native(Native),
    Int(i32),
    Float(R32),
    Text(String),
    Block(Vec<Inst>),
    Defer(Box<Inst>),
}

impl fmt::Display for Inst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Inst::Native(n) => write!(f, "{}", n.to_str()),
            Inst::Int(s) => write!(f, "{}", s),
            Inst::Float(s) => write!(f, "{}", s),
            Inst::Text(s) => {
                if s.contains(' ') {
                    write!(f, "\"{}\"", s)
                } else {
                    write!(f, "{}", s)
                }
            },
            Inst::Block(s) => {
                write!(f, "{{ ")?;
                for inst in s {
                    write!(f, "{} ", inst)?;
                }
                write!(f, "}}")
            },
            Inst::Defer(s) => write!(f, ";{}", *s),
        }
    }
}

pub fn parse(lexer: Lexer) -> Result<Vec<Inst>, &'static str> {
    let base = (Vec::new(), 0, false);
    let mut stack = vec![base];

    for token in lexer {
        let (out, defer_level, _) = stack.last_mut()
            .ok_or("too many closing bois")?;

        match token {
            Token::Text(s) => {
                if let Some(n) = Native::from_str(s) {
                    out.push(Inst::Native(n));
                } else {
                    out.push(Inst::Text(s.to_string()));
                }
            },
            Token::Int(i) => {
                out.push(Inst::Int(i));
            },
            Token::Float(f) => {
                out.push(Inst::Float(r32(f)));
            },
            Token::SemiColon => {
                *defer_level += 1;
                continue;
            },
            Token::LeftCurly |
            Token::LeftParen => {
                let reorder = token == Token::LeftParen;
                stack.push((Vec::new(), 0, reorder));
                continue;
            },
            Token::RightCurly |
            Token::RightParen => {
                if *defer_level > 0 {
                    return Err("nothing to defer");
                }

                // SAFETY: The case that the stack is empty is caught
                // in the beginning of the loop
                let (mut inner, _, reorder) = stack.pop().unwrap();

                if reorder != (token == Token::RightParen) {
                    return Err("wrong closing bois");
                }

                if reorder {
                    // (exit) ~> { exit }
                    // (put hello) ~> { hello put }
                    // (4 - 3) ~> { 3 4 - }
                    // (1 2 3 4 5) ~> { 5 4 3 1 2 }
                    if inner.len() > 2 {
                        inner.swap(0, 1);
                    }
                    inner.reverse();
                }

                stack.last_mut()
                    .ok_or("closing boi without a friend")?
                    .0.push(Inst::Block(inner));
            },
        }

        let (out, defer_level, _) = stack.last_mut()
            .ok_or("too many closing bois")?;

        if *defer_level > 0 {
            for _ in 0..*defer_level {
                // SAFETY: The unwrap is only reached after an Inst has been
                // pushed, because the loop is continued otherwise
                let inner = Box::new(out.pop().unwrap());
                out.push(Inst::Defer(inner));
            }
            *defer_level = 0;
        }
    }

    let (prog, trailing_defer, _) = stack.pop()
        .ok_or("opening boi without a friend")?;

    if trailing_defer > 0 {
        return Err("trailing defer");
    }

    if !stack.is_empty() {
        return Err("too many opening bois");
    }

    Ok(prog)
}

#[cfg(test)]
mod test {
    use crate::lex::*;
    use super::*;

    #[test]
    fn parse_simple() {
        let lexer = lex("test { \"hello\" { ;put } do }");
        let prog = parse(lexer).unwrap();
        let result = format!("{:?}", prog);
        assert_eq!(result, "[Text(\"test\"), \
            Block([Text(\"hello\"), Block([Defer(Native(Put))]), \
            Native(Do)])]");
    }

    #[test]
    fn parse_deferred_blocks() {
        let lexer = lex("1 ;{ ;{ hello } do ;put do } if");
        let prog = parse(lexer).unwrap();
        let result = format!("{:?}", prog);
        assert_eq!(result, "[Int(1), \
            Defer(Block([Defer(Block([Text(\"hello\")])), Native(Do), \
            Defer(Native(Put)), Native(Do)])), Native(If)]");
    }

    #[test]
    fn parse_paren_simple() {
        let lexer = lex("(1 + (2 * 3)) put");
        let prog = parse(lexer).unwrap();
        let result = format!("{:?}", prog);
        assert_eq!(result, "[Block([Block([Int(3), Int(2), \
            Native(Mul)]), Int(1), Native(Add)]), Native(Put)]");
    }

    #[test]
    fn parse_block_trailing_defer() {
        let lexer = lex("a {;b ;}");
        assert!(parse(lexer).is_err());
    }

    #[test]
    fn parse_trailing_defer() {
        let lexer = lex(";a ;{b} ;");
        assert!(parse(lexer).is_err());
    }

    #[test]
    fn parse_brackets_not_closing() {
        let lexer = lex("{b} {a{c}");
        assert!(parse(lexer).is_err());
    }

    #[test]
    fn parse_brackets_not_opening() {
        let lexer = lex("{{b} {a}} c}");
        assert!(parse(lexer).is_err());
    }

    #[test]
    fn parse_brackets_mismatch() {
        let lexer = lex("( ;a put }");
        assert!(parse(lexer).is_err());
    }

    #[test]
    fn parse_brackets_sub_zero() {
        let lexer = lex("{ab}} {{cd}");
        assert!(parse(lexer).is_err());
    }
}
