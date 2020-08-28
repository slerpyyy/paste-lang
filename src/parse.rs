use std::fmt;
use std::hash::*;
use noisy_float::prelude::*;

use crate::lex::*;

macro_rules! impl_native_enum {
    ($($tok: ident => $text: literal),*) => {
        #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
        pub enum Native {
            $($tok,)*
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
    Exit => "exit"
);

impl fmt::Display for Native {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Obj {
    Native(Native),
    Int(i64),
    Float(R64),
    Text(String),
    Block(Vec<Obj>),
    Defer(Box<Obj>),
}

impl fmt::Display for Obj {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Obj::Native(n) => write!(f, "{}", n.to_str()),
            Obj::Int(s) => write!(f, "{}", s),
            Obj::Float(s) => write!(f, "{}", s),
            Obj::Text(s) => {
                if s.contains(' ') {
                    write!(f, "\"{}\"", s)
                } else {
                    write!(f, "{}", s)
                }
            },
            Obj::Block(s) => {
                write!(f, "{{ ")?;
                for obj in s {
                    write!(f, "{} ", obj)?;
                }
                write!(f, "}}")
            },
            Obj::Defer(s) => write!(f, ";{}", *s),
        }
    }
}

pub fn parse(lexer: Lexer) -> Result<Vec<Obj>, &'static str> {
    let base = (Vec::new(), 0, false);
    let mut stack = vec![base];

    for token in lexer {
        let (out, defer_level, _) = stack.last_mut()
            .ok_or("too many closing bois")?;

        match token {
            Token::Text(s) => {
                if let Some(n) = Native::from_str(s) {
                    out.push(Obj::Native(n));
                } else {
                    out.push(Obj::Text(s.to_string()));
                }
            },
            Token::Int(i) => {
                out.push(Obj::Int(i));
            },
            Token::Float(f) => {
                out.push(Obj::Float(r64(f)));
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
                    if inner.len() > 2 {
                        inner.swap(0, 1);
                    }
                    inner.reverse();
                }

                stack.last_mut()
                    .ok_or("closing boi without a friend")?
                    .0.push(Obj::Block(inner));
            },
        }

        let (out, defer_level, _) = stack.last_mut()
            .ok_or("too many closing bois")?;

        if *defer_level > 0 {
            for _ in 0..*defer_level {
                // SAFETY: The unwrap is only reached after an Obj has been
                // pushed, because the loop is continued otherwise
                let inner = Box::new(out.pop().unwrap());
                out.push(Obj::Defer(inner));
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
        let lexer = lex("1 ;{ ;{ hello } do ;put do } do");
        let prog = parse(lexer).unwrap();
        let result = format!("{:?}", prog);
        assert_eq!(result, "[Int(1), \
            Defer(Block([Defer(Block([Text(\"hello\")])), Native(Do), \
            Defer(Native(Put)), Native(Do)])), Native(Do)]");
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
