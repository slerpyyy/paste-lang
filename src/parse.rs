use crate::lex::*;
use noisy_float::prelude::{r64, R64};
use std::{collections::HashSet, fmt, rc::Rc, str::FromStr};

macro_rules! impl_native_enum {
    ($($tok: ident => $text: literal,)+) => {
        #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
        pub enum Native {
            $($tok,)+
        }

        impl FromStr for Native {
            type Err = ();
            fn from_str(string: &str) -> Result<Self, Self::Err> {
                match string {
                    $($text => Ok(Native::$tok),)+
                    _ => Err(()),
                }
            }
        }

        impl Native {
            /// Returns the representation of a native symbol.
            ///
            /// A call to this method always results in a static str.
            ///
            /// # Examples
            /// ```
            /// # use paste::parse::Native;
            /// let nat = Native::Assign;
            ///
            /// assert_eq!(nat.to_str(), "=");
            /// ```
            #[inline]
            pub fn to_str(&self) -> &'static str {
                match self {
                    $(Native::$tok => $text,)+
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
    Exit => "exit",
);

impl fmt::Display for Native {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum Sym {
    Native(Native),
    Int(i64),
    Float(R64),
    Text(Rc<str>),
    Block(Rc<[Sym]>),
    Defer(Box<Sym>),
}

impl Sym {
    #[inline]
    pub fn text(s: impl Into<Rc<str>>) -> Self {
        Self::Text(s.into())
    }
}

impl fmt::Display for Sym {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Sym::Native(n) => write!(f, "{}", n.to_str()),
            Sym::Int(s) => write!(f, "{}", s),
            Sym::Float(s) => write!(f, "{}", s),
            Sym::Text(s) => {
                if s.contains(' ') {
                    write!(f, "\"{}\"", s)
                } else {
                    write!(f, "{}", s)
                }
            }
            Sym::Block(s) => {
                write!(f, "{{ ")?;
                for sym in s.iter() {
                    write!(f, "{} ", sym)?;
                }
                write!(f, "}}")
            }
            Sym::Defer(s) => write!(f, ";{}", *s),
        }
    }
}

/// Checks if a token stream is complete and ready to parse.
///
/// This function runs through the given token iterator and checks if every
/// opening bracket has a closing counterpart. Inversely, if the function
/// returns false, the token stream is not be complete and must be continued
/// to be able to parse correctly. The function may also result in an error,
/// if brackets are placed incorrectly.
///
/// # Examples
/// ```
/// # use paste::{lex::*, parse::*};
/// let unfinished = check_complete(lex("t ;(put hello"));
/// let complete = check_complete(lex("t ;(put hello) ="));
/// let invalid = check_complete(lex("t ;(put hello} ="));
///
/// assert_eq!(unfinished, Ok(false));
/// assert_eq!(complete, Ok(true));
/// assert!(invalid.is_err());
/// ```
pub fn check_complete<'a>(tokens: impl Iterator<Item = Token<'a>>) -> Result<bool, &'static str> {
    let mut stack = Vec::<bool>::new();

    for token in tokens {
        match token {
            Token::LeftCurly | Token::LeftParen => {
                let curly = token == Token::LeftCurly;
                stack.push(curly)
            }
            Token::RightCurly | Token::RightParen => {
                let curly = token == Token::RightCurly;
                match stack.pop() {
                    None =>
                        return Err("too many closing brackets"),
                    Some(b) if b != curly =>
                        return Err("wrong closing brackets"),
                    _ => {},
                }
            }
            _ => {}
        }
    }

    Ok(stack.is_empty())
}

/// Parses a given token stream.
///
/// It converts a given token iterator into a vector of symbols, which is
/// ready to be evaluated.
pub fn parse<'a>(tokens: impl Iterator<Item = Token<'a>>) -> Result<Vec<Sym>, &'static str> {
    let mut stack = vec![(Vec::new(), 0, false)];
    let mut symbol_cache = HashSet::<Rc<str>>::new();

    for token in tokens {
        let (out, defer_level, _) = stack.last_mut().ok_or("too many closing brackets")?;

        match token {
            Token::Text(s) => {
                let sym = if let Ok(n) = Native::from_str(s) {
                    Sym::Native(n)
                } else if let Some(cached) = symbol_cache.get(s) {
                    Sym::Text(cached.clone())
                } else {
                    let s: Rc<str> = s.into();
                    symbol_cache.insert(s.clone());
                    Sym::Text(s)
                };
                out.push(sym);
            }
            Token::Int(i) => {
                out.push(Sym::Int(i));
            }
            Token::Float(f) => {
                out.push(Sym::Float(r64(f)));
            }
            Token::SemiColon => {
                *defer_level += 1;
                continue;
            }
            Token::LeftCurly | Token::LeftParen => {
                let reverse = token == Token::LeftParen;
                stack.push((Vec::new(), 0, reverse));
                continue;
            }
            Token::RightCurly | Token::RightParen => {
                if *defer_level > 0 {
                    return Err("nothing to defer");
                }

                // SAFETY: The case that the stack is empty is caught
                // in the beginning of the loop
                let (mut inner, _, reverse) = stack.pop().unwrap();

                if reverse != (token == Token::RightParen) {
                    return Err("wrong closing brackets");
                }

                if reverse {
                    inner.reverse();
                }

                stack
                    .last_mut()
                    .ok_or("closing bracket without a friend")?
                    .0
                    .push(Sym::Block(inner.into()));
            }
            Token::Tick => {
                let sym = out.pop().ok_or("tick without a symbol")?;
                out.insert(0, sym);
            }
        }

        let (out, defer_level, _) = stack.last_mut().ok_or("too many closing brackets")?;

        if *defer_level > 0 {
            for _ in 0..*defer_level {
                // SAFETY: The unwrap is only reached after an sym has
                // been pushed, because the loop is continued otherwise
                let inner = Box::new(out.pop().unwrap());
                out.push(Sym::Defer(inner));
            }
            *defer_level = 0;
        }
    }

    let (prog, trailing_defer, _) = stack.pop().ok_or("opening bracket without a friend")?;

    if trailing_defer > 0 {
        return Err("trailing defer");
    }

    if !stack.is_empty() {
        return Err("too many opening brackets");
    }

    Ok(prog)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn check_complete_true() {
        let lexer = lex("test ;{ (;{ put } hello) do } =");
        let result = check_complete(lexer);
        assert_eq!(result, Ok(true));
    }

    #[test]
    fn check_complete_false() {
        let lexer = lex("test ;{ (;{ put } hello");
        let result = check_complete(lexer);
        assert_eq!(result, Ok(false));
    }

    #[test]
    fn check_complete_too_many_closing() {
        let lexer = lex("{{b} {a}} c}");
        assert!(parse(lexer).is_err());
    }

    #[test]
    fn check_complete_bracket_mismatch() {
        let lexer = lex("( ;a put }");
        assert!(parse(lexer).is_err());
    }

    #[test]
    fn check_complete_sub_zero() {
        let lexer = lex("{ab}} {{cd}");
        assert!(parse(lexer).is_err());
    }

    #[test]
    fn parse_simple() {
        let lexer = lex("test { \"hello\" { ;put } do }");
        let prog = parse(lexer).unwrap();
        let result = format!("{:?}", prog);
        assert_eq!(
            result,
            "[Text(\"test\"), \
            Block([Text(\"hello\"), Block([Defer(Native(Put))]), \
            Native(Do)])]"
        );
    }

    #[test]
    fn parse_deferred_blocks() {
        let lexer = lex("1 ;{ ;{ hello } do ;put do } do");
        let prog = parse(lexer).unwrap();
        let result = format!("{:?}", prog);
        assert_eq!(
            result,
            "[Int(1), \
            Defer(Block([Defer(Block([Text(\"hello\")])), Native(Do), \
            Defer(Native(Put)), Native(Do)])), Native(Do)]"
        );
    }

    #[test]
    fn parse_tick_simple() {
        let lexer = lex("3 4' (2 +' 3)'' * +");
        let prog = parse(lexer).unwrap();
        let result = format!("{:?}", prog);
        assert_eq!(
            result,
            "[Int(3), Block([Int(3), Int(2), \
            Native(Add)]), Int(4), Native(Mul), Native(Add)]"
        );
    }

    #[test]
    fn parse_paren_simple() {
        let lexer = lex("(1 +' (2 *' 3)) put");
        let prog = parse(lexer).unwrap();
        let result = format!("{:?}", prog);
        assert_eq!(
            result,
            "[Block([Block([Int(3), Int(2), \
            Native(Mul)]), Int(1), Native(Add)]), Native(Put)]"
        );
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
    fn parse_brackets_too_many_closing() {
        let lexer = lex("{{b} {a}} c}");
        assert!(parse(lexer).is_err());
    }

    #[test]
    fn parse_brackets_bracket_mismatch() {
        let lexer = lex("( ;a put }");
        assert!(parse(lexer).is_err());
    }

    #[test]
    fn parse_brackets_sub_zero() {
        let lexer = lex("{ab}} {{cd}");
        assert!(parse(lexer).is_err());
    }
}
