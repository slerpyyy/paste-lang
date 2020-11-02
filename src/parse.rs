use crate::lex::*;
use noisy_float::prelude::{r64, R64};
use std::cmp::Ordering;
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
            /// # use paste_lang::Native;
            /// let nat = Native::Assign;
            ///
            /// assert_eq!(nat.to_str(), "=");
            /// ```
            #[inline] #[must_use]
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
    Comma => ",",
    Do => "do",
    Defer => "defer",
    If => "if",
    While => "while",
    Tern => "?",
    Copy => "copy",
    Xch => "xch",
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
    Block(Vec<Sym>),
    Defer(Box<Sym>),
}

impl PartialOrd for Sym {
    fn partial_cmp(&self, rhs: &Self) -> Option<Ordering> {
        match (self, rhs) {
            (Sym::Native(x), Sym::Native(y)) => x.to_str().partial_cmp(y.to_str()),
            (Sym::Native(x), Sym::Int(y)) => x.to_str().partial_cmp(y.to_string().as_str()),
            (Sym::Native(x), Sym::Float(y)) => x.to_str().partial_cmp(y.to_string().as_str()),
            (Sym::Native(x), Sym::Text(y)) => x.to_str().partial_cmp(y),
            (Sym::Int(x), Sym::Native(y)) => x.to_string().as_str().partial_cmp(y.to_str()),
            (Sym::Int(x), Sym::Int(y)) => x.partial_cmp(y),
            (Sym::Int(x), Sym::Float(y)) => r64(*x as _).partial_cmp(y),
            (Sym::Int(x), Sym::Text(y)) => x.to_string().as_str().partial_cmp(y),
            (Sym::Float(x), Sym::Native(y)) => x.to_string().as_str().partial_cmp(y.to_str()),
            (Sym::Float(x), Sym::Int(y)) => x.partial_cmp(&r64(*y as _)),
            (Sym::Float(x), Sym::Float(y)) => x.partial_cmp(y),
            (Sym::Float(x), Sym::Text(y)) => x.to_string().as_str().partial_cmp(&y),
            (Sym::Text(x), Sym::Native(y)) => x.as_ref().partial_cmp(y.to_str()),
            (Sym::Text(x), Sym::Int(y)) => x.as_ref().partial_cmp(y.to_string().as_str()),
            (Sym::Text(x), Sym::Float(y)) => x.as_ref().partial_cmp(y.to_string().as_str()),
            (Sym::Text(x), Sym::Text(y)) => x.partial_cmp(y),
            _ => None,
        }
    }
}

impl Sym {
    #[inline]
    pub fn text(s: impl Into<Rc<str>>) -> Self {
        Self::Text(s.into())
    }

    #[inline]
    pub fn concat(a: impl fmt::Display, b: impl fmt::Display) -> Self {
        Sym::text(format!("{}{}", a, b))
    }

    #[inline]
    pub fn int(n: impl Into<i64>) -> Self {
        Sym::Int(n.into())
    }

    #[inline]
    pub fn float(f: impl Into<f64>) -> Self {
        Sym::Float(r64(f.into()))
    }

    #[inline]
    fn interned_str(s: &str, cache: &mut HashSet<Rc<str>>) -> Self {
        if let Ok(n) = Native::from_str(s) {
            Sym::Native(n)
        } else if let Some(cached) = cache.get(s) {
            Sym::Text(cached.clone())
        } else {
            let s: Rc<str> = s.into();
            cache.insert(s.clone());
            Sym::text(s)
        }
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
/// # use paste_lang::*;
/// let unfinished = check_complete(lex("t ;(put hello"));
/// let complete = check_complete(lex("t ;(put hello) ="));
/// let invalid = check_complete(lex("t ;{put hello) ="));
///
/// assert_eq!(unfinished, Ok(false));
/// assert_eq!(complete, Ok(true));
/// assert!(invalid.is_err());
/// ```
///
/// # Errors
/// If the given piece of code cannot be completed to valid paste code,
/// a string describing the problem is returned.
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
                    None => return Err("too many closing brackets"),
                    Some(b) if b != curly => return Err("wrong closing brackets"),
                    Some(_) => {}
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
///
/// # Errors
/// If the given piece of code cannot be completed to valid paste code,
/// a string describing the problem is returned.
pub fn parse<'a>(tokens: impl Iterator<Item = Token<'a>>) -> Result<Vec<Sym>, &'static str> {
    let mut stack = vec![(Vec::new(), 0_u32, false)];
    let mut symbol_cache = HashSet::<Rc<str>>::new();

    for token in tokens {
        let (out, defer_level, _) = stack.last_mut().ok_or("too many closing brackets")?;

        match token {
            Token::Str(s) => {
                out.push(Sym::interned_str(s, &mut symbol_cache));
            }

            Token::String(s) => {
                out.push(Sym::interned_str(s.as_str(), &mut symbol_cache));
            }

            Token::Int(i) => {
                out.push(Sym::int(i));
            }

            Token::Float(f) => {
                out.push(Sym::float(f));
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
                    .push(Sym::Block(inner));
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
