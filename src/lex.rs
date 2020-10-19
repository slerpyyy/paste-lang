use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum Token<'a> {
    Raw(&'a str),
    String(String),
    Int(i64),
    Float(f64),
    LeftCurly,
    RightCurly,
    LeftParen,
    RightParen,
    SemiColon,
    Tick,
}

#[derive(Debug, Clone)]
pub struct Lexer<'a> {
    input: Chars<'a>,
    curr: Option<char>,
    mark: &'a str,
}

impl<'a> Lexer<'a> {
    #[inline]
    fn read_next(&mut self) {
        self.mark = self.input.as_str();
        self.curr = self.input.next();
    }

    #[inline]
    fn whitespace(&mut self) {
        while let Some(ch) = self.curr {
            if !ch.is_whitespace() {
                return;
            }

            self.read_next();
        }
    }

    #[inline]
    fn comments(&mut self) {
        while let Some('#') = self.curr {
            loop {
                if let Some('\n') = self.curr {
                    self.whitespace();
                    break;
                }

                if self.curr.is_none() {
                    break;
                }

                self.read_next();
            }
        }
    }

    #[inline]
    fn single(&mut self, tok: Token<'a>) -> Option<Token<'a>> {
        self.read_next();
        Some(tok)
    }

    #[inline]
    fn munch<P>(&mut self, predicate: P) -> Option<Token<'a>>
    where
        P: Fn(char) -> bool,
    {
        let start = self.mark;
        let mut len = 0;

        while let Some(ch) = self.curr {
            if !predicate(ch) {
                break;
            }

            self.read_next();
            len += 1;
        }

        let text = &start[..len];
        Self::num_parse(text).or_else(|| Some(Token::Raw(text)))
    }

    #[inline]
    fn string(&mut self) -> Option<Token<'a>> {
        self.read_next();
        let start = self.mark;
        let mut len = 0;

        while let Some(c) = self.curr {
            if c == '"' {
                break;
            }

            if c == '\\' {
                self.read_next();
                len += 1;
            }

            self.read_next();
            len += 1;
        }

        self.read_next();
        let raw = &start[..len];
        if let num @ Some(_) = Self::num_parse(raw) {
            return num;
        }

        Some(Token::String(
            raw.replace("\\r", "\r")
                .replace("\\t", "\t")
                .replace("\\\\", "\\")
                .replace("\\'", "\'")
                .replace("\\\"", "\"")
                .replace("\\n", "\n"),
        ))
    }

    #[inline]
    fn num_parse(raw: &'a str) -> Option<Token<'a>> {
        let int = raw.parse::<i64>().ok().map(Token::Int);
        let float = || raw.parse::<f64>().ok().map(Token::Float);
        int.or_else(float)
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Token<'a>> {
        self.whitespace();
        self.comments();

        match self.curr? {
            '{' => self.single(Token::LeftCurly),
            '}' => self.single(Token::RightCurly),
            '(' => self.single(Token::LeftParen),
            ')' => self.single(Token::RightParen),
            ';' => self.single(Token::SemiColon),
            '\'' => self.single(Token::Tick),
            '"' => self.string(),
            _ => self.munch(|ch| {
                !ch.is_whitespace() && !matches!(ch, '{' | '}' | '(' | ')' | '"' | ';' | '\'' | '#')
            }),
        }
    }
}

/// Lexes a given str.
///
/// This function takes a &str containing paste source code and returns a
/// token stream, which is ready to be parsed.
///
/// # Examples
/// ```
/// # use paste_lang::lex::*;
/// let mut tokens = lex("{ \"hello\" ;put do } test =");
///
/// assert_eq!(tokens.next(), Some(Token::LeftCurly));
/// assert_eq!(tokens.next(), Some(Token::String("hello".into())));
/// assert_eq!(tokens.next(), Some(Token::SemiColon));
/// assert_eq!(tokens.next(), Some(Token::Raw("put")));
/// assert_eq!(tokens.next(), Some(Token::Raw("do")));
/// assert_eq!(tokens.next(), Some(Token::RightCurly));
/// assert_eq!(tokens.next(), Some(Token::Raw("test")));
/// assert_eq!(tokens.next(), Some(Token::Raw("=")));
/// assert_eq!(tokens.next(), None);
/// ```
pub fn lex(source: &'_ str) -> Lexer<'_> {
    let mut lexer = Lexer {
        input: source.chars(),
        curr: None,
        mark: source,
    };
    lexer.read_next();
    lexer
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn lex_simple() {
        let code = "test # abc\n { \"hello\" ;put do }";
        let res = lex(code).collect::<Vec<_>>();
        assert_eq!(
            res,
            vec![
                Token::Raw("test"),
                Token::LeftCurly,
                Token::String("hello".into()),
                Token::SemiColon,
                Token::Raw("put"),
                Token::Raw("do"),
                Token::RightCurly,
            ]
        );
    }

    #[test]
    fn lex_squished() {
        let code = "aaa;{test{hello};put do}do";
        let res = lex(code).collect::<Vec<_>>();
        assert_eq!(
            res,
            vec![
                Token::Raw("aaa"),
                Token::SemiColon,
                Token::LeftCurly,
                Token::Raw("test"),
                Token::LeftCurly,
                Token::Raw("hello"),
                Token::RightCurly,
                Token::SemiColon,
                Token::Raw("put"),
                Token::Raw("do"),
                Token::RightCurly,
                Token::Raw("do"),
            ]
        );
    }

    #[test]
    fn lex_comments() {
        let code = "# comment 1\n# comment 2\naaa# comment 3\n bbb #comment 4";
        let res = lex(code).collect::<Vec<_>>();
        assert_eq!(res, vec![Token::Raw("aaa"), Token::Raw("bbb")]);
    }

    #[test]
    fn lex_string_simple() {
        let code = "\"Hello there!\" put";
        let res = lex(code).collect::<Vec<_>>();
        assert_eq!(
            res,
            vec![Token::String("Hello there!".into()), Token::Raw("put")]
        );
    }

    #[test]
    fn lex_string_escape() {
        let code = r#""this\tis\'\na\\\"test\"""#;
        let res = lex(code).collect::<Vec<_>>();
        assert_eq!(res, vec![Token::String("this\tis\'\na\\\"test\"".into())]);
    }
}
