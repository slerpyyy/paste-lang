use std::str::Chars;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Token<'a> {
    Text(&'a str),
    Int(i64),
    Float(f64),
    LeftCurly,
    RightCurly,
    LeftParen,
    RightParen,
    SemiColon,
    Tick,
}

pub struct Lexer<'a> {
    input: Chars<'a>,
    curr: Option<char>,
    mark: &'a str,
}

impl<'a> Lexer<'a> {
    fn read_next(&mut self) {
        self.mark = self.input.as_str();
        self.curr = self.input.next();
    }

    fn whitespace(&mut self) {
        while let Some(ch) = self.curr {
            if !ch.is_whitespace() {
                return;
            }

            self.read_next();
        }
    }

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

    fn single(&mut self, tok: Token<'a>) -> Option<Token<'a>> {
        self.read_next();
        Some(tok)
    }

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

        Self::finalize(&start[..len])
    }

    fn string(&mut self) -> Option<Token<'a>> {
        self.read_next();
        let tok_opt = self.munch(|c| c != '"');
        self.read_next();
        tok_opt
    }

    fn finalize(raw: &'a str) -> Option<Token<'a>> {
        let int = raw.parse::<i64>().ok().map(Token::Int);
        let float = raw.parse::<f64>().ok().map(Token::Float);
        let text = Some(Token::Text(raw));

        int.or(float).or(text)
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
                Token::Text("test"),
                Token::LeftCurly,
                Token::Text("hello"),
                Token::SemiColon,
                Token::Text("put"),
                Token::Text("do"),
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
                Token::Text("aaa"),
                Token::SemiColon,
                Token::LeftCurly,
                Token::Text("test"),
                Token::LeftCurly,
                Token::Text("hello"),
                Token::RightCurly,
                Token::SemiColon,
                Token::Text("put"),
                Token::Text("do"),
                Token::RightCurly,
                Token::Text("do"),
            ]
        );
    }

    #[test]
    fn lex_comments() {
        let code = "# comment 1\n# comment 2\naaa# comment 3\n bbb #comment 4";
        let res = lex(code).collect::<Vec<_>>();
        assert_eq!(res, vec![Token::Text("aaa"), Token::Text("bbb"),]);
    }
}
