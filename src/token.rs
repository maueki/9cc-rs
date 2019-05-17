use failure::{Error, Fail};
use lazy_static::lazy_static;
use std::collections::VecDeque;

#[derive(Fail, Debug)]
#[fail(display = "Tokenize Error: {}, pos: {}", _0, _1)]
struct TokenizeError(String, usize);

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Token {
    Num(i64),
    Op(OpType),
    Char(char), // single-letter symbol
    Ident(String),
    Return,
    If,
    Else,
    Eof,
    Sizeof,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum OpType {
    Eq,
    Ne,
    Le,
    Ge,
}

lazy_static! {
    pub static ref STR_TO_OP: Vec<(&'static str, OpType)> = {
        use OpType::*;
        let mut v = Vec::new();
        v.push(("==", Eq));
        v.push(("!=", Ne));
        v.push(("<=", Le));
        v.push((">=", Ge));
        v
    };
    pub static ref RESERVED: Vec<(&'static str, Token)> = {
        use Token::*;
        let mut v = Vec::new();
        v.push(("if", If));
        v.push(("else", Else));
        v.push(("return", Return));
        v.push(("sizeof", Sizeof));
        v
    };
}

pub type Tokens = VecDeque<(Token, usize)>;

fn is_alnum(c: char) -> bool {
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') || (c == '_')
}

pub fn tokenize(text: &str) -> Result<Tokens, Error> {
    let chars = text.chars().collect::<Vec<_>>();
    let mut pos = 0;
    let mut tokens = VecDeque::new();

    'outer: while pos < chars.len() {
        if chars[pos].is_whitespace() {
            pos += 1;
            continue;
        }

        for (word, token) in RESERVED.iter() {
            if chars[pos..].iter().collect::<String>().starts_with(word) {
                match chars.get(pos + word.len()) {
                    Some(c) if !is_alnum(*c) => {
                        tokens.push_back((token.clone(), pos));
                        pos += word.len();
                        continue 'outer;
                    }
                    _ => {}
                }
            }
        }

        for (sym, ty) in STR_TO_OP.iter() {
            if chars.len() < pos + sym.len() {
                continue;
            }

            if *sym
                == chars[pos..pos + sym.len()]
                    .iter()
                    .collect::<String>()
                    .as_str()
            {
                if let Some(..) = chars.get(pos + sym.len()) {
                    tokens.push_back((Token::Op(ty.clone()), pos));
                    pos += sym.len();
                    continue 'outer;
                }
            }
        }

        if let Some(..) = "+-*/;=(),{}<>[]&.!?:|^%~#"
            .chars()
            .find(|&c| c == chars[pos])
        {
            tokens.push_back((Token::Char(chars[pos]), pos));
            pos += 1;
            continue;
        }

        if chars[pos].is_digit(10) {
            let cs = chars[pos..]
                .iter()
                .take_while(|c| c.is_digit(10))
                .collect::<String>();
            tokens.push_back((Token::Num(cs.parse::<i64>().unwrap()), pos));
            pos += cs.len();
            continue;
        }

        if 'a' <= chars[pos] && chars[pos] <= 'z' {
            let ident: String = chars[pos..].iter().take_while(|&c| is_alnum(*c)).collect();
            let offset = ident.len();
            tokens.push_back((Token::Ident(ident), pos));
            pos += offset;
            continue;
        }

        return Err(TokenizeError(
            format!(
                "トークナイズできません: {}",
                chars[pos..].iter().collect::<String>()
            ),
            pos,
        )
        .into());
    }

    tokens.push_back((Token::Eof, pos));
    Ok(tokens)
}

#[cfg(test)]
mod test {
    use super::OpType::*;
    use super::*;

    #[test]
    fn tokenize_test() {
        use super::Token::*;

        assert_eq!(
            tokenize("1+1").unwrap(),
            vec![(Num(1), 0), (Char('+'), 1), (Num(1), 2), (Eof, 3)]
        );

        assert_eq!(
            tokenize("(3+5)/2").unwrap(),
            vec![
                (Char('('), 0),
                (Num(3), 1),
                (Char('+'), 2),
                (Num(5), 3),
                (Char(')'), 4),
                (Char('/'), 5),
                (Num(2), 6),
                (Eof, 7),
            ]
        );

        assert_eq!(
            tokenize("a=1;return a;").unwrap(),
            vec![
                (Ident("a".to_owned()), 0),
                (Char('='), 1),
                (Num(1), 2),
                (Char(';'), 3),
                (Return, 4),
                (Ident("a".to_owned()), 11),
                (Char(';'), 12),
                (Eof, 13),
            ]
        );

        assert_eq!(
            tokenize("5*(9-6)").unwrap(),
            vec![
                (Num(5), 0),
                (Char('*'), 1),
                (Char('('), 2),
                (Num(9), 3),
                (Char('-'), 4),
                (Num(6), 5),
                (Char(')'), 6),
                (Eof, 7),
            ]
        );

        assert_eq!(
            tokenize("a=1;if a==1 return 2;").unwrap(),
            vec![
                (Ident("a".to_owned()), 0),
                (Char('='), 1),
                (Num(1), 2),
                (Char(';'), 3),
                (If, 4),
                (Ident("a".to_owned()), 7),
                (Op(Eq), 8),
                (Num(1), 10),
                (Return, 12),
                (Num(2), 19),
                (Char(';'), 20),
                (Eof, 21),
            ]
        );

        assert_eq!(
            tokenize("a=1;if a==1 return 2; else return 3;").unwrap(),
            vec![
                (Ident("a".to_owned()), 0),
                (Char('='), 1),
                (Num(1), 2),
                (Char(';'), 3),
                (If, 4),
                (Ident("a".to_owned()), 7),
                (Op(Eq), 8),
                (Num(1), 10),
                (Return, 12),
                (Num(2), 19),
                (Char(';'), 20),
                (Else, 22),
                (Return, 27),
                (Num(3), 34),
                (Char(';'), 35),
                (Eof, 36),
            ]
        );

        assert_eq!(
            tokenize("a=1;if(a==1){return 2;}").unwrap(),
            vec![
                (Ident("a".to_owned()), 0),
                (Char('='), 1),
                (Num(1), 2),
                (Char(';'), 3),
                (If, 4),
                (Char('('), 6),
                (Ident("a".to_owned()), 7),
                (Op(Eq), 8),
                (Num(1), 10),
                (Char(')'), 11),
                (Char('{'), 12),
                (Return, 13),
                (Num(2), 20),
                (Char(';'), 21),
                (Char('}'), 22),
                (Eof, 23),
            ]
        );

        assert_eq!(
            tokenize("foo();").unwrap(),
            vec![
                (Ident("foo".to_owned()), 0),
                (Char('('), 3),
                (Char(')'), 4),
                (Char(';'), 5),
                (Eof, 6),
            ]
        );

        assert_eq!(
            tokenize("foo(1,2);").unwrap(),
            vec![
                (Ident("foo".to_owned()), 0),
                (Char('('), 3),
                (Num(1), 4),
                (Char(','), 5),
                (Num(2), 6),
                (Char(')'), 7),
                (Char(';'), 8),
                (Eof, 9),
            ]
        );
    }

}
