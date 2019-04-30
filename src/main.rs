use std::collections::VecDeque;
use std::env;

use failure::{Error, Fail};

#[derive(Fail, Debug)]
#[fail(display = "Parse Error: {}, pos: {}", _0, _1)]
struct ParseError(String, usize);

#[derive(Fail, Debug)]
#[fail(display = "Tokenize Error: {}, pos: {}", _0, _1)]
struct TokenizeError(String, usize);

#[derive(PartialEq, Eq, Debug)]
enum Token {
    Num(i64),
    Op(OpType),
    ParenL,
    ParenR,
    Ident(char),
    Assign,
    Semicolon,
    Eof,
}

#[derive(PartialEq, Eq, Debug)]
enum OpType {
    Add,
    Sub,
    Mul,
    Div,
}

fn char2op(c: char) -> Option<OpType> {
    match c {
        '+' => Some(OpType::Add),
        '-' => Some(OpType::Sub),
        '*' => Some(OpType::Mul),
        '/' => Some(OpType::Div),
        _ => None,
    }
}

type Tokens = VecDeque<(Token, usize)>;

fn tokenize(text: &str) -> Result<Tokens, Error> {
    let chars = text.clone().chars().collect::<Vec<_>>();
    let mut pos = 0;
    let mut tokens = VecDeque::new();

    while pos < chars.len() {
        if chars[pos].is_whitespace() {
            pos += 1;
            continue;
        }

        if let Some(op) = char2op(chars[pos]) {
            tokens.push_back((Token::Op(op), pos));
            pos += 1;
            continue;
        }

        if chars[pos] == '(' {
            tokens.push_back((Token::ParenL, pos));
            pos += 1;
            continue;
        }

        if chars[pos] == ')' {
            tokens.push_back((Token::ParenR, pos));
            pos += 1;
            continue;
        }

        if 'a' <= chars[pos] && chars[pos] <= 'z' {
            tokens.push_back((Token::Ident(chars[pos]), pos));
            pos += 1;
            continue;
        }

        if chars[pos] == '-' {
            tokens.push_back((Token::Assign, pos));
            pos += 1;
            continue;
        }

        if chars[pos] == ';' {
            tokens.push_back((Token::Semicolon, pos));
            pos += 1;
            continue;
        }

        if chars[pos] == '=' {
            tokens.push_back((Token::Assign, pos));
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

#[derive(PartialEq, Eq, Debug)]
enum Node {
    Num(i64),
    Op(OpType, Box<Node>, Box<Node>),
    Assign(Box<Node>, Box<Node>),
    Ident(char),
}

fn new_node_num(v: i64) -> Node {
    Node::Num(v)
}

fn new_node_op(ty: OpType, lhs: Node, rhs: Node) -> Node {
    Node::Op(ty, Box::new(lhs), Box::new(rhs))
}

fn new_node_assign(lhs: Node, rhs: Node) -> Node {
    Node::Assign(Box::new(lhs), Box::new(rhs))
}

fn program(tokens: &mut Tokens) -> Result<Vec<Node>, Error> {
    let mut nodes = Vec::new();

    while tokens[0].0 != Token::Eof {
        nodes.push(stmt(tokens)?);
    }

    Ok(nodes)
}

fn stmt(tokens: &mut Tokens) -> Result<Node, Error> {
    let node = assign(tokens)?;

    if let (Token::Semicolon, _) = tokens[0] {
        tokens.pop_front();
        return Ok(node);
    }

    Err(ParseError("';'ではないトークンです".to_owned(), tokens[0].1).into())
}

fn assign(tokens: &mut Tokens) -> Result<Node, Error> {
    let mut node = add(tokens)?;

    loop {
        match tokens[0] {
            (Token::Assign, _) => {
                tokens.pop_front();
                node = new_node_assign(node, assign(tokens)?);
            }
            _ => break,
        }
    }

    Ok(node)
}

fn add(tokens: &mut Tokens) -> Result<Node, Error> {
    let mut node = mul(tokens)?;

    loop {
        match tokens[0] {
            (Token::Op(OpType::Add), _) => {
                tokens.pop_front();
                node = new_node_op(OpType::Add, node, mul(tokens)?);
            }
            (Token::Op(OpType::Sub), _) => {
                tokens.pop_front();
                node = new_node_op(OpType::Sub, node, mul(tokens)?);
            }
            _ => break,
        }
    }

    Ok(node)
}

fn mul(tokens: &mut Tokens) -> Result<Node, Error> {
    let mut node = term(tokens)?;

    loop {
        match tokens[0] {
            (Token::Op(OpType::Mul), _) => {
                tokens.pop_front();
                node = new_node_op(OpType::Mul, node, term(tokens)?);
            }
            (Token::Op(OpType::Div), _) => {
                tokens.pop_front();
                node = new_node_op(OpType::Div, node, term(tokens)?);
            }
            _ => break,
        }
    }
    Ok(node)
}

fn term(tokens: &mut Tokens) -> Result<Node, Error> {
    match tokens[0] {
        (Token::ParenL, _) => {
            tokens.pop_front();
            let node = add(tokens)?;
            match tokens[0] {
                (Token::ParenR, _pos) => {
                    tokens.pop_front();
                }
                (_, pos) => {
                    return Err(ParseError(
                        "開き括弧に対応する閉じ括弧がありません".to_owned(),
                        pos,
                    )
                    .into());
                }
            }
            return Ok(node);
        }
        (Token::Num(n), _) => {
            tokens.pop_front();
            return Ok(new_node_num(n));
        }
        (Token::Ident(c), _) => {
            tokens.pop_front();
            return Ok(Node::Ident(c));
        }
        _ => {}
    }

    Err(ParseError(
        "数値でも閉じ括弧でもないトークンです".to_owned(),
        tokens[0].1,
    )
    .into())
}

fn gen_lval(node: &Node) -> Result<(), Error> {
    match node {
        Node::Ident(c) => {
            let offset = (('z' as u32) - (*c as u32) + 1) * 8;
            println!("  mov rax, rbp");
            println!("  sub rax, {}", offset);
            println!("  push rax");
            Ok(())
        }
        _ => Err(ParseError(
            "代入の左辺値が変数ではありません".to_owned(),
            0,
        )
        .into()),
    }
}

fn gen(node: &Node) -> Result<(), Error> {
    use OpType::*;

    match node {
        Node::Num(v) => {
            println!("  push {}", v);
            Ok(())
        }
        Node::Ident(..) => {
            gen_lval(node)?;
            println!("  pop rax");
            println!("  mov rax, [rax]");
            println!("  push rax");
            Ok(())
        }
        Node::Assign(lhs, rhs) => {
            gen_lval(&lhs)?;
            gen(rhs)?;
            println!("  pop rdi");
            println!("  pop rax");
            println!("  mov [rax], rdi");
            println!("  push rdi");
            Ok(())
        }
        Node::Op(op, lhs, rhs) => {
            gen(lhs)?;
            gen(rhs)?;

            println!("  pop rdi");
            println!("  pop rax");

            match op {
                Add => println!("  add rax, rdi"),
                Sub => println!("  sub rax, rdi"),
                Mul => println!("  mul rdi"),
                Div => {
                    println!("  mov rdx, 0");
                    println!("  div rdi");
                }
            }

            println!("  push rax");
            Ok(())
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("引数の個数が正しくありません");
        ::std::process::exit(1);
    }

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    let input = &args[1];
    let mut tokens = tokenize(&input).unwrap();
    let nodes = program(&mut tokens).unwrap();

    println!("  push rbp");
    println!("  mov rbp, rsp");
    println!("  sub rsp, 208");

    for n in nodes {
        gen(&n).unwrap();
        println!("  pop rax");
    }

    println!("  mov rsp, rbp");
    println!("  pop rbp");
    println!("  ret");
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
            vec![(Num(1), 0), (Op(Add), 1), (Num(1), 2), (Eof, 3)]
        );

        assert_eq!(
            tokenize("(3+5)/2").unwrap(),
            vec![
                (ParenL, 0),
                (Num(3), 1),
                (Op(Add), 2),
                (Num(5), 3),
                (ParenR, 4),
                (Op(Div), 5),
                (Num(2), 6),
                (Eof, 7)
            ]
        );
    }

    #[test]
    fn add_test() {
        use super::Node::*;
        {
            let mut tokens = super::tokenize("1+1").unwrap();
            assert_eq!(
                add(&mut tokens).unwrap(),
                Op(Add, Box::new(Num(1)), Box::new(Num(1)))
            );
        }

        {
            let mut tokens = super::tokenize("(3+5)/2").unwrap();
            assert_eq!(
                add(&mut tokens).unwrap(),
                Op(
                    Div,
                    Box::new(Op(Add, Box::new(Num(3)), Box::new(Num(5)))),
                    Box::new(Num(2))
                )
            );
        }
    }

    #[test]
    fn program_test() {
        use super::Node::*;
        use super::*;

        {
            let mut tokens = tokenize("0;").unwrap();
            assert_eq!(program(&mut tokens).unwrap(), vec![Node::Num(0)]);
        }

        {
            let mut tokens = tokenize("a=1;b=2;a+b;").unwrap();
            let p = program(&mut tokens);
            assert_eq!(p.is_ok(), true);
            assert_eq!(
                p.unwrap(),
                vec![
                    Assign(Box::new(Ident('a')), Box::new(Num(1))),
                    Assign(Box::new(Ident('b')), Box::new(Num(2))),
                    Op(OpType::Add, Box::new(Ident('a')), Box::new(Ident('b')))
                ]
            );
        }
    }
}
