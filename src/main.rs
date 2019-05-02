use lazy_static::lazy_static;
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
    Ident(String),
    Assign,
    Semicolon,
    Return,
    Eof,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum OpType {
    Add,
    Sub,
    Mul,
    Div,
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

        v.push(("+", Add));
        v.push(("-", Sub));
        v.push(("*", Mul));
        v.push(("/", Div));
        v
    };
}

type Tokens = VecDeque<(Token, usize)>;

fn is_alnum(c: char) -> bool {
    ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('0' <= c && c <= '9') || (c == '_')
}

fn tokenize(text: &str) -> Result<Tokens, Error> {
    let chars = text.chars().collect::<Vec<_>>();
    let mut pos = 0;
    let mut tokens = VecDeque::new();

    'outer: while pos < chars.len() {
        if chars[pos].is_whitespace() {
            pos += 1;
            continue;
        }

        if chars[pos..]
            .iter()
            .collect::<String>()
            .starts_with("return")
        {
            match chars.get(pos + 6) {
                Some(c) if !is_alnum(*c) => {
                    tokens.push_back((Token::Return, pos));
                    pos += 6;
                    continue;
                }
                _ => {}
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

/// parser syntax
///
/// program: stmt program
/// program: ε
///
/// stmt: assign ";"
///
/// assign: equality
/// assign: equality "=" assign
///
/// equality: relational
/// equality: equality "==" relational
/// equality: equality "!=" relational
///
/// relational: add
/// relational: relational "<" add
/// relational: relational "<=" add
/// relational: relational ">" add
/// relational: relational ">=" add
///
/// add: mul
/// add: add "+" mul
/// add: add "-" mul
///
/// mul: unary
/// mul: mul "*" unary
/// mul: mul "/" unary
///
/// unary: term
/// unary: "+" term
/// unary: "-" term
///
/// term: num
/// term: ident
/// term: "(" assign ")"

#[derive(PartialEq, Eq, Debug)]
enum Node {
    Num(i64),
    Op(OpType, Box<Node>, Box<Node>),
    Assign(Box<Node>, Box<Node>),
    Ident(String),
    Return(Box<Node>),
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

fn new_node_return(lhs: Node) -> Node {
    Node::Return(Box::new(lhs))
}

fn program(tokens: &mut Tokens) -> Result<Vec<Node>, Error> {
    let mut nodes = Vec::new();

    while tokens[0].0 != Token::Eof {
        nodes.push(stmt(tokens)?);
    }

    Ok(nodes)
}

fn stmt(tokens: &mut Tokens) -> Result<Node, Error> {
    let node = match tokens[0] {
        (Token::Return, _) => {
            tokens.pop_front();
            new_node_return(assign(tokens)?)
        }
        _ => assign(tokens)?,
    };

    if let (Token::Semicolon, _) = tokens[0] {
        tokens.pop_front();
        return Ok(node);
    }

    Err(ParseError("';'ではないトークンです".to_owned(), tokens[0].1).into())
}

fn assign(tokens: &mut Tokens) -> Result<Node, Error> {
    let mut node = equality(tokens)?;

    while let (Token::Assign, _) = tokens[0] {
        tokens.pop_front();
        node = new_node_assign(node, assign(tokens)?);
    }

    Ok(node)
}

fn equality(tokens: &mut Tokens) -> Result<Node, Error> {
    let mut node = relational(tokens)?;

    loop {
        match tokens[0] {
            (Token::Op(OpType::Eq), _) => {
                tokens.pop_front();
                node = new_node_op(OpType::Eq, node, relational(tokens)?);
            }
            (Token::Op(OpType::Ne), _) => {
                tokens.pop_front();
                node = new_node_op(OpType::Ne, node, relational(tokens)?);
            }
            _ => break,
        }
    }
    Ok(node)
}

fn relational(tokens: &mut Tokens) -> Result<Node, Error> {
    let mut node = add(tokens)?;

    loop {
        match tokens[0] {
            (Token::Op(OpType::Le), _) => {
                tokens.pop_front();
                node = new_node_op(OpType::Le, node, add(tokens)?);
            }
            (Token::Op(OpType::Ge), _) => {
                tokens.pop_front();
                node = new_node_op(OpType::Ge, node, add(tokens)?);
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
    match tokens.pop_front().unwrap() {
        (Token::ParenL, _) => {
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
            return Ok(new_node_num(n));
        }
        (Token::Ident(ref id), _) => {
            return Ok(Node::Ident(id.clone()));
        }
        front => {
            tokens.push_front(front);
        }
    }

    Err(ParseError(
        "数値でも閉じ括弧でもないトークンです".to_owned(),
        tokens[0].1,
    )
    .into())
}

fn gen_lval(node: &Node, context: &mut Context) -> Result<(), Error> {
    match node {
        Node::Ident(id) => {
            let offset = context.var_put(id.clone());
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

fn gen(node: &Node, context: &mut Context) -> Result<(), Error> {
    use OpType::*;

    match node {
        Node::Return(lhs) => {
            gen(lhs, context)?;
            println!("  pop rax");
            println!("  mov rsp, rbp");
            println!("  pop rbp");
            println!("  ret");
            Ok(())
        }
        Node::Num(v) => {
            println!("  push {}", v);
            Ok(())
        }
        Node::Ident(..) => {
            gen_lval(node, context)?;
            println!("  pop rax");
            println!("  mov rax, [rax]");
            println!("  push rax");
            Ok(())
        }
        Node::Assign(lhs, rhs) => {
            gen_lval(&lhs, context)?;
            gen(rhs, context)?;
            println!("  pop rdi");
            println!("  pop rax");
            println!("  mov [rax], rdi");
            println!("  push rdi");
            Ok(())
        }
        Node::Op(op, lhs, rhs) => {
            gen(lhs, context)?;
            gen(rhs, context)?;

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
                Eq => {
                    println!("  cmp rax, rdi");
                    println!("  sete al");
                    println!("  movzb rax, al");
                }
                Ne => {
                    println!("  cmp rax, rdi");
                    println!("  setne al");
                    println!("  movzb rax, al");
                }
                Le => {
                    println!("  cmp rax, rdi");
                    println!("  setle al");
                    println!("  movzb rax, al");
                }
                Ge => {
                    println!("  cmp rdi, rax");
                    println!("  setle al");
                    println!("  movzb rax, al");
                }
            }

            println!("  push rax");
            Ok(())
        }
    }
}

struct Context {
    var_map: VecDeque<(String, usize)>,
    cur_offset: usize,
}

impl Context {
    fn new() -> Self {
        Context {
            var_map: VecDeque::new(),
            cur_offset: 0,
        }
    }

    fn var_put(&mut self, ident: String) -> usize {
        for var in self.var_map.iter() {
            if var.0 == ident {
                return var.1;
            }
        }

        let offset = self.cur_offset;
        self.cur_offset += 8;
        self.var_map.push_front((ident, offset));
        offset
    }

    /*    fn var_get(&mut self, ident: String) -> Option<usize> {
        for var in self.var_map.iter() {
            if var.0 == ident {
                return Some(var.1);
            }
        }

        None
    }*/
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

    let mut context = Context::new();
    for n in nodes {
        gen(&n, &mut context).unwrap();
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

        assert_eq!(
            tokenize("a=1;return a;").unwrap(),
            vec![
                (Ident("a".to_owned()), 0),
                (Assign, 1),
                (Num(1), 2),
                (Semicolon, 3),
                (Return, 4),
                (Ident("a".to_owned()), 11),
                (Semicolon, 12),
                (Eof, 13)
            ]
        );

        assert_eq!(
            tokenize("5*(9-6)").unwrap(),
            vec![
                (Num(5), 0),
                (Op(Mul), 1),
                (ParenL, 2),
                (Num(9), 3),
                (Op(Sub), 4),
                (Num(6), 5),
                (ParenR, 6),
                (Eof, 7),
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
                    Assign(Box::new(Ident("a".to_owned())), Box::new(Num(1))),
                    Assign(Box::new(Ident("b".to_owned())), Box::new(Num(2))),
                    Op(
                        OpType::Add,
                        Box::new(Ident("a".to_owned())),
                        Box::new(Ident("b".to_owned()))
                    )
                ]
            );
        }

        {
            let mut tokens = tokenize("a=1;b=2;return a+b;").unwrap();
            let p = program(&mut tokens);
            assert_eq!(p.is_ok(), true);
            assert_eq!(
                p.unwrap(),
                vec![
                    Assign(Box::new(Ident("a".to_owned())), Box::new(Num(1))),
                    Assign(Box::new(Ident("b".to_owned())), Box::new(Num(2))),
                    Return(Box::new(Op(
                        OpType::Add,
                        Box::new(Ident("a".to_owned())),
                        Box::new(Ident("b".to_owned()))
                    )))
                ]
            );
        }

        {
            let mut tokens = tokenize("foo = 1;\nbar = 2 + 3;\nreturn foo + bar;").unwrap();
            let p = program(&mut tokens);
            assert_eq!(p.is_ok(), true);
            assert_eq!(
                p.unwrap(),
                vec![
                    Assign(Box::new(Ident("foo".to_owned())), Box::new(Num(1))),
                    Assign(
                        Box::new(Ident("bar".to_owned())),
                        Box::new(Op(Add, Box::new(Num(2)), Box::new(Num(3))))
                    ),
                    Return(Box::new(Op(
                        OpType::Add,
                        Box::new(Ident("foo".to_owned())),
                        Box::new(Ident("bar".to_owned()))
                    )))
                ]
            );
        }
    }
}
