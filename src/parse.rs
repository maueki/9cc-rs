use failure::{Error, Fail};

use super::token::*;

#[derive(Fail, Debug)]
#[fail(display = "Parse Error: {}, pos: {}", _0, _1)]
pub struct ParseError(String, usize);

/// parser syntax
///
/// program: stmt program
/// program: ε
///
/// stmt: "{" block_items "}"
/// stmt: assign ";"
/// stmt: "return" assign ";"
/// stmt: ifclause
///
/// block_items: stmt
/// block_items: stmt block_items
///
/// ifclause: "if" "(" assign ")" stmt ["else" stmt]
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
/// term: ident "(" [arguments] ")"
/// term: num
/// term: ident
/// term: "(" assign ")"
///
/// arguments: assign
/// arguments: assign, arguments

#[derive(PartialEq, Eq, Debug)]
pub enum Node {
    Num(i64),
    Op(OpType, Box<Node>, Box<Node>),
    Assign(Box<Node>, Box<Node>),
    Ident(String),
    Return(Box<Node>),
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
    Block(Vec<Node>),
    Call(String, Vec<Node>),
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

fn new_node_if(cond: Node, t: Node, e: Option<Node>) -> Node {
    Node::If(Box::new(cond), Box::new(t), e.map(Box::new))
}

fn expect(tokens: &mut Tokens, token: Token) -> Result<(), Error> {
    match tokens.pop_front() {
        Some((tk, _)) if tk == token => Ok(()),
        Some((_, pos)) => Err(ParseError("Invalid Tokesn".to_owned(), pos).into()),
        None => Err(ParseError("Invalid Eof".to_owned(), 0).into()),
    }
}

fn consume(tokens: &mut Tokens, token: Token) -> Result<(), Error> {
    expect(tokens, token)
}

pub fn program(tokens: &mut Tokens) -> Result<Vec<Node>, Error> {
    let mut nodes = Vec::new();

    while tokens[0].0 != Token::Eof {
        nodes.push(stmt(tokens)?);
    }

    Ok(nodes)
}

fn stmt(tokens: &mut Tokens) -> Result<Node, Error> {
    let node = match tokens.get(0) {
        Some((Token::Return, _)) => {
            tokens.pop_front();
            new_node_return(assign(tokens)?)
        }
        Some((Token::If, _)) => {
            tokens.pop_front();
            expect(tokens, Token::ParenL)?;
            let node_cond = assign(tokens)?;
            expect(tokens, Token::ParenR)?;

            let node_then = stmt(tokens)?;
            let node_else = match consume(tokens, Token::Else) {
                Ok(()) => Some(stmt(tokens)?),
                _ => None,
            };

            return Ok(new_node_if(node_cond, node_then, node_else));
        }
        Some((Token::BraceL, _)) => {
            tokens.pop_front();
            let mut stmts = Vec::new();
            while let Ok(node) = stmt(tokens) {
                stmts.push(node);
            }
            expect(tokens, Token::BraceR)?;
            return Ok(Node::Block(stmts));
        }
        _ => assign(tokens)?,
    };

    if let Some((Token::Semicolon, _)) = tokens.pop_front() {
        Ok(node)
    } else {
        Err(ParseError("';'ではないトークンです".to_owned(), 0).into())
    }
}

fn assign(tokens: &mut Tokens) -> Result<Node, Error> {
    let mut node = equality(tokens)?;

    while let Some((Token::Assign, _)) = tokens.get(0) {
        tokens.pop_front();
        node = new_node_assign(node, assign(tokens)?);
    }

    Ok(node)
}

fn equality(tokens: &mut Tokens) -> Result<Node, Error> {
    let mut node = relational(tokens)?;

    loop {
        match tokens.get(0) {
            Some((Token::Op(OpType::Eq), _)) => {
                tokens.pop_front();
                node = new_node_op(OpType::Eq, node, relational(tokens)?);
            }
            Some((Token::Op(OpType::Ne), _)) => {
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
        match tokens.get(0) {
            Some((Token::Op(OpType::Le), _)) => {
                tokens.pop_front();
                node = new_node_op(OpType::Le, node, add(tokens)?);
            }
            Some((Token::Op(OpType::Ge), _)) => {
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
        match tokens.get(0) {
            Some((Token::Op(OpType::Add), _)) => {
                tokens.pop_front();
                node = new_node_op(OpType::Add, node, mul(tokens)?);
            }
            Some((Token::Op(OpType::Sub), _)) => {
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
        match tokens.get(0) {
            Some((Token::Op(OpType::Mul), _)) => {
                tokens.pop_front();
                node = new_node_op(OpType::Mul, node, term(tokens)?);
            }
            Some((Token::Op(OpType::Div), _)) => {
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
            match tokens.get(0) {
                Some((Token::ParenR, _pos)) => {
                    tokens.pop_front();
                }
                Some((_, pos)) => {
                    return Err(ParseError(
                        "開き括弧に対応する閉じ括弧がありません".to_owned(),
                        *pos,
                    )
                    .into());
                }
                None => {
                    return Err(ParseError("想定されないEOFです".to_owned(), 0).into());
                }
            }
            return Ok(node);
        }
        (Token::Num(n), _) => {
            return Ok(new_node_num(n));
        }
        (Token::Ident(ref id), _) => {
            if let Some((Token::ParenL, _)) = tokens.get(0) {
                tokens.pop_front();
                let args = arguments(tokens)?;
                expect(tokens, Token::ParenR)?;
                return Ok(Node::Call(id.clone(), args));
            }
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

fn arguments(tokens: &mut Tokens) -> Result<Vec<Node>, Error> {
    let mut nodes = Vec::new();

    match assign(tokens) {
        Ok(n) => nodes.push(n),
        _ => return Ok(nodes),
    }

    while let Some((Token::Comma, _)) = tokens.get(0) {
        tokens.pop_front();
        nodes.push(assign(tokens)?);
    }

    Ok(nodes)
}

#[cfg(test)]
mod test {
    use super::OpType::*;
    use super::*;

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

        {
            let mut tokens = tokenize("a=1;b=2;if (a== b) return a+b;else return 0;").unwrap();
            let p = program(&mut tokens);
            assert_eq!(p.is_ok(), true);
            assert_eq!(
                p.unwrap(),
                vec![
                    Assign(Box::new(Ident("a".to_owned())), Box::new(Num(1))),
                    Assign(Box::new(Ident("b".to_owned())), Box::new(Num(2))),
                    If(
                        Box::new(Op(
                            Eq,
                            Box::new(Ident("a".to_owned())),
                            Box::new(Ident("b".to_owned()))
                        )),
                        Box::new(Return(Box::new(Op(
                            Add,
                            Box::new(Ident("a".to_owned())),
                            Box::new(Ident("b".to_owned()))
                        )))),
                        Some(Box::new(Return(Box::new(Num(0)))))
                    ),
                ]
            );
        }

        {
            let mut tokens = tokenize("foo(1,2,a);").unwrap();
            let p = program(&mut tokens);
            assert_eq!(p.is_ok(), true);
            assert_eq!(
                p.unwrap(),
                vec![Call(
                    "foo".to_owned(),
                    vec![Num(1), Num(2), Ident("a".to_owned())]
                )]
            );
        }
    }
}
