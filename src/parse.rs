use failure::{Error, Fail};

use super::token::*;

#[derive(Fail, Debug)]
#[fail(display = "Parse Error: {}, pos: {}", _0, _1)]
pub struct ParseError(String, usize);

/// parser syntax
///
/// program: decl_func program
/// program: ε
///
/// decl_func: type ident "(" params ")" "{" stmt "}"
///
/// params: type ident
/// params: type ident, params
///
/// type: ident
/// type: type "*"
///
/// stmt: "{" block_items "}"
/// stmt: assign ";"
/// stmt: "return" assign ";"
/// stmt: ifclause
/// stmt: decl_var
///
/// block_items: stmt
/// block_items: stmt block_items
///
/// ifclause: "if" "(" assign ")" stmt ["else" stmt]
///
/// decl_var: type ident ";"
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

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum TyType {
    Int,
    Custom(String),
    Ptr(Box<TyType>),
}

#[derive(PartialEq, Eq, Debug)]
pub struct Param {
    pub ty: TyType,
    pub name: String,
}

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
    DeclFunc(String, Vec<Param>, Vec<Node>),
    DeclVar(String, TyType),
}

pub fn parse(tokens: &Tokens) -> Result<Vec<Node>, Error> {
    let mut context = Context::new();
    program(tokens, &mut context)
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

fn consume(c: char, tokens: &Tokens, context: &mut Context) -> Result<(), Error> {
    match tokens.get(context.pos) {
        Some((Token::SLSym(sym), _)) if *sym == c => {
            context.pos += 1;
            Ok(())
        }
        Some((_, pos)) => Err(ParseError("Invalid Token".to_owned(), *pos).into()),
        None => Err(ParseError("Invalid Eof".to_owned(), 0).into()),
    }
}

fn expect(token: Token, tokens: &Tokens, context: &mut Context) -> Result<(), Error> {
    match tokens.get(context.pos) {
        Some((tk, _)) if *tk == token => {
            context.pos += 1;
            Ok(())
        }
        Some((_, pos)) => Err(ParseError("Invalid Token".to_owned(), *pos).into()),
        None => Err(ParseError("Invalid Eof".to_owned(), 0).into()),
    }
}

fn program(tokens: &Tokens, context: &mut Context) -> Result<Vec<Node>, Error> {
    let mut nodes = Vec::new();

    while tokens[context.pos].0 != Token::Eof {
        nodes.push(decl_func(tokens, context)?);
    }

    Ok(nodes)
}

fn decl_func(tokens: &Tokens, context: &mut Context) -> Result<Node, Error> {
    ty(tokens, context)?; // TODO

    let fname = match tokens.get(context.pos) {
        Some((Token::Ident(fname), _)) => {
            context.pos += 1;
            fname
        }
        Some((_, pos)) => {
            return Err(ParseError("不適切な関数名です".to_owned(), *pos).into())
        }
        _ => return Err(ParseError("想定しないEOFです".to_owned(), 0).into()),
    };

    consume('(', tokens, context)?;
    let ps = params(tokens, context)?;
    consume(')', tokens, context)?;

    consume('{', tokens, context)?;
    let mut nodes = Vec::new();
    while let Ok(node) = stmt(tokens, context) {
        nodes.push(node);
    }
    consume('}', tokens, context)?;

    Ok(Node::DeclFunc(fname.clone(), ps, nodes))
}

fn params(tokens: &Tokens, context: &mut Context) -> Result<Vec<Param>, Error> {
    let mut ps = Vec::new();

    while let Ok(param_type) = ty(tokens, context) {
        match tokens.get(context.pos) {
            Some((Token::Ident(pname), _)) => {
                ps.push(Param {
                    ty: param_type,
                    name: pname.clone(),
                });
                context.pos += 1;
            }
            _ => return Err(ParseError("params: Unexpected Token".to_owned(), 0).into()),
        }

        match tokens.get(context.pos) {
            Some((Token::SLSym(','), _)) => {
                context.pos += 1;
            }
            _ => break,
        }
    }

    Ok(ps)
}

fn stmt(tokens: &Tokens, context: &mut Context) -> Result<Node, Error> {
    match tokens.get(context.pos) {
        Some((Token::Return, _)) => {
            context.pos += 1;
            let node = new_node_return(assign(tokens, context)?);
            consume(';', tokens, context)?;
            Ok(node)
        }
        Some((Token::If, _)) => {
            context.pos += 1;
            consume('(', tokens, context)?;
            let node_cond = assign(tokens, context)?;
            consume(')', tokens, context)?;

            let node_then = stmt(tokens, context)?;
            let node_else = match expect(Token::Else, tokens, context) {
                Ok(()) => Some(stmt(tokens, context)?),
                _ => None,
            };

            Ok(new_node_if(node_cond, node_then, node_else))
        }
        Some((Token::SLSym('{'), _)) => {
            context.pos += 1;
            let mut stmts = Vec::new();
            while let Ok(node) = stmt(tokens, context) {
                stmts.push(node);
            }
            consume('}', tokens, context)?;
            Ok(Node::Block(stmts))
        }
        _ => {
            let pos = context.pos;
            if let Ok(node) = decl_var(tokens, context) {
                Ok(node)
            } else {
                context.pos = pos;
                let node = assign(tokens, context)?;
                consume(';', tokens, context)?;
                Ok(node)
            }
        }
    }
}

fn decl_var(tokens: &Tokens, context: &mut Context) -> Result<Node, Error> {
    let t = ty(tokens, context)?;

    if let Some((Token::Ident(var), _)) = tokens.get(context.pos) {
        context.pos += 1;
        consume(';', tokens, context)?;
        Ok(Node::DeclVar(var.to_string(), t))
    } else {
        Err(ParseError("decl_var: Unexpected Token".to_owned(), 0).into())
    }
}

fn str2ty(s: &str) -> TyType {
    match s {
        "int" => TyType::Int,
        _ => TyType::Custom(s.to_string()),
    }
}

fn ty(tokens: &Tokens, context: &mut Context) -> Result<TyType, Error> {
    let mut tytype = match tokens.get(context.pos) {
        Some((Token::Ident(tname), _)) => {
            context.pos += 1;
            str2ty(tname)
        }
        _ => return Err(ParseError("ty: Unexpected Token".to_owned(), 0).into()),
    };

    while let Some((Token::SLSym('*'), _)) = tokens.get(context.pos) {
        context.pos += 1;
        tytype = TyType::Ptr(Box::new(tytype));
    }

    Ok(tytype)
}

fn assign(tokens: &Tokens, context: &mut Context) -> Result<Node, Error> {
    let mut node = equality(tokens, context)?;

    while let Some((Token::SLSym('='), _)) = tokens.get(context.pos) {
        context.pos += 1;
        node = new_node_assign(node, assign(tokens, context)?);
    }

    Ok(node)
}

fn equality(tokens: &Tokens, context: &mut Context) -> Result<Node, Error> {
    let mut node = relational(tokens, context)?;

    loop {
        match tokens.get(context.pos) {
            Some((Token::Op(OpType::Eq), _)) => {
                context.pos += 1;
                node = new_node_op(OpType::Eq, node, relational(tokens, context)?);
            }
            Some((Token::Op(OpType::Ne), _)) => {
                context.pos += 1;
                node = new_node_op(OpType::Ne, node, relational(tokens, context)?);
            }
            _ => break,
        }
    }
    Ok(node)
}

fn relational(tokens: &Tokens, context: &mut Context) -> Result<Node, Error> {
    let mut node = add(tokens, context)?;

    loop {
        match tokens.get(context.pos) {
            Some((Token::Op(OpType::Le), _)) => {
                context.pos += 1;
                node = new_node_op(OpType::Le, node, add(tokens, context)?);
            }
            Some((Token::Op(OpType::Ge), _)) => {
                context.pos += 1;
                node = new_node_op(OpType::Ge, node, add(tokens, context)?);
            }
            _ => break,
        }
    }
    Ok(node)
}

fn add(tokens: &Tokens, context: &mut Context) -> Result<Node, Error> {
    let mut node = mul(tokens, context)?;

    loop {
        match tokens.get(context.pos) {
            Some((Token::SLSym('+'), _)) => {
                context.pos += 1;
                node = new_node_op(OpType::Add, node, mul(tokens, context)?);
            }
            Some((Token::SLSym('-'), _)) => {
                context.pos += 1;
                node = new_node_op(OpType::Sub, node, mul(tokens, context)?);
            }
            _ => break,
        }
    }

    Ok(node)
}

fn mul(tokens: &Tokens, context: &mut Context) -> Result<Node, Error> {
    let mut node = term(tokens, context)?;

    loop {
        match tokens.get(context.pos) {
            Some((Token::SLSym('*'), _)) => {
                context.pos += 1;
                node = new_node_op(OpType::Mul, node, term(tokens, context)?);
            }
            Some((Token::SLSym('/'), _)) => {
                context.pos += 1;
                node = new_node_op(OpType::Div, node, term(tokens, context)?);
            }
            _ => break,
        }
    }
    Ok(node)
}

fn term(tokens: &Tokens, context: &mut Context) -> Result<Node, Error> {
    match tokens.get(context.pos) {
        Some((Token::SLSym('('), _)) => {
            context.pos += 1;
            let node = add(tokens, context)?;
            match tokens.get(context.pos) {
                Some((Token::SLSym(')'), _pos)) => {
                    context.pos += 1;
                    Ok(node)
                }
                Some((_, pos)) => Err(ParseError(
                    "開き括弧に対応する閉じ括弧がありません".to_owned(),
                    *pos,
                )
                .into()),
                None => Err(ParseError("想定されないEOFです".to_owned(), 0).into()),
            }
        }
        Some((Token::Num(n), _)) => {
            context.pos += 1;
            Ok(new_node_num(*n))
        }
        Some((Token::Ident(ref id), _)) => {
            context.pos += 1;
            if let Some((Token::SLSym('('), _)) = tokens.get(context.pos) {
                context.pos += 1;
                let args = arguments(tokens, context)?;
                consume(')', tokens, context)?;
                return Ok(Node::Call(id.clone(), args));
            }
            Ok(Node::Ident(id.clone()))
        }
        Some(..) => Err(ParseError(
            "数値でも閉じ括弧でもないトークンです".to_owned(),
            tokens[0].1,
        )
        .into()),
        _ => Err(ParseError("想定されないEOFです".to_owned(), 0).into()),
    }
}

fn arguments(tokens: &Tokens, context: &mut Context) -> Result<Vec<Node>, Error> {
    let mut nodes = Vec::new();

    match assign(tokens, context) {
        Ok(n) => nodes.push(n),
        _ => return Ok(nodes),
    }

    while let Some((Token::SLSym(','), _)) = tokens.get(context.pos) {
        context.pos += 1;
        nodes.push(assign(tokens, context)?);
    }

    Ok(nodes)
}

struct Context {
    pos: usize,
}

impl Context {
    fn new() -> Self {
        Context { pos: 0 }
    }
}

#[cfg(test)]
mod test {
    use super::OpType::*;
    use super::*;

    #[test]
    fn add_test() {
        use super::Node::*;
        {
            let mut context = Context::new();
            let tokens = super::tokenize("1+1").unwrap();
            assert_eq!(
                add(&tokens, &mut context).unwrap(),
                Op(Add, Box::new(Num(1)), Box::new(Num(1)))
            );
        }

        {
            let mut context = Context::new();
            let tokens = super::tokenize("(3+5)/2").unwrap();
            assert_eq!(
                add(&tokens, &mut context).unwrap(),
                Op(
                    Div,
                    Box::new(Op(Add, Box::new(Num(3)), Box::new(Num(5)))),
                    Box::new(Num(2))
                )
            );
        }
    }

    fn stmts(tokens: &Tokens) -> Result<Vec<Node>, Error> {
        let mut nodes = Vec::new();
        let mut context = Context::new();

        while let Ok(node) = stmt(tokens, &mut context) {
            nodes.push(node);
        }

        Ok(nodes)
    }

    #[test]
    fn stmt_test() {
        use super::Node::*;
        use super::*;

        {
            let tokens = tokenize("0;").unwrap();
            assert_eq!(stmts(&tokens).unwrap(), vec![Node::Num(0)]);
        }

        {
            let tokens = tokenize("a=1;b=2;a+b;").unwrap();
            let p = stmts(&tokens);
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
            let tokens = tokenize("a=1;b=2;return a+b;").unwrap();
            let p = stmts(&tokens);
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
            let tokens = tokenize("foo = 1;\nbar = 2 + 3;\nreturn foo + bar;").unwrap();
            let p = stmts(&tokens);
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
            let tokens = tokenize("a=1;b=2;if (a== b) return a+b;else return 0;").unwrap();
            let p = stmts(&tokens).unwrap();
            assert_eq!(
                p,
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
            let tokens = tokenize("foo(1,2,a);").unwrap();
            let p = stmts(&tokens);
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

    #[test]
    fn program_tset() {
        use super::Node::*;
        {
            let tokens = tokenize("int main() { return 0; }").unwrap();
            let p = parse(&tokens).unwrap();
            assert_eq!(
                p,
                vec![DeclFunc(
                    "main".to_owned(),
                    Vec::new(),
                    vec![Return(Box::new(Num(0)))]
                )]
            );
        }

        {
            let tokens = tokenize("int hoge() { int* a; return 0; }").unwrap();
            let p = parse(&tokens).unwrap();
            assert_eq!(
                p,
                vec![DeclFunc(
                    "hoge".to_owned(),
                    Vec::new(),
                    vec![
                        DeclVar("a".to_owned(), TyType::Ptr(Box::new(TyType::Int))),
                        Return(Box::new(Num(0)))
                    ]
                )]
            );
        }
    }
}
