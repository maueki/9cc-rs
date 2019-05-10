use failure::{Error, Fail};

use super::token::*;

#[derive(Fail, Debug)]
#[fail(display = "Parse Error: {}, pos: {}", _0, _1)]
pub struct ParseError(String, usize);

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Le,
    Ge,
}

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
    Bin(BinOp, Box<Node>, Box<Node>),
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
    let mut context = Context::new(tokens);
    program(&mut context)
}

fn new_node_num(v: i64) -> Node {
    Node::Num(v)
}

fn new_node_bin(ty: BinOp, lhs: Node, rhs: Node) -> Node {
    Node::Bin(ty, Box::new(lhs), Box::new(rhs))
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

fn new_node_ident(s: &str) -> Node {
    Node::Ident(s.to_string())
}

fn consume(c: char, context: &mut Context) -> Result<(), Error> {
    match context.pop_token() {
        Some((Token::Sym(sym), _)) if *sym == c => Ok(()),
        Some((tk, pos)) => {
            Err(
                ParseError(format!("consume: expect {:?}, but {:?}", c, *tk), *pos).into(),
            )
        }
        None => Err(ParseError("Invalid Eof".to_owned(), 0).into()),
    }
}

fn expect(token: Token, context: &mut Context) -> Result<(), Error> {
    match context.pop_token() {
        Some((tk, _)) if *tk == token => Ok(()),
        Some((_, pos)) => Err(ParseError("expect: Invalid Token".to_owned(), *pos).into()),
        None => Err(ParseError("Invalid Eof".to_owned(), 0).into()),
    }
}

fn program(context: &mut Context) -> Result<Vec<Node>, Error> {
    let mut nodes = Vec::new();

    loop {
        if let Some((Token::Eof, _)) = context.front_token() {
            break;
        }
        nodes.push(decl_func(context)?);
    }

    Ok(nodes)
}

fn decl_func(context: &mut Context) -> Result<Node, Error> {
    ty(context)?; // TODO

    let fname = {
        let fname = match context.pop_token() {
            Some((Token::Ident(fname), _)) => fname,
            Some((_, pos)) => {
                return Err(
                    ParseError("不適切な関数名です".to_owned(), *pos).into(),
                )
            }
            _ => return Err(ParseError("想定しないEOFです".to_owned(), 0).into()),
        };
        fname.clone()
    };

    consume('(', context)?;
    let ps = params(context)?;
    consume(')', context)?;

    consume('{', context)?;
    let mut nodes = Vec::new();
    while let Ok(node) = stmt(context) {
        nodes.push(node);
    }
    consume('}', context)?;

    Ok(Node::DeclFunc(fname.clone(), ps, nodes))
}

fn params(context: &mut Context) -> Result<Vec<Param>, Error> {
    let mut ps = Vec::new();

    while let Ok(param_type) = ty(context) {
        match context.pop_token() {
            Some((Token::Ident(pname), _)) => {
                ps.push(Param {
                    ty: param_type,
                    name: pname.clone(),
                });
            }
            _ => return Err(ParseError("params: Unexpected Token".to_owned(), 0).into()),
        }

        match context.front_token() {
            Some((Token::Sym(','), _)) => {
                context.pop_token();
            }
            _ => break,
        }
    }

    Ok(ps)
}

fn stmt(context: &mut Context) -> Result<Node, Error> {
    match context.front_token() {
        Some((Token::Return, _)) => {
            context.pop_token();
            let node = new_node_return(assign(context)?);
            consume(';', context)?;
            Ok(node)
        }
        Some((Token::If, _)) => {
            context.pop_token();
            consume('(', context)?;
            let node_cond = assign(context)?;
            consume(')', context)?;

            let node_then = stmt(context)?;
            let node_else = match expect(Token::Else, context) {
                Ok(()) => Some(stmt(context)?),
                _ => None,
            };

            Ok(new_node_if(node_cond, node_then, node_else))
        }
        Some((Token::Sym('{'), _)) => {
            context.pop_token();
            let mut stmts = Vec::new();
            while let Ok(node) = stmt(context) {
                stmts.push(node);
            }
            consume('}', context)?;
            Ok(Node::Block(stmts))
        }
        _ => {
            let pos = context.pos; // FIXME:
            if let Ok(node) = decl_var(context) {
                Ok(node)
            } else {
                context.pos = pos;
                let node = assign(context)?;
                consume(';', context)?;
                Ok(node)
            }
        }
    }
}

fn decl_var(context: &mut Context) -> Result<Node, Error> {
    let t = ty(context)?;

    if let Some((Token::Ident(var), _)) = context.pop_token() {
        let var = var.clone();
        consume(';', context)?;
        Ok(Node::DeclVar(var.to_string(), t))
    } else {
        Err(
            ParseError("decl_var: Unexpected Token".to_owned(), 0).into(),
        )
    }
}

fn str2ty(s: &str) -> TyType {
    match s {
        "int" => TyType::Int,
        _ => TyType::Custom(s.to_string()),
    }
}

fn ty(context: &mut Context) -> Result<TyType, Error> {
    let mut tytype = match context.front_token() {
        Some((Token::Ident(tname), _)) => str2ty(tname),
        _ => return Err(ParseError("ty: Unexpected Token".to_owned(), 0).into()),
    };

    context.pop_token();

    while let Some((Token::Sym('*'), _)) = context.front_token() {
        context.pop_token();
        tytype = TyType::Ptr(Box::new(tytype));
    }

    Ok(tytype)
}

fn assign(context: &mut Context) -> Result<Node, Error> {
    let mut node = equality(context)?;
    while let Some((Token::Sym('='), _)) = context.front_token() {
        context.pop_token();
        node = new_node_assign(node, assign(context)?);
    }
    Ok(node)
}

fn equality(context: &mut Context) -> Result<Node, Error> {
    let mut node = relational(context)?;

    loop {
        match context.front_token() {
            Some((Token::Op(OpType::Eq), _)) => {
                context.pop_token();
                node = new_node_bin(BinOp::Eq, node, relational(context)?);
            }
            Some((Token::Op(OpType::Ne), _)) => {
                context.pop_token();
                node = new_node_bin(BinOp::Ne, node, relational(context)?);
            }
            _ => break,
        }
    }
    Ok(node)
}

fn relational(context: &mut Context) -> Result<Node, Error> {
    let mut node = add(context)?;

    loop {
        match context.front_token() {
            Some((Token::Op(OpType::Le), _)) => {
                context.pop_token();
                node = new_node_bin(BinOp::Le, node, add(context)?);
            }
            Some((Token::Op(OpType::Ge), _)) => {
                context.pop_token();
                node = new_node_bin(BinOp::Ge, node, add(context)?);
            }
            _ => break,
        }
    }
    Ok(node)
}

fn add(context: &mut Context) -> Result<Node, Error> {
    let mut node = mul(context)?;

    loop {
        match context.front_token() {
            Some((Token::Sym('+'), _)) => {
                context.pop_token();
                node = new_node_bin(BinOp::Add, node, mul(context)?);
            }
            Some((Token::Sym('-'), _)) => {
                context.pop_token();
                node = new_node_bin(BinOp::Sub, node, mul(context)?);
            }
            _ => break,
        }
    }

    Ok(node)
}

fn mul(context: &mut Context) -> Result<Node, Error> {
    let mut node = term(context)?;

    loop {
        match context.front_token() {
            Some((Token::Sym('*'), _)) => {
                context.pop_token();
                node = new_node_bin(BinOp::Mul, node, term(context)?);
            }
            Some((Token::Sym('/'), _)) => {
                context.pop_token();
                node = new_node_bin(BinOp::Div, node, term(context)?);
            }
            _ => break,
        }
    }
    Ok(node)
}

fn term(context: &mut Context) -> Result<Node, Error> {
    match context.front_token() {
        Some((Token::Sym('('), _)) => {
            context.pop_token();
            let node = add(context)?;
            match context.front_token() {
                Some((Token::Sym(')'), _pos)) => {
                    context.pop_token();
                    Ok(node)
                }
                Some((_, pos)) => Err(
                    ParseError(
                        "開き括弧に対応する閉じ括弧がありません".to_owned(),
                        *pos,
                    ).into(),
                ),
                None => Err(
                    ParseError("想定されないEOFです".to_owned(), 0).into(),
                ),
            }
        }
        Some((Token::Num(n), _)) => {
            let n = *n;
            context.pop_token();
            Ok(new_node_num(n))
        }
        Some((Token::Ident(id), _)) => {
            let id = id.clone();
            context.pop_token();
            if let Some((Token::Sym('('), _)) = context.front_token() {
                context.pop_token();
                let args = arguments(context)?;
                consume(')', context)?;
                return Ok(Node::Call(id, args));
            }
            Ok(new_node_ident(&id))
        }
        Some((_, pos)) => Err(
            ParseError(
                "数値でも閉じ括弧でもないトークンです".to_owned(),
                *pos,
            ).into(),
        ),
        _ => Err(
            ParseError("想定されないEOFです".to_owned(), 0).into(),
        ),
    }
}

fn arguments(context: &mut Context) -> Result<Vec<Node>, Error> {
    let mut nodes = Vec::new();

    match assign(context) {
        Ok(n) => nodes.push(n),
        _ => return Ok(nodes),
    }

    while let Some((Token::Sym(','), _)) = context.front_token() {
        context.pop_token();
        nodes.push(assign(context)?);
    }

    Ok(nodes)
}

struct Context<'a> {
    tokens: &'a Tokens,
    pos: usize,
}

impl<'a> Context<'a> {
    fn new(tokens: &'a Tokens) -> Self {
        Context { tokens, pos: 0 }
    }

    fn pop_token(&mut self) -> Option<&(Token, usize)> {
        let token = self.tokens.get(self.pos);
        self.pos += 1;
        token
    }

    fn front_token(&self) -> Option<&(Token, usize)> {
        self.tokens.get(self.pos)
    }
}

#[cfg(test)]
mod test {
    use super::BinOp::*;
    use super::*;

    #[test]
    fn add_test() {
        use super::Node::*;
        {
            let tokens = super::tokenize("1+1").unwrap();
            let mut context = Context::new(&tokens);
            assert_eq!(
                add(&mut context).unwrap(),
                new_node_bin(Add, Num(1), Num(1))
            );
        }

        {
            let tokens = super::tokenize("(3+5)/2").unwrap();
            let mut context = Context::new(&tokens);
            assert_eq!(
                add(&mut context).unwrap(),
                new_node_bin(Div, new_node_bin(Add, Num(3), Num(5)), Num(2))
            );
        }
    }

    fn stmts(tokens: &Tokens) -> Result<Vec<Node>, Error> {
        let mut nodes = Vec::new();
        let mut context = Context::new(tokens);

        loop {
            match stmt(&mut context) {
                Ok(node) => {
                    nodes.push(node);
                }
                Err(err) => {
                    eprintln!("{:?}", err);
                    break;
                }
            }
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
                    new_node_assign(new_node_ident("a"), Num(1)),
                    new_node_assign(new_node_ident("b"), Num(2)),
                    new_node_bin(Add, new_node_ident("a"), new_node_ident("b")),
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
                    new_node_assign(new_node_ident("a"), Num(1)),
                    new_node_assign(new_node_ident("b"), Num(2)),
                    new_node_return(new_node_bin(Add, new_node_ident("a"), new_node_ident("b"))),
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
                    new_node_assign(new_node_ident("foo"), Num(1)),
                    new_node_assign(new_node_ident("bar"), new_node_bin(Add, Num(2), Num(3))),
                    new_node_return(new_node_bin(
                        Add,
                        new_node_ident("foo"),
                        new_node_ident("bar"),
                    )),
                ]
            );
        }

        {
            let tokens = tokenize("a=1;b=2;if (a== b) return a+b;else return 0;").unwrap();
            let p = stmts(&tokens).unwrap();
            assert_eq!(
                p,
                vec![
                    new_node_assign(new_node_ident("a"), Num(1)),
                    new_node_assign(new_node_ident("b"), Num(2)),
                    new_node_if(
                        new_node_bin(Eq, new_node_ident("a"), new_node_ident("b")),
                        new_node_return(new_node_bin(Add, new_node_ident("a"), new_node_ident("b"))),
                        Some(new_node_return(Num(0)))
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
                vec![
                    Call("foo".to_owned(), vec![Num(1), Num(2), new_node_ident("a")]),
                ]
            );
        }
    }

    #[test]
    fn program_tset() {
        use super::Node::*;
        {
            let tokens = tokenize("int main() { return 0; }").unwrap();
            let p = parse(&tokens);
            assert_eq!(
                p.unwrap(),
                vec![
                    DeclFunc("main".to_owned(), Vec::new(), vec![new_node_return(Num(0))]),
                ]
            );
        }

        {
            let tokens = tokenize("int hoge() { int* a; return 0; }").unwrap();
            let p = parse(&tokens).unwrap();
            assert_eq!(
                p,
                vec![
                    DeclFunc(
                        "hoge".to_owned(),
                        Vec::new(),
                        vec![
                            DeclVar("a".to_owned(), TyType::Ptr(Box::new(TyType::Int))),
                            Return(Box::new(Num(0))),
                        ]
                    ),
                ]
            );
        }
    }
}
