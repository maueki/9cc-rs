use failure::{Error, Fail};
use std::collections::HashMap;

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
/// unary: "*" term
/// unary: "&" term
/// unary: sizeof term
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
    Ptr(Box<TyType>),
    Void,
}

#[derive(PartialEq, Eq, Debug)]
pub struct Param {
    pub ty: TyType,
    pub name: String,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Node {
    Num(TyType, i64),
    Bin(TyType, BinOp, Box<Node>, Box<Node>),
    Assign(TyType, Box<Node>, Box<Node>),
    Ident(TyType, String),
    Return(Box<Node>),
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
    Block(Vec<Node>),
    Call(TyType, String, Vec<Node>),
    DeclFunc(String, Vec<Param>, Vec<Node>),
    DeclVar(String, TyType),
    Addr(TyType, Box<Node>),
    Deref(TyType, Box<Node>),
    Sizeof(TyType),
}

pub fn parse(tokens: &Tokens) -> Result<Vec<Node>, Error> {
    let mut context = GlobalContext::new(tokens);
    program(&mut context)
}

fn new_node_num(ty: TyType, v: i64) -> Node {
    Node::Num(ty, v)
}

fn new_node_bin(ty: TyType, op: BinOp, lhs: Node, rhs: Node) -> Node {
    Node::Bin(ty, op, Box::new(lhs), Box::new(rhs))
}

fn new_node_assign(ty: TyType, lhs: Node, rhs: Node) -> Node {
    Node::Assign(ty, Box::new(lhs), Box::new(rhs))
}

fn new_node_return(lhs: Node) -> Node {
    Node::Return(Box::new(lhs))
}

fn new_node_if(cond: Node, t: Node, e: Option<Node>) -> Node {
    Node::If(Box::new(cond), Box::new(t), e.map(Box::new))
}

fn new_node_ident(ty: TyType, s: &str) -> Node {
    Node::Ident(ty, s.to_string())
}

fn consume(c: char, context: &mut Context) -> Result<(), Error> {
    match context.front_token() {
        Some((Token::Sym(sym), _)) if *sym == c => {
            context.pop_token();
            Ok(())
        }
        Some((tk, pos)) => {
            Err(ParseError(format!("consume: expect {:?}, but {:?}", c, *tk), *pos).into())
        }
        None => Err(ParseError("Invalid Eof".to_owned(), 0).into()),
    }
}

fn expect(token: Token, context: &mut Context) -> Result<(), Error> {
    match context.pop_token() {
        Some((tk, _)) if tk == token => Ok(()),
        Some((_, pos)) => Err(ParseError("expect: Invalid Token".to_owned(), pos).into()),
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
    let context = &mut BlockContext::new(context);

    ty(context)?; // TODO

    let fname = {
        let fname = match context.pop_token() {
            Some((Token::Ident(fname), _)) => fname,
            Some((_, pos)) => {
                return Err(ParseError("不適切な関数名です".to_owned(), pos).into())
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
                context.put_var(&pname.clone(), &param_type);
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
            let node = new_node_return(assign(context)?.1);
            consume(';', context)?;
            Ok(node)
        }
        Some((Token::If, _)) => {
            context.pop_token();
            consume('(', context)?;
            let node_cond = assign(context)?.1;
            consume(')', context)?;

            let node_then = stmt(context)?;
            let node_else = match expect(Token::Else, context) {
                Ok(()) => Some(stmt(context)?),
                _ => None,
            };

            Ok(new_node_if(node_cond, node_then, node_else))
        }
        Some((Token::Sym('{'), _)) => {
            let context = &mut BlockContext::new(context);
            context.pop_token();
            let mut stmts = Vec::new();
            while let Ok(node) = stmt(context) {
                stmts.push(node);
            }
            consume('}', context)?;
            Ok(Node::Block(stmts))
        }
        _ => {
            let pos = context.get_pos(); // FIXME:
            if let Ok(node) = decl_var(context) {
                Ok(node)
            } else {
                context.set_pos(pos);
                let node = assign(context)?.1;
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
        context.put_var(&var.clone(), &t.clone());
        Ok(Node::DeclVar(var.to_string(), t))
    } else {
        Err(ParseError("decl_var: Unexpected Token".to_owned(), 0).into())
    }
}

fn str2ty(s: &str) -> Result<TyType, Error> {
    match s {
        "int" => Ok(TyType::Int),
        _ => Err(ParseError(format!("Invalid Type: {}", s), 0).into()),
    }
}

fn ty(context: &mut Context) -> Result<TyType, Error> {
    let mut tytype = match context.front_token() {
        Some((Token::Ident(tname), _)) => str2ty(tname)?,
        _ => return Err(ParseError("ty: Unexpected Token".to_owned(), 0).into()),
    };

    context.pop_token();

    while let Some((Token::Sym('*'), _)) = context.front_token() {
        context.pop_token();
        tytype = TyType::Ptr(Box::new(tytype));
    }

    Ok(tytype)
}

fn assign(context: &mut Context) -> Result<(TyType, Node), Error> {
    let (lty, mut lnode) = equality(context)?;
    while let Some((Token::Sym('='), _)) = context.front_token() {
        context.pop_token();
        let (_rty, rnode) = assign(context)?;
        // TODO: 型チェック
        lnode = new_node_assign(lty.clone(), lnode, rnode);
    }
    Ok((lty, lnode))
}

fn equality(context: &mut Context) -> Result<(TyType, Node), Error> {
    let (mut lty, mut lnode) = relational(context)?;

    loop {
        match context.front_token() {
            Some((Token::Op(OpType::Eq), _)) => {
                context.pop_token();
                let (_rty, rnode) = relational(context)?;
                // TODO: 型チェック
                lty = TyType::Int;
                lnode = new_node_bin(lty.clone(), BinOp::Eq, lnode, rnode);
            }
            Some((Token::Op(OpType::Ne), _)) => {
                context.pop_token();
                let (_rty, rnode) = relational(context)?;
                // TODO: 型チェック
                lty = TyType::Int;
                lnode = new_node_bin(lty.clone(), BinOp::Ne, lnode, rnode);
            }
            _ => break,
        }
    }

    Ok((lty, lnode))
}

fn relational(context: &mut Context) -> Result<(TyType, Node), Error> {
    let (mut lty, mut lnode) = add(context)?;

    loop {
        match context.front_token() {
            Some((Token::Op(OpType::Le), _)) => {
                context.pop_token();
                let (_rty, rnode) = add(context)?;
                // TODO: 型チェック
                lty = TyType::Int;
                lnode = new_node_bin(lty.clone(), BinOp::Le, lnode, rnode);
            }
            Some((Token::Op(OpType::Ge), _)) => {
                context.pop_token();
                let (_rty, rnode) = add(context)?;
                // TODO: 型チェック
                lty = TyType::Int;
                lnode = new_node_bin(lty.clone(), BinOp::Ge, lnode, rnode);
            }
            _ => break,
        }
    }
    Ok((lty, lnode))
}

fn add(context: &mut Context) -> Result<(TyType, Node), Error> {
    let (mut lty, mut lnode) = mul(context)?;

    loop {
        match context.front_token() {
            Some((Token::Sym('+'), pos)) => {
                let pos = pos.clone();
                context.pop_token();
                let (rty, rnode) = mul(context)?;
                lty = match (lty, rty) {
                    (TyType::Int, TyType::Ptr(ty)) => TyType::Ptr(ty),
                    (TyType::Ptr(ty), TyType::Int) => TyType::Ptr(ty),
                    (TyType::Int, TyType::Int) => TyType::Int,
                    _ => {
                        return Err(ParseError("加算不可能な型です".to_owned(), pos).into())
                    }
                };
                lnode = new_node_bin(lty.clone(), BinOp::Add, lnode, rnode);
            }
            Some((Token::Sym('-'), pos)) => {
                let pos = pos.clone();
                context.pop_token();
                let (rty, rnode) = mul(context)?;
                lty = match (lty, rty) {
                    (TyType::Int, TyType::Ptr(_ty)) => {
                        return Err(ParseError("引算不可能な型です".to_owned(), pos).into())
                    }
                    (TyType::Ptr(ty), TyType::Int) => TyType::Ptr(ty),
                    (TyType::Int, TyType::Int) => TyType::Int,
                    _ => {
                        return Err(ParseError("引算不可能な型です".to_owned(), pos).into())
                    }
                };

                lnode = new_node_bin(lty.clone(), BinOp::Sub, lnode, rnode);
            }
            _ => break,
        }
    }

    Ok((lty, lnode))
}

fn mul(context: &mut Context) -> Result<(TyType, Node), Error> {
    let (lty, mut lnode) = unary(context)?;

    loop {
        match context.front_token() {
            Some((Token::Sym('*'), pos)) => {
                let pos = pos.clone();
                context.pop_token();
                let (rty, rnode) = unary(context)?;
                if lty != TyType::Int || rty != TyType::Int {
                    return Err(ParseError("乗算不可能な型です".to_owned(), pos).into());
                }
                lnode = new_node_bin(lty.clone(), BinOp::Mul, lnode, rnode);
            }
            Some((Token::Sym('/'), pos)) => {
                let pos = pos.clone();
                context.pop_token();
                let (rty, rnode) = unary(context)?;
                if lty != TyType::Int || rty != TyType::Int {
                    return Err(ParseError("除算不可能な型です".to_owned(), pos).into());
                }
                lnode = new_node_bin(lty.clone(), BinOp::Div, lnode, rnode);
            }
            _ => break,
        }
    }

    Ok((lty, lnode))
}

fn unary(context: &mut Context) -> Result<(TyType, Node), Error> {
    if let Ok(..) = consume('*', context) {
        let (ty, node) = term(context)?;
        match ty {
            TyType::Ptr(ty) => Ok((*ty.clone(), Node::Deref(*ty.clone(), Box::new(node)))),
            t => {
                Err(ParseError(
                    format!("デリファレンスできない型です: {:?}", t),
                    0, /*FIXME*/
                )
                .into())
            }
        }
    } else if let Ok(..) = consume('&', context) {
        let (ty, node) = term(context)?;
        Ok((
            TyType::Ptr(Box::new(ty.clone())),
            Node::Addr(TyType::Ptr(Box::new(ty.clone())), Box::new(node)),
        ))
    } else if let Some((Token::Sizeof, _)) = context.front_token() {
        context.pop_token();
        let (ty, _) = unary(context)?;
        Ok((TyType::Int, Node::Sizeof(ty)))
    } else {
        term(context)
    }
}

fn term(context: &mut Context) -> Result<(TyType, Node), Error> {
    match context.front_token() {
        Some((Token::Sym('('), _)) => {
            context.pop_token();
            let (ty, node) = add(context)?;
            match context.front_token() {
                Some((Token::Sym(')'), _pos)) => {
                    context.pop_token();
                    Ok((ty, node))
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
            let n = *n;
            context.pop_token();
            Ok((TyType::Int, new_node_num(TyType::Int, n)))
        }
        Some((Token::Ident(id), pos)) => {
            let pos = pos.clone();
            let id = id.clone();
            context.pop_token();
            if let Some((Token::Sym('('), _)) = context.front_token() {
                context.pop_token();
                let args = arguments(context)?;
                consume(')', context)?;
                return Ok((
                    TyType::Int, /*FIXME*/
                    Node::Call(TyType::Int /*FIXME*/, id, args),
                ));
            }

            match context.get_var(&id) {
                Some(ty) => Ok((ty.clone(), new_node_ident(ty.clone(), &id))),
                _ => Err(
                    ParseError(format!("宣言されていない変数です: {}", id), pos).into(),
                ),
            }
        }
        Some((t, pos)) => Err(ParseError(
            format!(
                "数値でも閉じ括弧でもないトークンです: {:?}",
                t
            ),
            *pos,
        )
        .into()),
        _ => Err(ParseError("想定されないEOFです".to_owned(), 0).into()),
    }
}

fn arguments(context: &mut Context) -> Result<Vec<Node>, Error> {
    let mut nodes = Vec::new();

    match assign(context) {
        Ok((_ty, n)) => nodes.push(n),
        _ => return Ok(nodes),
    }

    while let Some((Token::Sym(','), _)) = context.front_token() {
        context.pop_token();
        nodes.push(assign(context)?.1);
    }

    Ok(nodes)
}

trait Context {
    fn pop_token(&mut self) -> Option<(Token, usize)>;
    fn front_token(&self) -> Option<&(Token, usize)>;
    fn set_pos(&mut self, pos: usize);
    fn get_pos(&self) -> usize;
    fn get_var(&self, id: &str) -> Option<&TyType>;
    fn put_var(&mut self, id: &str, ty: &TyType);
}

struct GlobalContext<'a> {
    tokens: &'a Tokens,
    pos: usize,
    vars: HashMap<String, TyType>,
}

impl<'a> GlobalContext<'a> {
    fn new(tokens: &'a Tokens) -> Self {
        Self {
            tokens,
            pos: 0,
            vars: HashMap::new(),
        }
    }
}

impl<'a> Context for GlobalContext<'a> {
    fn pop_token(&mut self) -> Option<(Token, usize)> {
        let token = self.tokens.get(self.pos);
        self.pos += 1;
        token.map(|c| c.clone())
    }

    fn front_token(&self) -> Option<&(Token, usize)> {
        self.tokens.get(self.pos)
    }

    fn set_pos(&mut self, pos: usize) {
        self.pos = pos;
    }

    fn get_pos(&self) -> usize {
        self.pos
    }

    fn get_var(&self, id: &str) -> Option<&TyType> {
        self.vars.get(id)
    }

    fn put_var(&mut self, id: &str, ty: &TyType) {
        self.vars.insert(id.to_string(), ty.clone());
    }
}

struct BlockContext<'a> {
    parent: &'a mut Context,
    vars: HashMap<String, TyType>,
}

impl<'a> BlockContext<'a> {
    fn new(parent: &'a mut Context) -> Self {
        BlockContext {
            parent,
            vars: HashMap::new(),
        }
    }
}

impl<'a> Context for BlockContext<'a> {
    fn pop_token(&mut self) -> Option<(Token, usize)> {
        self.parent.pop_token()
    }

    fn front_token(&self) -> Option<&(Token, usize)> {
        self.parent.front_token()
    }

    fn set_pos(&mut self, pos: usize) {
        self.parent.set_pos(pos);
    }

    fn get_pos(&self) -> usize {
        self.parent.get_pos()
    }

    fn get_var(&self, id: &str) -> Option<&TyType> {
        match self.vars.get(id) {
            t @ Some(..) => t,
            _ => self.parent.get_var(id),
        }
    }

    fn put_var(&mut self, id: &str, ty: &TyType) {
        self.vars.insert(id.to_string(), ty.clone());
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
            let mut context = GlobalContext::new(&tokens);
            assert_eq!(
                add(&mut context).unwrap().1,
                new_node_bin(TyType::Int, Add, Num(TyType::Int, 1), Num(TyType::Int, 1))
            );
        }

        {
            let tokens = super::tokenize("(3+5)/2").unwrap();
            let mut context = GlobalContext::new(&tokens);
            assert_eq!(
                add(&mut context).unwrap().1,
                new_node_bin(
                    TyType::Int,
                    Div,
                    new_node_bin(TyType::Int, Add, Num(TyType::Int, 3), Num(TyType::Int, 5)),
                    Num(TyType::Int, 2),
                )
            );
        }
    }

    fn stmts(tokens: &Tokens) -> Result<Vec<Node>, Error> {
        let mut nodes = Vec::new();
        let mut context = GlobalContext::new(tokens);

        loop {
            if let Some((Token::Eof, _)) = context.front_token() {
                break;
            }

            match stmt(&mut context) {
                Ok(node) => {
                    nodes.push(node);
                }
                Err(err) => {
                    eprintln!("{:?}", err);
                    return Err(err);
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
            assert_eq!(stmts(&tokens).unwrap(), vec![Node::Num(TyType::Int, 0)]);
        }

        {
            let tokens = tokenize("int a;int b;a=1;b=2;a+b;").unwrap();
            let p = stmts(&tokens);
            assert_eq!(p.is_ok(), true);
            assert_eq!(
                p.unwrap(),
                vec![
                    Node::DeclVar("a".to_owned(), TyType::Int),
                    Node::DeclVar("b".to_owned(), TyType::Int),
                    new_node_assign(
                        TyType::Int,
                        new_node_ident(TyType::Int, "a"),
                        Num(TyType::Int, 1)
                    ),
                    new_node_assign(
                        TyType::Int,
                        new_node_ident(TyType::Int, "b"),
                        Num(TyType::Int, 2)
                    ),
                    new_node_bin(
                        TyType::Int,
                        Add,
                        new_node_ident(TyType::Int, "a"),
                        new_node_ident(TyType::Int, "b")
                    ),
                ]
            );
        }

        {
            let tokens = tokenize("int a;int b;a=1;b=2;return a+b;").unwrap();
            let p = stmts(&tokens);
            assert_eq!(p.is_ok(), true);
            assert_eq!(
                p.unwrap(),
                vec![
                    Node::DeclVar("a".to_owned(), TyType::Int),
                    Node::DeclVar("b".to_owned(), TyType::Int),
                    new_node_assign(
                        TyType::Int,
                        new_node_ident(TyType::Int, "a"),
                        Num(TyType::Int, 1)
                    ),
                    new_node_assign(
                        TyType::Int,
                        new_node_ident(TyType::Int, "b"),
                        Num(TyType::Int, 2)
                    ),
                    new_node_return(new_node_bin(
                        TyType::Int,
                        Add,
                        new_node_ident(TyType::Int, "a"),
                        new_node_ident(TyType::Int, "b"),
                    )),
                ]
            );
        }

        {
            let tokens =
                tokenize("int foo;int bar;foo = 1;\nbar = 2 + 3;\nreturn foo + bar;").unwrap();
            let p = stmts(&tokens);
            assert_eq!(p.is_ok(), true);
            assert_eq!(
                p.unwrap(),
                vec![
                    Node::DeclVar("foo".to_owned(), TyType::Int),
                    Node::DeclVar("bar".to_owned(), TyType::Int),
                    new_node_assign(
                        TyType::Int,
                        new_node_ident(TyType::Int, "foo"),
                        Num(TyType::Int, 1)
                    ),
                    new_node_assign(
                        TyType::Int,
                        new_node_ident(TyType::Int, "bar"),
                        new_node_bin(TyType::Int, Add, Num(TyType::Int, 2), Num(TyType::Int, 3))
                    ),
                    new_node_return(new_node_bin(
                        TyType::Int,
                        Add,
                        new_node_ident(TyType::Int, "foo"),
                        new_node_ident(TyType::Int, "bar"),
                    )),
                ]
            );
        }

        {
            let tokens =
                tokenize("int a;int b;a=1;b=2;if (a== b) return a+b;else return 0;").unwrap();
            let p = stmts(&tokens).unwrap();
            assert_eq!(
                p,
                vec![
                    Node::DeclVar("a".to_owned(), TyType::Int),
                    Node::DeclVar("b".to_owned(), TyType::Int),
                    new_node_assign(
                        TyType::Int,
                        new_node_ident(TyType::Int, "a"),
                        Num(TyType::Int, 1)
                    ),
                    new_node_assign(
                        TyType::Int,
                        new_node_ident(TyType::Int, "b"),
                        Num(TyType::Int, 2)
                    ),
                    new_node_if(
                        new_node_bin(
                            TyType::Int,
                            Eq,
                            new_node_ident(TyType::Int, "a"),
                            new_node_ident(TyType::Int, "b"),
                        ),
                        new_node_return(new_node_bin(
                            TyType::Int,
                            Add,
                            new_node_ident(TyType::Int, "a"),
                            new_node_ident(TyType::Int, "b"),
                        )),
                        Some(new_node_return(Num(TyType::Int, 0)))
                    ),
                ]
            );
        }

        {
            let tokens = tokenize("int a;foo(1,2,a);").unwrap();
            let p = stmts(&tokens);
            assert_eq!(p.is_ok(), true);
            assert_eq!(
                p.unwrap(),
                vec![
                    Node::DeclVar("a".to_owned(), TyType::Int),
                    Call(
                        TyType::Int,
                        "foo".to_owned(),
                        vec![
                            Num(TyType::Int, 1),
                            Num(TyType::Int, 2),
                            new_node_ident(TyType::Int, "a"),
                        ]
                    ),
                ]
            );
        }

        {
            let tokens = tokenize("int x; x=3; int *y; y=&x; return *y;").unwrap();
            let p = stmts(&tokens).unwrap();
            assert_eq!(
                p,
                vec![
                    DeclVar("x".to_owned(), TyType::Int),
                    new_node_assign(
                        TyType::Int,
                        new_node_ident(TyType::Int, "x"),
                        Num(TyType::Int, 3)
                    ),
                    DeclVar("y".to_owned(), TyType::Ptr(Box::new(TyType::Int))),
                    new_node_assign(
                        TyType::Ptr(Box::new(TyType::Int)),
                        new_node_ident(TyType::Ptr(Box::new(TyType::Int)), "y"),
                        Addr(
                            TyType::Ptr(Box::new(TyType::Int)),
                            Box::new(new_node_ident(TyType::Int, "x")),
                        )
                    ),
                    new_node_return(Deref(
                        TyType::Int,
                        Box::new(new_node_ident(TyType::Ptr(Box::new(TyType::Int)), "y"),),
                    )),
                ]
            );
        }

        {
            let tokens = tokenize("int a; sizeof a;").unwrap();
            let p = stmts(&tokens).unwrap();
            assert_eq!(
                p,
                vec![DeclVar("a".to_owned(), TyType::Int), Sizeof(TyType::Int)]
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
                vec![DeclFunc(
                    "main".to_owned(),
                    Vec::new(),
                    vec![new_node_return(Num(TyType::Int, 0))]
                ),]
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
                        Return(Box::new(Num(TyType::Int, 0))),
                    ]
                ),]
            );
        }
    }
}
