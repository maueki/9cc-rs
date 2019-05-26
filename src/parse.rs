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

trait Consume<T> {
    fn consume(&mut self, tk: T) -> Result<(), Error>;
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
/// decl_var: type ident ["[" num "]"] ";"
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
/// unary: postfix
/// unary: "*" postfix
/// unary: "&" postfix
/// unary: "+" postfix
/// unary: "-" postfix
/// unary: sizeof postfix
///
/// postfix: term
/// postfix: term "[" assign "]"
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
    Array(Box<TyType>, usize),
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
    DeclFunc(TyType, String, Vec<Param>, Vec<Node>),
    DeclVar(String, TyType),
    Addr(TyType, Box<Node>),
    Deref(TyType, Box<Node>),
    Sizeof(TyType),
}

pub fn get_type(node: &Node) -> TyType {
    match node {
        Node::Num(ty, ..) => ty.clone(),
        Node::Bin(ty, ..) => ty.clone(),
        Node::Assign(ty, ..) => ty.clone(),
        Node::Ident(ty, ..) => ty.clone(),
        Node::Call(ty, ..) => ty.clone(),
        Node::Addr(ty, ..) => ty.clone(),
        Node::Deref(ty, ..) => ty.clone(),
        Node::DeclFunc(ty, ..) => ty.clone(),
        Node::Sizeof(..) => TyType::Int,
        _ => unimplemented!(),
    }
}

pub fn deref_type(ty: &TyType) -> TyType {
    match ty {
        TyType::Ptr(ty) => *ty.clone(),
        TyType::Array(ty, ..) => *ty.clone(),
        _ => unimplemented!(),
    }
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

fn program<T: Context>(context: &mut T) -> Result<Vec<Node>, Error> {
    let mut nodes = Vec::new();

    loop {
        if let Some((Token::Eof, _)) = context.front_token() {
            break;
        }
        nodes.push(decl_func(context)?);
    }

    Ok(nodes)
}

fn decl_func<T: Context>(context: &mut T) -> Result<Node, Error> {
    let context = &mut BlockContext::new(context);

    let t = ty(context)?; // TODO

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

    context.consume('(')?;
    let ps = params(context)?;
    context.consume(')')?;

    context.consume('{')?;
    let mut nodes = Vec::new();
    while let Ok(node) = stmt(context) {
        nodes.push(node);
    }
    context.consume('}')?;

    Ok(Node::DeclFunc(t, fname.clone(), ps, nodes))
}

fn params<T: Context>(context: &mut T) -> Result<Vec<Param>, Error> {
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

        if context.consume(',').is_err() {
            break;
        }
    }

    Ok(ps)
}

fn stmt<T: Context>(context: &mut T) -> Result<Node, Error> {
    match context.front_token() {
        Some((Token::Return, _)) => {
            context.pop_token();
            let node = new_node_return(assign(context)?.1);
            context.consume(';')?;
            Ok(node)
        }
        Some((Token::If, _)) => {
            context.pop_token();
            context.consume('(')?;
            let node_cond = assign(context)?.1;
            context.consume(')')?;

            let node_then = stmt(context)?;
            let node_else = if let Ok(..) = context.consume(&Token::Else) {
                Some(stmt(context)?)
            } else {
                None
            };

            Ok(new_node_if(node_cond, node_then, node_else))
        }
        Some((Token::Char('{'), _)) => {
            let context = &mut BlockContext::new(context);
            context.pop_token();
            let mut stmts = Vec::new();
            while let Ok(node) = stmt(context) {
                stmts.push(node);
            }
            context.consume('}')?;
            Ok(Node::Block(stmts))
        }
        _ => {
            let pos = context.get_pos(); // FIXME:
            if let Ok(node) = decl_var(context) {
                Ok(node)
            } else {
                context.set_pos(pos);
                let node = assign(context)?.1;
                context.consume(';')?;
                Ok(node)
            }
        }
    }
}

fn decl_var<T: Context>(context: &mut T) -> Result<Node, Error> {
    let mut t = ty(context)?;

    if let Some((Token::Ident(var), _)) = context.pop_token() {
        let var = var.clone();

        // TODO: 数値なし(int a[])や多重配列は未実装
        if context.consume('[').is_ok() {
            if let Some((Token::Num(n), _)) = context.front_token() {
                t = TyType::Array(Box::new(t), *n as usize);
                context.pop_token();
            } else {
                unimplemented!();
            }
            context.consume(']')?;
        }
        context.consume(';')?;
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

fn ty<T: Context>(context: &mut T) -> Result<TyType, Error> {
    let mut tytype = match context.front_token() {
        Some((Token::Ident(tname), _)) => str2ty(tname)?,
        _ => return Err(ParseError("ty: Unexpected Token".to_owned(), 0).into()),
    };

    context.pop_token();

    while let Some((Token::Char('*'), _)) = context.front_token() {
        context.pop_token();
        tytype = TyType::Ptr(Box::new(tytype));
    }

    Ok(tytype)
}

fn assign<T: Context>(context: &mut T) -> Result<(TyType, Node), Error> {
    let (lty, mut lnode) = equality(context)?;
    while context.consume('=').is_ok() {
        let (_rty, rnode) = assign(context)?;
        // TODO: 型チェック
        lnode = new_node_assign(lty.clone(), lnode, rnode);
    }
    Ok((lty, lnode))
}

fn equality<T: Context>(context: &mut T) -> Result<(TyType, Node), Error> {
    let (mut lty, mut lnode) = relational(context)?;

    loop {
        if context.consume(&Token::Eq).is_ok() {
            let (_rty, rnode) = relational(context)?;
            // TODO: 型チェック
            lty = TyType::Int;
            lnode = new_node_bin(lty.clone(), BinOp::Eq, lnode, rnode);
        } else if context.consume(&Token::Ne).is_ok() {
            let (_rty, rnode) = relational(context)?;
            // TODO: 型チェック
            lty = TyType::Int;
            lnode = new_node_bin(lty.clone(), BinOp::Ne, lnode, rnode);
        } else {
            break;
        }
    }

    Ok((lty, lnode))
}

fn relational<T: Context>(context: &mut T) -> Result<(TyType, Node), Error> {
    let (mut lty, mut lnode) = add(context)?;

    loop {
        if context.consume(&Token::Le).is_ok() {
            let (_rty, rnode) = add(context)?;
            // TODO: 型チェック
            lty = TyType::Int;
            lnode = new_node_bin(lty.clone(), BinOp::Le, lnode, rnode);
        } else if context.consume(&Token::Ge).is_ok() {
            let (_rty, rnode) = add(context)?;
            // TODO: 型チェック
            lty = TyType::Int;
            lnode = new_node_bin(lty.clone(), BinOp::Ge, lnode, rnode);
        } else {
            break;
        }
    }
    Ok((lty, lnode))
}

fn add<T: Context>(context: &mut T) -> Result<(TyType, Node), Error> {
    let (mut lty, mut lnode) = mul(context)?;

    loop {
        match context.front_token() {
            Some((Token::Char('+'), _pos)) => {
                context.pop_token();
                let (_, rnode) = mul(context)?;
                let ret = add_node(lnode, rnode)?;
                lnode = ret.1;
                lty = ret.0;
            }
            Some((Token::Char('-'), pos)) => {
                let pos = *pos;
                context.pop_token();
                let (rty, rnode) = mul(context)?;
                lnode = match (lty.clone(), rty) {
                    (TyType::Int, TyType::Ptr(_ty)) => {
                        return Err(ParseError("引算不可能な型です".to_owned(), pos).into())
                    }
                    (TyType::Ptr(ty), TyType::Int) => new_node_bin(
                        TyType::Ptr(ty.clone()),
                        BinOp::Sub,
                        lnode,
                        new_node_bin(TyType::Int, BinOp::Mul, rnode, Node::Sizeof(*ty)),
                    ),
                    (TyType::Int, TyType::Int) => {
                        new_node_bin(lty.clone(), BinOp::Sub, lnode, rnode)
                    }
                    _ => {
                        return Err(ParseError("引算不可能な型です".to_owned(), pos).into())
                    }
                };
            }
            _ => break,
        }
    }

    Ok((lty, lnode))
}

fn add_node(lnode: Node, rnode: Node) -> Result<(TyType, Node), Error> {
    let lty = get_type(&lnode);
    let rty = get_type(&rnode);

    let node = match (lty.clone(), rty) {
        (TyType::Int, TyType::Ptr(ty)) => new_node_bin(
            TyType::Ptr(ty.clone()),
            BinOp::Add,
            new_node_bin(TyType::Int, BinOp::Mul, lnode, Node::Sizeof(*ty)),
            rnode,
        ),
        (TyType::Ptr(ty), TyType::Int) => new_node_bin(
            TyType::Ptr(ty.clone()),
            BinOp::Add,
            lnode,
            new_node_bin(TyType::Int, BinOp::Mul, rnode, Node::Sizeof(*ty)),
        ),
        (TyType::Int, TyType::Array(ty, _)) => new_node_bin(
            TyType::Ptr(ty.clone()),
            BinOp::Add,
            new_node_bin(TyType::Int, BinOp::Mul, lnode, Node::Sizeof(*ty)),
            rnode,
        ),
        (TyType::Array(ty, _), TyType::Int) => new_node_bin(
            TyType::Ptr(ty.clone()),
            BinOp::Add,
            lnode,
            new_node_bin(TyType::Int, BinOp::Mul, rnode, Node::Sizeof(*ty)),
        ),
        (TyType::Int, TyType::Int) => new_node_bin(lty.clone(), BinOp::Add, lnode, rnode),
        _ => return Err(ParseError("加算不可能な型です".to_owned(), 0).into()),
    };

    Ok((get_type(&node), node))
}

fn mul<T: Context>(context: &mut T) -> Result<(TyType, Node), Error> {
    let (lty, mut lnode) = unary(context)?;

    loop {
        match context.front_token() {
            Some((Token::Char('*'), pos)) => {
                let pos = *pos;
                context.pop_token();
                let (rty, rnode) = unary(context)?;
                if lty != TyType::Int || rty != TyType::Int {
                    return Err(ParseError("乗算不可能な型です".to_owned(), pos).into());
                }
                lnode = new_node_bin(lty.clone(), BinOp::Mul, lnode, rnode);
            }
            Some((Token::Char('/'), pos)) => {
                let pos = *pos;
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

fn unary<T: Context>(context: &mut T) -> Result<(TyType, Node), Error> {
    if let Ok(..) = context.consume('*') {
        let (ty, node) = postfix(context)?;
        match ty {
            TyType::Ptr(ty) => Ok((*ty.clone(), Node::Deref(*ty.clone(), Box::new(node)))),
            TyType::Array(ty, ..) => Ok((*ty.clone(), Node::Deref(*ty.clone(), Box::new(node)))),
            t => {
                Err(ParseError(
                    format!("デリファレンスできない型です: {:?}", t),
                    0, /*FIXME*/
                )
                .into())
            }
        }
    } else if let Ok(..) = context.consume('&') {
        let (ty, node) = postfix(context)?;
        Ok((
            TyType::Ptr(Box::new(ty.clone())),
            Node::Addr(TyType::Ptr(Box::new(ty.clone())), Box::new(node)),
        ))
    } else if let Ok(..) = context.consume('+') {
        postfix(context)
    } else if let Ok(..) = context.consume('-') {
        let (ty, node) = postfix(context)?;
        Ok((
            ty.clone(),
            new_node_bin(ty, BinOp::Sub, new_node_num(TyType::Int, 0), node),
        ))
    } else if let Some((Token::Sizeof, _)) = context.front_token() {
        context.pop_token();
        let (ty, _) = unary(context)?;
        Ok((TyType::Int, Node::Sizeof(ty)))
    } else {
        postfix(context)
    }
}

fn postfix<T: Context>(context: &mut T) -> Result<(TyType, Node), Error> {
    let (ty, mut node) = term(context)?;

    if context.consume('[').is_ok() {
        let (_, rnode) = assign(context)?;
        context.consume(']')?;
        let (ty, ret_node) = add_node(node, rnode)?;
        node = Node::Deref(deref_type(&ty), Box::new(ret_node))
    }

    Ok((ty, node))
}

fn term<T: Context>(context: &mut T) -> Result<(TyType, Node), Error> {
    match context.front_token() {
        Some((Token::Char('('), _)) => {
            context.pop_token();
            let (ty, node) = add(context)?;
            context.consume(')')?;
            Ok((ty, node))
        }
        Some((Token::Num(n), _)) => {
            let n = *n;
            context.pop_token();
            Ok((TyType::Int, new_node_num(TyType::Int, n)))
        }
        Some((Token::Ident(id), pos)) => {
            let pos = *pos;
            let id = id.clone();
            context.pop_token();
            if let Some((Token::Char('('), _)) = context.front_token() {
                context.pop_token();
                let args = arguments(context)?;
                context.consume(')')?;
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

fn arguments<T: Context>(context: &mut T) -> Result<Vec<Node>, Error> {
    let mut nodes = Vec::new();

    match assign(context) {
        Ok((_ty, n)) => nodes.push(n),
        _ => return Ok(nodes),
    }

    while context.consume(',').is_ok() {
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
        token.cloned()
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

impl<T> Consume<&Token> for T
where
    T: Context,
{
    fn consume(&mut self, token: &Token) -> Result<(), Error> {
        match self.front_token() {
            Some((tk, _pos)) if tk == token => {
                self.pop_token();
                Ok(())
            }
            Some((tk, pos)) => Err(ParseError(
                format!("consume: expect {:?}, but {:?}", token, tk.clone()),
                *pos,
            )
            .into()),
            None => Err(ParseError("Invalid Eof".to_owned(), 0).into()),
        }
    }
}

impl<T> Consume<char> for T
where
    T: Context,
{
    fn consume(&mut self, c: char) -> Result<(), Error> {
        self.consume(&Token::Char(c))
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

        {
            let tokens = tokenize("int a[10];").unwrap();
            let p = stmts(&tokens).unwrap();
            assert_eq!(
                p,
                vec![DeclVar(
                    "a".to_owned(),
                    TyType::Array(Box::new(TyType::Int), 10)
                )]
            );
        }

        {
            let tokens = tokenize("int a[10]; a[5];").unwrap();
            let p = stmts(&tokens).unwrap();
            assert_eq!(
                p,
                vec![
                    DeclVar("a".to_owned(), TyType::Array(Box::new(TyType::Int), 10)),
                    Deref(
                        TyType::Int,
                        Box::new(new_node_bin(
                            TyType::Ptr(Box::new(TyType::Int)),
                            Add,
                            new_node_ident(TyType::Array(Box::new(TyType::Int), 10), "a"),
                            new_node_bin(
                                TyType::Int,
                                Mul,
                                new_node_num(TyType::Int, 5),
                                Sizeof(TyType::Int)
                            ),
                        ))
                    ),
                ]
            );
        }

        {
            let tokens = tokenize("int a[2]; *a = 1;").unwrap();
            let p = stmts(&tokens).unwrap();
            assert_eq!(
                p,
                vec![
                    DeclVar("a".to_owned(), TyType::Array(Box::new(TyType::Int), 2)),
                    new_node_assign(
                        TyType::Int,
                        Deref(
                            TyType::Int,
                            Box::new(new_node_ident(TyType::Array(Box::new(TyType::Int), 2), "a"))
                        ),
                        new_node_num(TyType::Int, 1)
                    )
                ]
            );
        }

        {
            let tokens = tokenize("return -3;").unwrap();
            let p = stmts(&tokens).unwrap();
            assert_eq!(
                p,
                vec![new_node_return(new_node_bin(
                    TyType::Int,
                    BinOp::Sub,
                    new_node_num(TyType::Int, 0),
                    new_node_num(TyType::Int, 3)
                ))]
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
                    TyType::Int,
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
                    TyType::Int,
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
