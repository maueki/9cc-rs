use failure::Error;
use std::collections::VecDeque;

use super::parse::*;
use super::*;
use lazy_static::lazy_static;

lazy_static! {
    pub static ref REG_ARGS: Vec<&'static str> = { vec!["rdi", "rsi", "rdx", "rcx", "r8", "r9"] };
    pub static ref REG_ARGS32: Vec<&'static str> =
        { vec!["edi", "esi", "edx", "ecx", "r8d", "r9d"] };
    pub static ref REGS: Vec<&'static str> =
        { vec!["r10", "r11", "rbx", "r12", "r13", "r14", "r15"] };
    pub static ref REGS32: Vec<&'static str> =
        { vec!["r10d", "r11d", "ebx", "r12d", "r13d", "r14d", "r15d"] };
}

fn sizeof(ty: &TyType) -> usize {
    match ty {
        TyType::Int => 4,
        TyType::Ptr(..) => 8,
        TyType::Array(ty, size) => size * sizeof(ty),
    }
}

fn reg(index: usize, ty: &TyType) -> &'static str {
    assert!(index <= REGS.len());

    match ty {
        TyType::Int => REGS32[index],
        TyType::Ptr(..) => REGS[index],
        TyType::Array(..) => REGS[index], // TODO
    }
}

fn reg64(index: usize) -> &'static str {
    assert!(index <= REGS.len());
    REGS[index]
}

fn argreg(index: usize, ty: &TyType) -> &'static str {
    assert!(index <= REG_ARGS.len());

    match ty {
        TyType::Int => REG_ARGS32[index],
        TyType::Ptr(..) => REG_ARGS[index],
        _ => unimplemented!(), // TODO
    }
}

fn retreg(ty: &TyType) -> &'static str {
    match ty {
        TyType::Int => "eax",
        TyType::Ptr(..) => "rax",
        _ => unimplemented!(), // TODO
    }
}

fn retreg64() -> &'static str {
    "rax"
}

#[derive(Fail, Debug)]
#[fail(display = "Gen Error: {}", _0)]
pub struct GenError(String);

pub fn gen_code(nodes: Vec<Node>) -> Result<(), Error> {
    let mut context = Context::new();
    for n in nodes {
        gen(&n, &mut context)?;
    }

    Ok(())
}

fn gen_lval(node: &Node, context: &mut Context) -> Result<(), Error> {
    match node {
        Node::Ident(_, id) => {
            let (_, offset) = context.var_get(id)?;
            println!("  mov rax, rbp");
            println!("  sub rax, {}", offset);
            println!("  push rax");
            Ok(())
        }
        Node::Deref(_, ptr) => gen(ptr, context),
        _ => Err(GenError("代入の左辺値が変数ではありません".to_owned()).into()),
    }
}

fn gen(node: &Node, context: &mut Context) -> Result<(), Error> {
    use super::BinOp::*;

    match node {
        Node::Return(lhs) => {
            gen(lhs, context)?;
            println!("  pop rax");
            println!("  mov rsp, rbp");
            println!("  pop rbp");
            println!("  ret");
            Ok(())
        }
        Node::Num(ty, v) => {
            println!("  mov {}, {}", reg(0, ty), v);
            println!("  push {}", reg64(0));
            Ok(())
        }
        Node::Ident(ty, _) => {
            gen_lval(node, context)?;
            match ty {
                TyType::Array(..) => {}
                _ => {
                    println!("  pop rax");
                    println!("  mov {}, [rax]", reg(0, ty));
                    println!("  push {}", reg64(0));
                }
            }

            Ok(())
        }
        Node::Assign(ty, lhs, rhs) => {
            gen_lval(&lhs, context)?;
            gen(rhs, context)?;

            println!("  pop {}", reg64(0));
            println!("  pop rax");
            println!("  mov [rax], {}", reg(0, ty));
            println!("  push rdi");
            Ok(())
        }
        Node::Bin(ty, op, lhs, rhs) => {
            gen(lhs, context)?;
            gen(rhs, context)?;

            println!("  pop {}", reg64(0));
            println!("  pop {}", retreg64());

            match op {
                Add => {
                    println!("  add {}, {}", retreg(ty), reg(0, ty));
                }
                Sub => {
                    println!("  sub {}, {}", retreg(ty), reg(0, ty));
                }
                Mul => {
                    println!("  imul {}", reg(0, ty));
                }
                Div => {
                    //                    println!("  mov rdx, 0");
                    println!("  cqo");
                    println!("  idiv {}", reg(0, ty));
                }
                Eq => {
                    println!("  cmp {}, {}", retreg64(), reg64(0));
                    println!("  sete al");
                    println!("  movzb {}, al", retreg64());
                }
                Ne => {
                    println!("  cmp {}, {}", retreg64(), reg64(0));
                    println!("  setne al");
                    println!("  movzb {}, al", retreg64());
                }
                Le => {
                    println!("  cmp {}, {}", retreg64(), reg64(0));
                    println!("  setle al");
                    println!("  movzb {}, al", retreg64());
                }
                Ge => {
                    println!("  cmp {}, {}", retreg64(), reg64(0));
                    println!("  setle al");
                    println!("  movzb {}, al", retreg64());
                }
            };

            println!("  push rax");
            Ok(())
        }
        Node::If(cond, node_then, node_else) => {
            gen(cond, context)?;

            let cond_t = get_type(cond);

            println!("  pop {}", reg64(0));
            println!("  cmp {}, 0", reg(0, &cond_t));
            let else_label = context.new_label();
            println!("  je {}", else_label);
            gen(node_then, context)?;
            match node_else {
                Some(node) => {
                    let end_label = context.new_label();
                    println!("  jmp {}", end_label);
                    println!("{}:", else_label);
                    gen(node, context)?;
                    println!("{}:", end_label);
                }
                None => {
                    println!("  mov rax, 0xdeadbeef"); // dummy
                    println!("  push rax"); // dummy
                    println!("{}:", else_label);
                }
            }
            Ok(())
        }
        Node::Block(nodes) => {
            for n in nodes {
                gen(n, context)?;
                println!("  pop rax");
            }

            println!("  push rax");
            Ok(())
        }
        Node::Call(_, fname, args) => {
            if args.len() > REG_ARGS.len() {
                return Err(GenError(format!("{}: 引数が多すぎます", fname)).into());
            }

            for node in args.iter().rev() {
                gen(node, context)?;
            }

            for reg in REG_ARGS.iter().take(args.len()) {
                println!("  pop {}", reg);
            }

            // rsp must be aligned 16-bytes
            println!("  xor r15,r15");
            println!("  test rsp,0xf");
            println!("  setnz r15b");
            println!("  shl r15,3");
            println!("  sub rsp, r15");
            println!("  call {}", fname);
            println!("  add rsp,r15");
            println!("  push rax");
            Ok(())
        }
        Node::DeclFunc(_ty, fname, params, stmts) => {
            if params.len() > REG_ARGS.len() {
                return Err(GenError(format!("{}: 引数が多すぎます", fname)).into());
            }

            let mut context = Context::with_parent(context);
            println!("{}:", fname);
            println!("  push rbp");
            println!("  mov rbp, rsp");
            println!("  sub rsp, 208"); // FIXME:

            for (i, p) in params.iter().enumerate() {
                let offset = context.var_put(&p.name, &p.ty);
                println!("  mov rax, rbp");
                println!("  sub rax, {}", offset);
                println!("  mov [rax], {}", argreg(i, &p.ty));
            }

            for node in stmts {
                gen(&node, &mut context)?;
                println!("  pop rax");
            }

            // TODO: Node::Returnで追加されているはずだが…
            println!("  mov rsp, rbp");
            println!("  pop rbp");
            println!("  ret");

            Ok(())
        }
        Node::DeclVar(ident, ty) => {
            context.var_put(ident, &ty);
            println!("  mov rax, 0xdeadbeef"); // dummy
            println!("  push rax"); // dummy
            Ok(())
        }
        Node::Deref(ty, ptr) => {
            gen(ptr, context)?;
            println!("  pop rax");
            println!("  mov {}, [rax]", reg(0, ty));
            println!("  push {}", reg64(0));
            Ok(())
        }
        Node::Addr(_, id) => {
            gen_lval(id, context)?;
            Ok(())
        }
        Node::Sizeof(ty) => {
            println!("  mov rax, {}", sizeof(&ty));
            println!("  push rax");
            Ok(())
        }
    }
}

struct Context<'a> {
    var_map: VecDeque<(String, TyType, usize)>,
    cur_offset: usize,
    label_index: usize,
    parent: Option<&'a Context<'a>>,
}

impl<'a> Context<'a> {
    fn new() -> Self {
        Context {
            var_map: VecDeque::new(),
            cur_offset: 8, // offset for rbp
            label_index: 0,
            parent: None,
        }
    }

    fn with_parent(parent: &'a Context<'a>) -> Self {
        Context {
            var_map: VecDeque::new(),
            cur_offset: 8, // offset for rbp
            label_index: 0,
            parent: Some(parent),
        }
    }

    fn var_put(&mut self, ident: &str, ty: &TyType) -> usize {
        for var in self.var_map.iter() {
            if var.0 == *ident {
                return var.2;
            }
        }

        // 以下変数未登録時

        // アライメント調整
        let size = match ty {
            TyType::Array(elmt, ..) => sizeof(elmt),
            ty => sizeof(ty),
        };
        if size == 8 && self.cur_offset % 8 != 0 {
            assert!(self.cur_offset % 4 == 0);
            self.cur_offset += 4;
        }

        let cur_offset = self.cur_offset;
        self.var_map
            .push_front((ident.to_string(), ty.clone(), cur_offset));

        self.cur_offset += sizeof(ty);
        cur_offset
    }

    fn var_get(&mut self, ident: &str) -> Result<(TyType, usize), Error> {
        for var in self.var_map.iter() {
            if var.0 == *ident {
                return Ok((var.1.clone(), var.2));
            }
        }

        Err(GenError(format!("Undefine variable: {}", ident)).into())
    }

    fn new_label(&mut self) -> String {
        let label = format!(".Label{}", self.label_index);
        self.label_index += 1;
        label
    }
}
