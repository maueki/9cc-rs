use failure::Error;
use std::collections::VecDeque;

use super::parse::*;
use super::*;
use lazy_static::lazy_static;

lazy_static! {
    pub static ref REG_ARGS: Vec<&'static str> = { vec!["rdi", "rsi", "rdx", "rcx", "r8", "r9"] };
}

fn sizeof(ty: &TyType) -> usize {
    match ty {
        TyType::Int => 4,
        TyType::Ptr(..) => 8,
        TyType::Array(ty, size) => size * sizeof(ty),
        _ => unreachable!(),
    }
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

fn gen_lval(node: &Node, context: &mut Context) -> Result<TyType, Error> {
    match node {
        Node::Ident(_, id) => {
            let (ty, offset) = context.var_get(id)?;
            println!("  mov rax, rbp");
            println!("  sub rax, {}", offset);
            println!("  push rax");
            Ok(ty)
        }
        Node::Deref(_, ptr) => gen(ptr, context),
        _ => Err(GenError("代入の左辺値が変数ではありません".to_owned()).into()),
    }
}

fn gen(node: &Node, context: &mut Context) -> Result<TyType, Error> {
    use super::BinOp::*;

    match node {
        Node::Return(lhs) => {
            gen(lhs, context)?;
            println!("  pop rax");
            println!("  mov rsp, rbp");
            println!("  pop rbp");
            println!("  ret");
            Ok(TyType::Void)
        }
        Node::Num(_, v) => {
            println!("  mov rax, {}", v);
            println!("  push rax");
            Ok(TyType::Int)
        }
        Node::Ident(..) => {
            let ty = gen_lval(node, context)?;
            println!("  pop rax");
            if sizeof(&ty) == 4 {
                println!("  mov eax, [rax]");
                println!("  movsx rax, eax");
            } else {
                println!("  mov rax, [rax]");
            }
            println!("  push rax");
            Ok(ty)
        }
        Node::Assign(_, lhs, rhs) => {
            let ty = gen_lval(&lhs, context)?;
            gen(rhs, context)?;
            println!("  pop rdi");
            println!("  pop rax");
            if sizeof(&ty) == 4 {
                // FIXME: 符号を含めてrdiをediへコピー
                println!("  mov r10, 0x8000000000000000");
                println!("  and r10, rdi");
                println!("  cmp r10, 0");
                let label = context.new_label();
                println!("  je {}", label);
                println!("  xor rdi, 0xffffffffffffffff");
                println!("  sub rdi, 1");
                println!("  xor edi, 0xffffffff");
                println!("  sub edi, 1");

                println!("{}:", label);
                println!("  mov [rax], edi");

                println!("  movsx rdi, edi");
            } else {
                println!("  mov [rax], rdi");
            }
            println!("  push rdi");
            Ok(ty)
        }
        Node::Bin(_, op, lhs, rhs) => {
            let lty = gen(lhs, context)?;
            let rty = gen(rhs, context)?;

            println!("  pop rdi");
            println!("  pop rax");

            let ty = match op {
                Add => match (lty, rty) {
                    (TyType::Int, TyType::Int) => {
                        println!("  add rax, rdi");
                        Ok(TyType::Int)
                    }
                    (TyType::Ptr(lt), TyType::Int) => {
                        println!("  mov rsi, rax");
                        println!("  mov rax, {}", sizeof(&lt));
                        println!("  mul rdi");
                        println!("  add rax, rsi");
                        Ok(TyType::Ptr(lt))
                    }
                    (TyType::Int, TyType::Ptr(lt)) => {
                        println!("  mov rsi, rdi");
                        println!("  mov rdi, {}", sizeof(&lt));
                        println!("  mul rdi");
                        println!("  add rax, rsi");
                        Ok(TyType::Ptr(lt))
                    }
                    (l, r) => {
                        Err(GenError(format!("invalid operands ( {:?} + {:?} )", l, r)).into())
                    }
                },
                Sub => {
                    println!("  sub rax, rdi");
                    Ok(TyType::Int) // FIXME
                }
                Mul => {
                    println!("  mul rdi");
                    Ok(TyType::Int) // FIXME
                }
                Div => {
                    println!("  mov rdx, 0");
                    println!("  div rdi");
                    Ok(TyType::Int) // FIXME
                }
                Eq => {
                    println!("  cmp rax, rdi");
                    println!("  sete al");
                    println!("  movzb rax, al");
                    Ok(TyType::Int) // FIXME
                }
                Ne => {
                    println!("  cmp rax, rdi");
                    println!("  setne al");
                    println!("  movzb rax, al");
                    Ok(TyType::Int) // FIXME
                }
                Le => {
                    println!("  cmp rax, rdi");
                    println!("  setle al");
                    println!("  movzb rax, al");
                    Ok(TyType::Int) // FIXME
                }
                Ge => {
                    println!("  cmp rdi, rax");
                    println!("  setle al");
                    println!("  movzb rax, al");
                    Ok(TyType::Int) // FIXME
                }
            };

            println!("  push rax");
            ty
        }
        Node::If(cond, node_then, node_else) => {
            gen(cond, context)?;
            println!("  pop rax");
            println!("  cmp rax, 0");
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
                    println!("{}:", else_label);
                }
            }
            Ok(TyType::Void)
        }
        Node::Block(nodes) => {
            for n in nodes {
                gen(n, context)?;
                println!("  pop rax");
            }

            println!("  push rax");
            Ok(TyType::Void)
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
            Ok(TyType::Int) // FIXME
        }
        Node::DeclFunc(fname, params, stmts) => {
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
                if sizeof(&p.ty) == 4 {
                    println!("  mov rdi, {}", REG_ARGS[i]);
                    // FIXME: 符号を含めてrdiをediへコピー
                    println!("  mov r10, 0x8000000000000000");
                    println!("  and r10, rdi");
                    println!("  cmp r10, 0");
                    let label = context.new_label();
                    println!("  je {}", label);
                    println!("  xor rdi, 0xffffffffffffffff");
                    println!("  sub rdi, 1");
                    println!("  xor edi, 0xffffffff");
                    println!("  sub edi, 1");

                    println!("{}:", label);
                    println!("  mov [rax], edi");
                } else {
                    println!("  mov [rax], {}", REG_ARGS[i]);
                }
            }

            for node in stmts {
                gen(&node, &mut context)?;
                println!("  pop rax");
            }

            // TODO: Node::Returnで追加されているはずだが…
            println!("  mov rsp, rbp");
            println!("  pop rbp");
            println!("  ret");

            Ok(TyType::Void)
        }
        Node::DeclVar(ident, ty) => {
            context.var_put(ident, &ty);
            Ok(ty.clone())
        }
        Node::Deref(dty, ptr) => {
            let ty = gen(ptr, context)?;
            println!("  pop rax");
            if sizeof(dty) == 4 {
                println!("  mov eax, [rax]");
                println!("  movsx rax, eax");
            } else {
                println!("  mov rax, [rax]");
            }
            println!("  push rax");
            match ty {
                TyType::Ptr(ty) => Ok(*ty),
                _ => unreachable!(), // FIXME:
            }
        }
        Node::Addr(_, id) => {
            let ty = gen_lval(id, context)?;
            Ok(TyType::Ptr(Box::new(ty)))
        }
        Node::Sizeof(ty) => {
            println!("  mov rax, {}", sizeof(&ty));
            println!("  push rax");
            Ok(TyType::Int)
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

        // アライメント調整
        let size = sizeof(ty);
        if size == 8 {
            if self.cur_offset % 8 != 0 {
                assert!(self.cur_offset % 4 == 0);
                self.cur_offset += 4;
            }
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
