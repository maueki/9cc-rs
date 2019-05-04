use failure::Error;
use std::collections::VecDeque;

use super::parse::*;
use super::*;

#[derive(Fail, Debug)]
#[fail(display = "Gen Error: {}", _0)]
pub struct GenError(String);

fn gen_lval(node: &Node, context: &mut Context) -> Result<(), Error> {
    match node {
        Node::Ident(id) => {
            let offset = context.var_put(id.clone());
            println!("  mov rax, rbp");
            println!("  sub rax, {}", offset);
            println!("  push rax");
            Ok(())
        }
        _ => Err(GenError("代入の左辺値が変数ではありません".to_owned()).into()),
    }
}

pub fn gen(node: &Node, context: &mut Context) -> Result<(), Error> {
    use super::OpType::*;

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
        Node::Call(fname, _) => {
            // rsp must be aligned 16-bytes
            println!("  xor r15,r15");
            println!("  test rsp,0xf");
            println!("  setnz r15b");
            println!("  shl r15,3");
            println!("  sub rsp, r15");
            println!("  call {}", fname);
            println!("  add rsp,r15");
            Ok(())
        }
    }
}

pub struct Context {
    var_map: VecDeque<(String, usize)>,
    cur_offset: usize,
    label_index: usize,
}

impl Context {
    pub fn new() -> Self {
        Context {
            var_map: VecDeque::new(),
            cur_offset: 0,
            label_index: 0,
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

    pub fn new_label(&mut self) -> String {
        let label = format!(".Label{}", self.label_index);
        self.label_index += 1;
        label
    }
}
