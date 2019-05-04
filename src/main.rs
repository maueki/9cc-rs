#![feature(bind_by_move_pattern_guards)]
use std::env;

use failure::Fail;

mod gen;
mod parse;
mod token;

use gen::*;
use parse::*;
use token::*;

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
