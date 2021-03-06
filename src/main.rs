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

    let input = &args[1];
    let tokens = tokenize(&input).unwrap();
    let nodes = parse(&tokens).unwrap();

    gen_code(nodes).unwrap();
}
