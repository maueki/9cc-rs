use std::env;

enum Token {
    NUM(i64),
    OP(OpType),
    EOF,
}

#[derive(PartialEq, Eq)]
enum OpType {
    Add,
    Sub,
}

fn tokenize(text: &str) -> Vec<(Token, usize)> {
    let chars = text.clone().chars().collect::<Vec<_>>();
    let mut pos = 0;
    let mut tokens = Vec::new();

    while pos < chars.len() {
        if chars[pos].is_whitespace() {
            pos += 1;
            continue;
        }

        if chars[pos] == '+' {
            tokens.push((Token::OP(OpType::Add), pos));
            pos += 1;
            continue;
        }

        if chars[pos] == '-' {
            tokens.push((Token::OP(OpType::Sub), pos));
            pos += 1;
            continue;
        }

        if chars[pos].is_digit(10) {
            let cs = chars[pos..]
                .iter()
                .take_while(|c| c.is_digit(10))
                .collect::<String>();
            tokens.push((Token::NUM(cs.parse::<i64>().unwrap()), pos));
            pos += cs.len();
            continue;
        }
        eprintln!(
            "トークナイズできません: {}",
            chars[pos..].iter().collect::<String>()
        );
        std::process::exit(1);
    }

    tokens.push((Token::EOF, pos));
    tokens
}

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() != 2 {
        eprintln!("引数の個数が正しくありません");
        ::std::process::exit(1);
    }

    let input = &args[1];
    let tokens = tokenize(&input);

    println!(".intel_syntax noprefix");
    println!(".global main");
    println!("main:");

    match tokens[0].0 {
        Token::NUM(n) => {
            println!("mov rax, {}", n);
        }
        _ => {
            eprintln!("最初の項が数ではありません");
            std::process::exit(1);
        }
    }

    let mut i = 1;
    while i < tokens.len() {
        let (tok, pos) = &tokens[i];

        match tok {
            Token::OP(op) => {
                if *op == OpType::Add {
                    i += 1;
                    let (next_tok, pos) = &tokens[i];
                    if let Token::NUM(n) = next_tok {
                        println!("  add rax, {}", n);
                        i += 1;
                    } else {
                        eprintln!("予期しないトークンです: {}", &input[*pos..]);
                        std::process::exit(1);
                    }

                    continue;
                }

                if *op == OpType::Sub {
                    i += 1;
                    let (next_tok, pos) = &tokens[i];
                    if let Token::NUM(n) = next_tok {
                        println!("  sub rax, {}", n);
                        i += 1;
                    } else {
                        eprintln!("予期しないトークンです: {}", &input[*pos..]);
                        std::process::exit(1);
                    }
                    continue;
                }

                eprintln!("予期しないトークンです: {}", &input[*pos..]);
                std::process::exit(1);
            }
            Token::EOF => {
                break;
            }
            _ => {
                eprintln!("予期しないトークンです: {}", &input[*pos..]);
                std::process::exit(1);
            }
        }
    }

    println!("  ret");
}
