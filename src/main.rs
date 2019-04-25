use std::env;

enum Token {
    NUM(i64),
    OP(String),
    EOF,
}

fn tokenize(text: &String) -> Vec<(Token, usize)> {
    let chars = text.clone().chars().collect::<Vec<_>>();
    let mut pos = 0;
    let mut tokens = Vec::new();

    while pos < chars.len() {
        if chars[pos].is_whitespace() {
            pos += 1;
            continue;
        }

        if chars[pos] == '+' || chars[pos] == '-' {
            tokens.push((Token::OP(chars[pos].to_string()), pos));
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
                if op == "+" {
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

                if op == "-" {
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
