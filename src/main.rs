use std::collections::VecDeque;
use std::env;

#[derive(PartialEq, Eq, Debug)]
enum Token {
    Num(i64),
    Op(OpType),
    ParenL,
    ParenR,
    Eof,
}

#[derive(PartialEq, Eq, Debug)]
enum OpType {
    Add,
    Sub,
    Mul,
    Div,
}

fn char2op(c: char) -> Option<OpType> {
    match c {
        '+' => Some(OpType::Add),
        '-' => Some(OpType::Sub),
        '*' => Some(OpType::Mul),
        '/' => Some(OpType::Div),
        _ => None,
    }
}

type Tokens = VecDeque<(Token, usize)>;

fn tokenize(text: &str) -> Tokens {
    let chars = text.clone().chars().collect::<Vec<_>>();
    let mut pos = 0;
    let mut tokens = VecDeque::new();

    while pos < chars.len() {
        if chars[pos].is_whitespace() {
            pos += 1;
            continue;
        }

        if let Some(op) = char2op(chars[pos]) {
            tokens.push_back((Token::Op(op), pos));
            pos += 1;
            continue;
        }

        if chars[pos] == '(' {
            tokens.push_back((Token::ParenL, pos));
            pos += 1;
            continue;
        }

        if chars[pos] == ')' {
            tokens.push_back((Token::ParenR, pos));
            pos += 1;
            continue;
        }

        if chars[pos].is_digit(10) {
            let cs = chars[pos..]
                .iter()
                .take_while(|c| c.is_digit(10))
                .collect::<String>();
            tokens.push_back((Token::Num(cs.parse::<i64>().unwrap()), pos));
            pos += cs.len();
            continue;
        }
        eprintln!(
            "トークナイズできません: {}",
            chars[pos..].iter().collect::<String>()
        );
        std::process::exit(1);
    }

    tokens.push_back((Token::Eof, pos));
    tokens
}

#[derive(PartialEq, Eq, Debug)]
enum Node {
    Num(i64),
    Op(OpType, Box<Node>, Box<Node>),
}

fn new_node_num(v: i64) -> Node {
    Node::Num(v)
}

fn new_node_op(ty: OpType, lhs: Node, rhs: Node) -> Node {
    Node::Op(ty, Box::new(lhs), Box::new(rhs))
}

fn add(tokens: &mut Tokens) -> Node {
    let mut node = mul(tokens);

    loop {
        match tokens[0] {
            (Token::Op(OpType::Add), _) => {
                tokens.pop_front();
                node = new_node_op(OpType::Add, node, mul(tokens))
            }
            (Token::Op(OpType::Sub), _) => {
                tokens.pop_front();
                node = new_node_op(OpType::Sub, node, mul(tokens))
            }
            _ => break,
        }
    }

    node
}

fn mul(tokens: &mut Tokens) -> Node {
    let mut node = term(tokens);

    loop {
        match tokens[0] {
            (Token::Op(OpType::Mul), _) => {
                tokens.pop_front();
                node = new_node_op(OpType::Mul, node, term(tokens));
            }
            (Token::Op(OpType::Div), _) => {
                tokens.pop_front();
                node = new_node_op(OpType::Div, node, term(tokens));
            }
            _ => break,
        }
    }
    node
}

fn term(tokens: &mut Tokens) -> Node {
    match tokens[0] {
        (Token::ParenL, _) => {
            tokens.pop_front();
            let node = add(tokens);
            match tokens[0] {
                (Token::ParenR, _pos) => {
                    tokens.pop_front();
                }
                _ => {
                    eprintln!("開き括弧に対応する閉じ括弧がありません");
                    std::process::exit(1);
                }
            }
            return node;
        }
        (Token::Num(n), _) => {
            tokens.pop_front();
            return new_node_num(n);
        }
        _ => {}
    }

    eprintln!("数値でも閉じ括弧でもないトークンです");
    std::process::exit(1);
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
        Token::Num(n) => {
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
            Token::Op(op) => {
                if *op == OpType::Add {
                    i += 1;
                    let (next_tok, pos) = &tokens[i];
                    if let Token::Num(n) = next_tok {
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
                    if let Token::Num(n) = next_tok {
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
            Token::Eof => {
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

#[cfg(test)]
mod test {
    use super::OpType::*;
    use super::*;

    #[test]
    fn tokenize_test() {
        use super::Token::*;

        assert_eq!(
            tokenize("1+1"),
            vec![(Num(1), 0), (Op(Add), 1), (Num(1), 2), (Eof, 3)]
        );

        assert_eq!(
            tokenize("(3+5)/2"),
            vec![
                (ParenL, 0),
                (Num(3), 1),
                (Op(Add), 2),
                (Num(5), 3),
                (ParenR, 4),
                (Op(Div), 5),
                (Num(2), 6),
                (Eof, 7)
            ]
        );
    }

    #[test]
    fn ast_test() {
        use super::Node::*;
        {
            let mut tokens = super::tokenize("1+1");
            assert_eq!(
                add(&mut tokens),
                Op(Add, Box::new(Num(1)), Box::new(Num(1)))
            );
        }

        {
            let mut tokens = super::tokenize("(3+5)/2");
            assert_eq!(
                add(&mut tokens),
                Op(
                    Div,
                    Box::new(Op(Add, Box::new(Num(3)), Box::new(Num(5)))),
                    Box::new(Num(2))
                )
            );
        }
    }
}
