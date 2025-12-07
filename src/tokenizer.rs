use std::fmt;
use regex::Regex;
use verbose_macros::{verbose, debug};
use crate::token_types::TokenType;

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line_number: usize,
}
// impl fmt::Debug for Token {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//         write!(f, "tokenType={}, {}, lexeme={}, line_number={}", self.token_type, self.token_type as u8, self.lexeme, self.line_number)
//     }
// }

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Token(tokenType=<TokenType.{}: {}>, lexeme='{}', line_number={})",
            self.token_type,
            self.token_type as u8,
            self.lexeme,
            self.line_number
        )
    }
}

fn make_token(token_type: TokenType, lexeme: String, line_number: usize) -> Token {
    debug!("{}", lexeme);
    let t = Token{token_type, lexeme, line_number};
    debug!("Created token: {:?}", t);

    return t;
}

fn is_end_of_line(current: usize, line_len: usize) -> bool {
    return (current + 1) >= line_len;
}

fn peek(c: char, line: &str, current: usize, line_len: usize) -> bool {
    if is_end_of_line(current, line_len) {
        return false;
    }
    let next = line.chars().nth(current + 1);
    return match next {
        Some(x) => x == c,
        None => false,
    }
}

fn handle_string(line: &str, current: usize, line_number: usize) -> (Token, usize){
    let re = Regex::new(r####""((?:[^"\\]|\\.)*)""####).unwrap();
    let substr = &line[current..];
    debug!("Substr: {:?}", substr);
    if let Some(caps) = re.captures(substr) {
        let string_literal = caps.get(0).unwrap().as_str();
        debug!("handle str");
        debug!("{}",string_literal);
        let substr_len = string_literal.len();
        debug!("{}",substr_len);
        return (make_token(TokenType::STRING, string_literal.to_owned(), line_number), substr_len);
    }
    panic!("Unclosed string literal on line {} starting at position {}\n\t{}", line_number, current, line);
}

fn handle_number(line: &str, current: usize, line_number: usize) -> (Token, usize) {
    let re = Regex::new(r"\d+(?:\.\d+)?(?:e[+-]?\d+)?").unwrap();
    let substr = &line[current..];
    debug!("Substr: {:?}", substr);
    if let Some(caps) = re.captures(substr) {
        let number_literal = caps.get(0).unwrap().as_str();
        let substr_len = number_literal.len();
        return (Token{token_type: TokenType::NUMBER, lexeme: number_literal.to_owned(), line_number}, substr_len);
    }
    panic!("Error pattern matching number on line {} starting at position {}\n\t{}", line_number, current, line);
}

fn handle_identifier(line: &str, current: usize, line_number: usize) -> (Token, usize) {
    let re = Regex::new(r"[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
    let substr = &line[current..];
    debug!("Substr: {:?}", substr);
    if let Some(caps) = re.captures(substr) {
        let identifier = caps.get(0).unwrap().as_str();
        let substr_len = identifier.len();
        let mut token_type = TokenType::IDENTIFIER;
        match identifier {
            "if" => token_type = TokenType::IF,
            "else" => token_type = TokenType::ELSE,
            "while" => token_type = TokenType::WHILE,
            "var" => token_type = TokenType::VAR,
            "print" => token_type = TokenType::PRINT,
            "unless" => token_type = TokenType::UNLESS,
            "fun" => token_type = TokenType::FUN,
            _ => (),
        }
        return (make_token(token_type, identifier.to_owned(), line_number), substr_len);
    }
    panic!("Error pattern matching identifier on line {} starting at position {}\n\t{}", line_number, current, line);
}

fn tokenize_line(line: &str, line_number: usize) -> Vec<Token> {
    let line_len = line.len();
    let mut tokens: Vec<Token> = Vec::new();
    let mut current: usize = 0;
    let line_chars: Vec<char> = line.chars().collect();
    while current < line_len {
        let c = line_chars[current];
        debug!("L{}.C{}: {}", line_number, current, c);
        match c {
            ';' => {
                tokens.push(Token { token_type: TokenType::SEMICOLON, lexeme: ";".to_owned(), line_number });
                current += 1;
            },
            '(' => {
                tokens.push(Token { token_type: TokenType::LEFT_PAREN, lexeme: "(".to_owned(), line_number });
                current += 1;
            },
            ')' => {
                tokens.push(Token { token_type: TokenType::RIGHT_PAREN, lexeme: ")".to_owned(), line_number });
                current += 1;
            },
            '{' => {
                tokens.push(Token { token_type: TokenType::LEFT_BRACE, lexeme: "{".to_owned(), line_number });
                current += 1;
            },
            '}' => {tokens.push(Token { token_type: TokenType::RIGHT_BRACE, lexeme: "}".to_owned(), line_number });
                current += 1;
            },
            ',' => {
                tokens.push(Token{token_type: TokenType::COMMA, lexeme: ",".to_owned(), line_number });
                current += 1;
            },
            '+' => {
                tokens.push(Token { token_type: TokenType::PLUS, lexeme: "+".to_owned(), line_number });
                current += 1;
            },
            '-' => {
                tokens.push(Token { token_type: TokenType::MINUS, lexeme: "-".to_owned(), line_number });
                current += 1;
            },
            '*' => {
                tokens.push(Token { token_type: TokenType::STAR, lexeme: "*".to_owned(), line_number });
                current += 1;
            },
            '/' => {
                tokens.push(Token { token_type: TokenType::SLASH, lexeme: "/".to_owned(), line_number });
                current += 1;
            },
            '=' => {
                if peek('=', line, current, line_len) {
                    tokens.push(Token { token_type: TokenType::EQUAL_EQUAL, lexeme: "==".to_owned(), line_number });
                    current += 2;
                } else {
                    tokens.push(Token{token_type: TokenType::EQUAL, lexeme: "=".to_owned(), line_number });
                    current += 1;
                }
            }
            '!' => {
                if peek('=', line, current, line_len) {
                    tokens.push(Token { token_type: TokenType::BANG_EQUAL, lexeme: "!=".to_owned(), line_number });
                    current += 2;
                } else {
                    tokens.push(Token{token_type: TokenType::BANG, lexeme: "!".to_owned(), line_number });
                    current += 1;
                }
            }
            '>' => {
                if peek('=', line, current, line_len) {
                    tokens.push(Token { token_type: TokenType::GREATER_EQUAL, lexeme: ">=".to_owned(), line_number });
                    current += 2;
                } else {
                    tokens.push(Token{token_type: TokenType::GREATER, lexeme: ">".to_owned(), line_number });
                    current += 1;
                }
            }
            '<' => {
                if peek('=', line, current, line_len) {
                    tokens.push(Token { token_type: TokenType::LESS_EQUAL, lexeme: "<=".to_owned(), line_number });
                    current += 2;
                } else {
                    tokens.push(Token{token_type: TokenType::LESS, lexeme: "<".to_owned(), line_number });
                    current += 1;
                }
            }
            '"' => {
                let (string_token, string_len) = handle_string(line, current, line_number);
                tokens.push(string_token);
                current += string_len;
            }
            c if c.is_digit(10) => {
                let (number_token, number_len) = handle_number(line, current, line_number);
                tokens.push(number_token);
                current += number_len;
            }
            c if c.is_alphabetic() || c == '_' => {
                let (identifier_token, identifier_len) = handle_identifier(line, current, line_number);
                tokens.push(identifier_token);
                current += identifier_len;
            }
            c if c.is_whitespace() => { current += 1; }
            _ => panic!("Unexpected character on line {}: {}", line_number, c)
        }
    }
    return tokens;
}

pub fn tokenize(code: String) -> Vec<Token> {
    print!("code contains:\n{}", code);
    let mut tokens: Vec<Token> = Vec::new();
    let mut lines = code.lines();
    let mut line_num : usize = 1;
    for line in &mut lines {
        tokens.append(&mut tokenize_line(line, line_num));
        line_num += 1;
    }

    return tokens;
}