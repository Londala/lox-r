use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenType {
    UNKNOWN,
    SEMICOLON,
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    COMMA,
    PLUS,
    MINUS,
    STAR,
    SLASH,

    // Tokens requiring 1-character lookahead
    EQUAL,
    EQUAL_EQUAL,
    BANG,
    BANG_EQUAL,
    LESS,
    LESS_EQUAL,
    GREATER,
    GREATER_EQUAL,

    // Literal values
    NUMBER,
    STRING,

    // identifier
    IDENTIFIER,

    // keywords
    IF,
    ELSE,
    WHILE,
    VAR,
    PRINT,
    UNLESS,
    FUN
}
impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}