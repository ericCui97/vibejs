use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Token {
    // Keywords
    Let,
    Const,
    Function,
    Return,
    If,
    Else,
    While,
    For,
    True,
    False,
    
    // Identifiers & Literals
    Identifier(String),
    Number(f64),
    String(String),
    
    // Operators
    Equal,      // =
    Plus,       // +
    Minus,      // -
    Asterisk,   // *
    Slash,      // /
    Bang,       // !
    PlusPlus,   // ++
    
    // Comparison
    Eq,         // ==
    NotEq,      // !=
    Lt,         // <
    Gt,         // >
    
    // Delimiters
    SemiColon,
    Comma,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Colon,
    
    EOF,
    Illegal,
}
