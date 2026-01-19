use crate::lexer::token::Token;
use serde::Serialize;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
#[serde(tag = "type")]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    While(WhileStatement),
    For(ForStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ForStatement {
    pub token: Token, // Token::For
    pub init: Option<Box<Statement>>,
    pub condition: Option<Expression>,
    pub update: Option<Box<Statement>>,
    pub body: BlockStatement,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct WhileStatement {
    pub token: Token, // Token::While
    pub condition: Expression,
    pub body: BlockStatement,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct LetStatement {
    pub token: Token, // Token::Let
    pub name: Identifier,
    pub value: Expression,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ReturnStatement {
    pub token: Token, // Token::Return
    pub return_value: Expression,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ExpressionStatement {
    pub token: Token, // The first token of the expression
    pub expression: Expression,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct BlockStatement {
    pub token: Token, // {
    pub statements: Vec<Statement>,
}

use std::fmt;

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Identifier {
    pub token: Token, // Token::Identifier
    pub value: String,
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct IntegerLiteral {
    pub token: Token, // Token::Number
    pub value: f64,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct StringLiteral {
    pub token: Token, // Token::String
    pub value: String,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct BooleanLiteral {
    pub token: Token, // Token::True or Token::False
    pub value: bool,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct PrefixExpression {
    pub token: Token, // The prefix token, e.g. !
    pub operator: String,
    pub right: Expression,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct InfixExpression {
    pub token: Token, // The operator token, e.g. +
    pub left: Expression,
    pub operator: String,
    pub right: Expression,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct IfExpression {
    pub token: Token, // 'if'
    pub condition: Expression,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct FunctionLiteral {
    pub token: Token, // 'function'
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct CallExpression {
    pub token: Token, // The '(' token
    pub function: Expression, // Identifier or FunctionLiteral
    pub arguments: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
#[serde(tag = "type")]
pub enum Expression {
    // Placeholder for now
    Empty,
    Identifier(Identifier),
    IntegerLiteral(IntegerLiteral),
    StringLiteral(StringLiteral),
    Boolean(BooleanLiteral),
    Prefix(Box<PrefixExpression>),
    Infix(Box<InfixExpression>),
    If(Box<IfExpression>),
    FunctionLiteral(Box<FunctionLiteral>),
    Call(Box<CallExpression>),
    Assign(Box<AssignmentExpression>),
    Array(Box<ArrayLiteral>),
    Hash(Box<HashLiteral>),
    Member(Box<MemberExpression>),
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct MemberExpression {
    pub token: Token, // The '.' token
    pub object: Expression,
    pub property: Expression,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct HashLiteral {
    pub token: Token, // '{'
    pub pairs: Vec<(Expression, Expression)>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct ArrayLiteral {
    pub token: Token, // '['
    pub elements: Vec<Expression>,
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct AssignmentExpression {
    pub token: Token, // Token::Equal
    pub name: Identifier,
    pub value: Expression,
}

impl Token {
    pub fn literal(&self) -> String {
        match self {
            Token::Let => "let".to_string(),
            Token::Const => "const".to_string(),
            Token::Return => "return".to_string(),
            Token::If => "if".to_string(),
            Token::Else => "else".to_string(),
            Token::While => "while".to_string(),
            Token::For => "for".to_string(),
            Token::Function => "function".to_string(),
            Token::Identifier(s) => s.clone(),
            Token::Number(n) => n.to_string(),
            Token::String(s) => s.clone(),
            Token::Equal => "=".to_string(),
            Token::SemiColon => ";".to_string(),
            Token::Comma => ",".to_string(),
            Token::Bang => "!".to_string(),
            Token::Minus => "-".to_string(),
            Token::Plus => "+".to_string(),
            Token::Asterisk => "*".to_string(),
            Token::Slash => "/".to_string(),
            Token::Lt => "<".to_string(),
            Token::Gt => ">".to_string(),
            Token::Eq => "==".to_string(),
            Token::NotEq => "!=".to_string(),
            Token::LParen => "(".to_string(),
            Token::RParen => ")".to_string(),
            Token::LBrace => "{".to_string(),
            Token::RBrace => "}".to_string(),
            Token::LBracket => "[".to_string(),
            Token::RBracket => "]".to_string(),
            Token::Colon => ":".to_string(),
            Token::Dot => ".".to_string(),
            // Add other token literals as needed
            _ => "".to_string(),
        }
    }
}
