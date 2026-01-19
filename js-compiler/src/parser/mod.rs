use crate::lexer::Lexer;
use crate::lexer::token::Token;
use crate::ast::{Program, Statement, LetStatement, ReturnStatement, WhileStatement, ExpressionStatement, Identifier, IntegerLiteral, Expression, PrefixExpression, InfixExpression, IfExpression, BlockStatement, FunctionLiteral, CallExpression, AssignmentExpression, ArrayLiteral, HashLiteral, StringLiteral, ForStatement, MemberExpression, BooleanLiteral};

// Pratt Parser Precedence
#[derive(PartialEq, PartialOrd)]
enum Precedence {
    Lowest,
    Assign,      // =
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
    Index,       // array[index] or obj.prop
}

impl Precedence {
    fn from_token(token: &Token) -> Precedence {
        match token {
            Token::Equal => Precedence::Assign,
            Token::Eq | Token::NotEq => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::LParen => Precedence::Call,
            Token::Dot | Token::LBracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

type PrefixParseFn = fn(&mut Parser) -> Option<Expression>;
type InfixParseFn = fn(&mut Parser, Expression) -> Option<Expression>;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let cur_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Self {
            lexer,
            cur_token,
            peek_token,
            errors: vec![],
        }
    }
    
    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };

        while self.cur_token != Token::EOF {
            if let Some(stmt) = self.parse_statement() {
                program.statements.push(stmt);
            }
            self.next_token();
        }

        program
    }
    
    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            Token::While => self.parse_while_statement(),
            Token::For => self.parse_for_statement(),
            _ => self.parse_expression_statement(),
        }
    }
    
    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone(); // Token::Let

        if !self.expect_peek_condition(matches!(self.peek_token, Token::Identifier(_)), self.peek_token.clone()) {
            return None;
        }

        let name = match &self.cur_token {
            Token::Identifier(ident) => Identifier {
                token: self.cur_token.clone(),
                value: ident.clone(),
            },
            _ => return None,
        };

        if !self.expect_peek(Token::Equal) {
            return None;
        }

        self.next_token();

        let value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::SemiColon {
            self.next_token();
        }

        Some(Statement::Let(LetStatement {
            token,
            name,
            value,
        }))
    }
    
    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone(); // Token::Return

        self.next_token();

        let return_value = self.parse_expression(Precedence::Lowest)?;

        if self.peek_token == Token::SemiColon {
            self.next_token();
        }

        Some(Statement::Return(ReturnStatement {
            token,
            return_value,
        }))
    }

    fn parse_while_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        
        if !self.expect_peek(Token::LParen) {
            return None;
        }
        
        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;
        
        if !self.expect_peek(Token::RParen) {
            return None;
        }
        
        if !self.expect_peek(Token::LBrace) {
            return None;
        }
        
        let body = self.parse_block_statement();
        
        Some(Statement::While(WhileStatement {
            token,
            condition,
            body,
        }))
    }

    fn parse_for_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        
        if !self.expect_peek(Token::LParen) {
            return None;
        }
        
        self.next_token();
        
        let init;
        if self.cur_token_is(Token::SemiColon) {
            init = None;
        } else {
            if self.cur_token_is(Token::Let) {
                init = self.parse_let_statement().map(Box::new);
            } else {
                init = self.parse_expression_statement().map(Box::new);
            }
        }
        
        if self.cur_token_is(Token::SemiColon) {
            self.next_token();
        }
        
        let mut condition = None;
        if !self.cur_token_is(Token::SemiColon) {
            condition = self.parse_expression(Precedence::Lowest);
        }
        
        if !self.expect_peek(Token::SemiColon) {
            return None;
        }
        self.next_token(); // move past ';'
        
        let mut update = None;
        if !self.cur_token_is(Token::RParen) {
            let exp = self.parse_expression(Precedence::Lowest)?;
            update = Some(Box::new(Statement::Expression(ExpressionStatement {
                token: token.clone(),
                expression: exp,
            })));
        }
        
        if !self.expect_peek(Token::RParen) {
            return None;
        }
        
        if !self.expect_peek(Token::LBrace) {
            return None;
        }
        
        let body = self.parse_block_statement();
        
        Some(Statement::For(ForStatement {
            token,
            init,
            condition,
            update,
            body,
        }))
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        
        let expression = self.parse_expression(Precedence::Lowest)?;
        
        if self.peek_token == Token::SemiColon {
            self.next_token();
        }
        
        Some(Statement::Expression(ExpressionStatement {
            token,
            expression,
        }))
    }
    
    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let prefix = self.get_prefix_parse_fn(&self.cur_token);
        
        let mut left_exp = match prefix {
            Some(f) => f(self),
            None => {
                // self.no_prefix_parse_fn_error(self.cur_token.clone());
                None
            }
        }?;
        
        while self.peek_token != Token::SemiColon && precedence < self.peek_precedence() {
            let infix = match self.get_infix_parse_fn(&self.peek_token) {
                Some(f) => f,
                None => return Some(left_exp),
            };
            
            self.next_token();
            
            left_exp = infix(self, left_exp)?;
        }
        
        Some(left_exp)
    }
    
    fn get_prefix_parse_fn(&self, token: &Token) -> Option<PrefixParseFn> {
        match token {
            Token::Identifier(_) => Some(Self::parse_identifier),
            Token::Number(_) => Some(Self::parse_number),
            Token::String(_) => Some(Self::parse_string_literal),
            Token::True | Token::False => Some(Self::parse_boolean),
            Token::Bang | Token::Minus => Some(Self::parse_prefix_expression),
            Token::LParen => Some(Self::parse_grouped_expression),
            Token::If => Some(Self::parse_if_expression),
            Token::Function => Some(Self::parse_function_literal),
            Token::LBracket => Some(Self::parse_array_literal),
            Token::LBrace => Some(Self::parse_hash_literal),
            _ => None,
        }
    }
    
    fn get_infix_parse_fn(&self, token: &Token) -> Option<InfixParseFn> {
        match token {
            Token::Plus | Token::Minus | Token::Slash | Token::Asterisk | 
            Token::Eq | Token::NotEq | Token::Lt | Token::Gt => Some(Self::parse_infix_expression),
            Token::LParen => Some(Self::parse_call_expression),
            Token::Dot => Some(Self::parse_member_expression),
            Token::Equal => Some(Self::parse_assignment_expression),
            Token::LBracket => Some(Self::parse_index_expression),
            _ => None,
        }
    }
    
    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.cur_token.clone();
        
        self.next_token();
        let index = self.parse_expression(Precedence::Lowest)?;
        
        if !self.expect_peek(Token::RBracket) {
            return None;
        }
        
        Some(Expression::Index(Box::new(crate::ast::IndexExpression {
            token,
            left,
            index,
        })))
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        match &self.cur_token {
            Token::Identifier(s) => Some(Expression::Identifier(Identifier {
                token: self.cur_token.clone(),
                value: s.clone(),
            })),
            _ => None,
        }
    }
    
    fn parse_number(&mut self) -> Option<Expression> {
        match &self.cur_token {
            Token::Number(val) => Some(Expression::IntegerLiteral(IntegerLiteral {
                token: self.cur_token.clone(),
                value: *val,
            })),
            _ => None,
        }
    }

    fn parse_string_literal(&mut self) -> Option<Expression> {
        match &self.cur_token {
            Token::String(val) => Some(Expression::StringLiteral(StringLiteral {
                token: self.cur_token.clone(),
                value: val.clone(),
            })),
            _ => None,
        }
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        Some(Expression::Boolean(BooleanLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token_is(Token::True),
        }))
    }
    
    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let operator = token.literal();
        
        self.next_token();
        
        let right = self.parse_expression(Precedence::Prefix)?;
        
        Some(Expression::Prefix(Box::new(PrefixExpression {
            token,
            operator,
            right,
        })))
    }
    
    fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.next_token();
        
        let exp = self.parse_expression(Precedence::Lowest);
        
        if !self.expect_peek(Token::RParen) {
            return None;
        }
        
        exp
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.cur_token.clone();
        let operator = token.literal();
        
        let precedence = self.cur_precedence();
        self.next_token();
        let right = self.parse_expression(precedence)?;
        
        Some(Expression::Infix(Box::new(InfixExpression {
            token,
            left,
            operator,
            right,
        })))
    }
    
    fn parse_if_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        
        if !self.expect_peek(Token::LParen) {
            return None;
        }
        
        self.next_token();
        let condition = self.parse_expression(Precedence::Lowest)?;
        
        if !self.expect_peek(Token::RParen) {
            return None;
        }
        
        if !self.expect_peek(Token::LBrace) {
            return None;
        }
        
        let consequence = self.parse_block_statement();
        
        let mut alternative = None;
        
        if self.peek_token == Token::Else {
            self.next_token();
            
            if !self.expect_peek(Token::LBrace) {
                return None;
            }
            
            alternative = Some(self.parse_block_statement());
        }
        
        Some(Expression::If(Box::new(IfExpression {
            token,
            condition,
            consequence,
            alternative,
        })))
    }
    
    fn parse_function_literal(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        
        if !self.expect_peek(Token::LParen) {
            return None;
        }
        
        let parameters = self.parse_function_parameters()?;
        
        if !self.expect_peek(Token::LBrace) {
            return None;
        }
        
        let body = self.parse_block_statement();
        
        Some(Expression::FunctionLiteral(Box::new(FunctionLiteral {
            token,
            parameters,
            body,
        })))
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let elements = self.parse_expression_list(Token::RBracket)?;
        
        Some(Expression::Array(Box::new(ArrayLiteral {
            token,
            elements,
        })))
    }

    fn parse_hash_literal(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let mut pairs = vec![];
        
        while self.peek_token != Token::RBrace && self.peek_token != Token::EOF {
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;
            
            if !self.expect_peek(Token::Colon) {
                return None;
            }
            
            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            
            pairs.push((key, value));
            
            if self.peek_token != Token::RBrace && !self.expect_peek(Token::Comma) {
                return None;
            }
        }
        
        if !self.expect_peek(Token::RBrace) {
            return None;
        }
        
        Some(Expression::Hash(Box::new(HashLiteral {
            token,
            pairs,
        })))
    }

    fn parse_expression_list(&mut self, end: Token) -> Option<Vec<Expression>> {
        let mut list = vec![];
        
        if self.peek_token == end {
            self.next_token();
            return Some(list);
        }
        
        self.next_token();
        list.push(self.parse_expression(Precedence::Lowest)?);
        
        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            list.push(self.parse_expression(Precedence::Lowest)?);
        }
        
        if !self.expect_peek(end) {
            return None;
        }
        
        Some(list)
    }
    
    fn parse_function_parameters(&mut self) -> Option<Vec<Identifier>> {
        let mut identifiers = vec![];
        
        if self.peek_token == Token::RParen {
            self.next_token();
            return Some(identifiers);
        }
        
        self.next_token();
        
        match &self.cur_token {
            Token::Identifier(ident) => {
                identifiers.push(Identifier {
                    token: self.cur_token.clone(),
                    value: ident.clone(),
                });
            },
            _ => return None,
        }
        
        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
             match &self.cur_token {
                Token::Identifier(ident) => {
                    identifiers.push(Identifier {
                        token: self.cur_token.clone(),
                        value: ident.clone(),
                    });
                },
                _ => return None,
            }
        }
        
        if !self.expect_peek(Token::RParen) {
            return None;
        }
        
        Some(identifiers)
    }
    
    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        let token = self.cur_token.clone();
        let arguments = self.parse_call_arguments()?;
        
        Some(Expression::Call(Box::new(CallExpression {
            token,
            function,
            arguments,
        })))
    }
    
    fn parse_call_arguments(&mut self) -> Option<Vec<Expression>> {
        let mut args = vec![];
        
        if self.peek_token == Token::RParen {
            self.next_token();
            return Some(args);
        }
        
        self.next_token();
        if let Some(arg) = self.parse_expression(Precedence::Lowest) {
            args.push(arg);
        } else {
            return None;
        }
        
        while self.peek_token == Token::Comma {
            self.next_token();
            self.next_token();
            
            if let Some(arg) = self.parse_expression(Precedence::Lowest) {
                args.push(arg);
            } else {
                return None;
            }
        }
        
        if !self.expect_peek(Token::RParen) {
            return None;
        }
        
        Some(args)
    }

    fn parse_member_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.cur_token.clone();
        
        self.next_token(); // consume '.'
        
        // The property must be an identifier
        let property = self.parse_identifier()?;
        
        Some(Expression::Member(Box::new(MemberExpression {
            token,
            object: left,
            property,
        })))
    }

    pub fn parse_assignment_expression(&mut self, left: Expression) -> Option<Expression> {
        let token = self.cur_token.clone();
        
        // Ensure left is an identifier
        // TODO: support member assignment
        let name = match left {
            Expression::Identifier(ident) => ident,
            _ => {
                // We could add an error here about invalid assignment target
                return None;
            }
        };
        
        self.next_token(); // consume '='
        
        let value = self.parse_expression(Precedence::Lowest)?;
        
        Some(Expression::Assign(Box::new(AssignmentExpression {
            token,
            name,
            value,
        })))
    }
    
    fn parse_block_statement(&mut self) -> BlockStatement {
        let token = self.cur_token.clone();
        let mut statements = vec![];
        
        self.next_token();
        
        while self.cur_token != Token::RBrace && self.cur_token != Token::EOF {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
            self.next_token();
        }
        
        BlockStatement {
            token,
            statements,
        }
    }
    
    fn peek_precedence(&self) -> Precedence {
        Precedence::from_token(&self.peek_token)
    }
    
    fn cur_precedence(&self) -> Precedence {
        Precedence::from_token(&self.cur_token)
    }
    
    fn cur_token_is(&self, t: Token) -> bool {
        match (&self.cur_token, &t) {
             (Token::Identifier(_), Token::Identifier(_)) => true,
             _ => self.cur_token == t,
        }
    }
    
    fn expect_peek(&mut self, t: Token) -> bool {
        if self.peek_token == t {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    fn expect_peek_condition(&mut self, condition: bool, t: Token) -> bool {
        if condition {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }
    
    fn peek_error(&mut self, t: Token) {
        let msg = format!("expected next token to be {:?}, got {:?} instead", t, self.peek_token);
        self.errors.push(msg);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::ast::{Statement, Expression};

    #[test]
    fn test_let_statements() {
        let input = "
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        
        if !parser.errors.is_empty() {
             for err in parser.errors {
                 eprintln!("parser error: {}", err);
             }
             panic!("parser has errors");
        }

        assert_eq!(program.statements.len(), 3);

        let tests = vec![
            "x",
            "y",
            "foobar",
        ];

        for (i, expected_ident) in tests.iter().enumerate() {
            let stmt = &program.statements[i];
            match stmt {
                Statement::Let(let_stmt) => {
                    assert_eq!(let_stmt.name.value, *expected_ident);
                    assert_eq!(let_stmt.token.literal(), "let");
                },
                _ => panic!("stmt not LetStatement. got={:?}", stmt),
            }
        }
    }

    #[test]
    fn test_return_statements() {
        let input = "
            return 5;
            return 10;
            return 993322;
        ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        
        if !parser.errors.is_empty() {
             for err in parser.errors {
                 eprintln!("parser error: {}", err);
             }
             panic!("parser has errors");
        }

        assert_eq!(program.statements.len(), 3);

        for stmt in program.statements {
            match stmt {
                Statement::Return(return_stmt) => {
                    assert_eq!(return_stmt.token.literal(), "return");
                },
                _ => panic!("stmt not ReturnStatement. got={:?}", stmt),
            }
        }
    }

    #[test]
    fn test_identifier_expression() {
        let input = "foobar;";
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        
        if !parser.errors.is_empty() {
             for err in parser.errors {
                 eprintln!("parser error: {}", err);
             }
             panic!("parser has errors");
        }

        assert_eq!(program.statements.len(), 1);
        
        match &program.statements[0] {
            Statement::Expression(stmt) => {
                match &stmt.expression {
                    Expression::Identifier(ident) => {
                         assert_eq!(ident.value, "foobar");
                         assert_eq!(ident.token.literal(), "foobar");
                    },
                    _ => panic!("exp not Identifier. got={:?}", stmt.expression),
                }
            },
            _ => panic!("stmt not ExpressionStatement. got={:?}", program.statements[0]),
        }
    }
    
    #[test]
    fn test_prefix_expressions() {
        struct PrefixTest {
            input: String,
            operator: String,
            value: String, 
        }
        
        let prefix_tests = vec![
            PrefixTest { input: "!foobar;".to_string(), operator: "!".to_string(), value: "foobar".to_string() },
            PrefixTest { input: "-foobar;".to_string(), operator: "-".to_string(), value: "foobar".to_string() },
        ];
        
        for tt in prefix_tests {
            let lexer = Lexer::new(&tt.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            
            if !parser.errors.is_empty() {
                 for err in parser.errors {
                     eprintln!("parser error: {}", err);
                 }
                 panic!("parser has errors");
            }
            
            assert_eq!(program.statements.len(), 1);
            
            match &program.statements[0] {
                Statement::Expression(stmt) => {
                    match &stmt.expression {
                        Expression::Prefix(exp) => {
                            assert_eq!(exp.operator, tt.operator);
                            match &exp.right {
                                Expression::Identifier(ident) => {
                                    assert_eq!(ident.value, tt.value);
                                },
                                _ => panic!("exp.right not Identifier. got={:?}", exp.right),
                            }
                        },
                        _ => panic!("stmt.expression not Prefix. got={:?}", stmt.expression),
                    }
                },
                _ => panic!("stmt not ExpressionStatement. got={:?}", program.statements[0]),
            }
        }
    }

    #[test]
    fn test_infix_expressions() {
        struct InfixTest {
            input: String,
            left_value: String,
            operator: String,
            right_value: String,
        }
        
        let infix_tests_identifiers = vec![
            InfixTest { input: "a + b;".to_string(), left_value: "a".to_string(), operator: "+".to_string(), right_value: "b".to_string() },
            InfixTest { input: "a - b;".to_string(), left_value: "a".to_string(), operator: "-".to_string(), right_value: "b".to_string() },
            InfixTest { input: "a * b;".to_string(), left_value: "a".to_string(), operator: "*".to_string(), right_value: "b".to_string() },
            InfixTest { input: "a / b;".to_string(), left_value: "a".to_string(), operator: "/".to_string(), right_value: "b".to_string() },
            InfixTest { input: "a > b;".to_string(), left_value: "a".to_string(), operator: ">".to_string(), right_value: "b".to_string() },
            InfixTest { input: "a < b;".to_string(), left_value: "a".to_string(), operator: "<".to_string(), right_value: "b".to_string() },
            InfixTest { input: "a == b;".to_string(), left_value: "a".to_string(), operator: "==".to_string(), right_value: "b".to_string() },
            InfixTest { input: "a != b;".to_string(), left_value: "a".to_string(), operator: "!=".to_string(), right_value: "b".to_string() },
        ];

        for tt in infix_tests_identifiers {
            let lexer = Lexer::new(&tt.input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            
            if !parser.errors.is_empty() {
                 for err in parser.errors {
                     eprintln!("parser error: {}", err);
                 }
                 panic!("parser has errors");
            }
            
            assert_eq!(program.statements.len(), 1);
            
            match &program.statements[0] {
                Statement::Expression(stmt) => {
                    match &stmt.expression {
                        Expression::Infix(exp) => {
                            assert_eq!(exp.operator, tt.operator);
                            match &exp.left {
                                Expression::Identifier(ident) => assert_eq!(ident.value, tt.left_value),
                                _ => panic!("exp.left not Identifier"),
                            }
                            match &exp.right {
                                Expression::Identifier(ident) => assert_eq!(ident.value, tt.right_value),
                                _ => panic!("exp.right not Identifier"),
                            }
                        },
                        _ => panic!("stmt.expression not Infix"),
                    }
                },
                _ => panic!("stmt not ExpressionStatement"),
            }
        }
    }
    
    #[test]
    fn test_if_expression() {
        let input = "if (x < y) { x }";
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        
        if !parser.errors.is_empty() {
             for err in parser.errors {
                 eprintln!("parser error: {}", err);
             }
             panic!("parser has errors");
        }

        println!("Statements: {:?}", program.statements);
        
        assert_eq!(program.statements.len(), 1);
        
        match &program.statements[0] {
            Statement::Expression(stmt) => {
                match &stmt.expression {
                    Expression::If(exp) => {
                        // Check condition
                        match &exp.condition {
                            Expression::Infix(infix) => {
                                assert_eq!(infix.operator, "<");
                                match &infix.left {
                                    Expression::Identifier(ident) => assert_eq!(ident.value, "x"),
                                    _ => panic!("condition.left not Identifier"),
                                }
                                match &infix.right {
                                    Expression::Identifier(ident) => assert_eq!(ident.value, "y"),
                                    _ => panic!("condition.right not Identifier"),
                                }
                            },
                            _ => panic!("condition not Infix"),
                        }
                        
                        // Check consequence
                        assert_eq!(exp.consequence.statements.len(), 1);
                        match &exp.consequence.statements[0] {
                            Statement::Expression(stmt) => {
                                match &stmt.expression {
                                    Expression::Identifier(ident) => assert_eq!(ident.value, "x"),
                                    _ => panic!("consequence stmt not Identifier"),
                                }
                            },
                            _ => panic!("consequence stmt not ExpressionStatement"),
                        }
                        
                        // Check alternative
                        assert!(exp.alternative.is_none());
                    },
                    _ => panic!("stmt.expression not If"),
                }
            },
            _ => panic!("stmt not ExpressionStatement"),
        }
    }
    
    #[test]
    fn test_if_else_expression() {
        let input = "if (x < y) { x } else { y }";
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        
        if !parser.errors.is_empty() {
             for err in parser.errors {
                 eprintln!("parser error: {}", err);
             }
             panic!("parser has errors");
        }
        
        if program.statements.len() != 1 {
            panic!("Expected 1 statement, got {}: {:?}", program.statements.len(), program.statements);
        }
        
        match &program.statements[0] {
            Statement::Expression(stmt) => {
                match &stmt.expression {
                    Expression::If(exp) => {
                        // Check condition (same as above)
                        
                        // Check alternative
                        assert!(exp.alternative.is_some());
                        let alt = exp.alternative.as_ref().unwrap();
                        assert_eq!(alt.statements.len(), 1);
                         match &alt.statements[0] {
                            Statement::Expression(stmt) => {
                                match &stmt.expression {
                                    Expression::Identifier(ident) => assert_eq!(ident.value, "y"),
                                    _ => panic!("alternative stmt not Identifier"),
                                }
                            },
                            _ => panic!("alternative stmt not ExpressionStatement"),
                        }
                    },
                    _ => panic!("stmt.expression not If"),
                }
            },
            _ => panic!("stmt not ExpressionStatement"),
        }
    }

    #[test]
    fn test_function_literal_parsing() {
        let input = "function(x, y) { x + y; }";
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        
        if !parser.errors.is_empty() {
             for err in parser.errors {
                 eprintln!("parser error: {}", err);
             }
             panic!("parser has errors");
        }
        
        assert_eq!(program.statements.len(), 1);
        
        match &program.statements[0] {
            Statement::Expression(stmt) => {
                match &stmt.expression {
                    Expression::FunctionLiteral(func) => {
                        assert_eq!(func.parameters.len(), 2);
                        assert_eq!(func.parameters[0].value, "x");
                        assert_eq!(func.parameters[1].value, "y");
                        
                        assert_eq!(func.body.statements.len(), 1);
                        
                         match &func.body.statements[0] {
                             Statement::Expression(body_stmt) => {
                                 match &body_stmt.expression {
                                     Expression::Infix(infix) => {
                                         assert_eq!(infix.operator, "+");
                                     },
                                     _ => panic!("function body stmt not Infix"),
                                 }
                             },
                             _ => panic!("function body stmt not ExpressionStatement"),
                         }
                    },
                    _ => panic!("stmt.expression not FunctionLiteral"),
                }
            },
            _ => panic!("stmt not ExpressionStatement"),
        }
    }

    #[test]
    fn test_call_expression_parsing() {
        let input = "add(1, 2 * 3, 4 + 5);";
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        
        if !parser.errors.is_empty() {
             for err in parser.errors {
                 eprintln!("parser error: {}", err);
             }
             panic!("parser has errors");
        }
        
        assert_eq!(program.statements.len(), 1);
        
        match &program.statements[0] {
            Statement::Expression(stmt) => {
                match &stmt.expression {
                    Expression::Call(call) => {
                        match &call.function {
                            Expression::Identifier(ident) => {
                                assert_eq!(ident.value, "add");
                            },
                            _ => panic!("call.function not Identifier"),
                        }
                        
                        assert_eq!(call.arguments.len(), 3);
                        
                        match &call.arguments[0] {
                            Expression::IntegerLiteral(int) => assert_eq!(int.value, 1.0),
                            _ => panic!("arg[0] not IntegerLiteral"),
                        }
                        
                        match &call.arguments[1] {
                            Expression::Infix(infix) => {
                                assert_eq!(infix.operator, "*");
                                match &infix.left {
                                    Expression::IntegerLiteral(int) => assert_eq!(int.value, 2.0),
                                    _ => panic!("infix.left not IntegerLiteral"),
                                }
                                match &infix.right {
                                    Expression::IntegerLiteral(int) => assert_eq!(int.value, 3.0),
                                    _ => panic!("infix.right not IntegerLiteral"),
                                }
                            },
                            _ => panic!("arg[1] not Infix"),
                        }
                        
                        match &call.arguments[2] {
                            Expression::Infix(infix) => {
                                assert_eq!(infix.operator, "+");
                                match &infix.left {
                                    Expression::IntegerLiteral(int) => assert_eq!(int.value, 4.0),
                                    _ => panic!("infix.left not IntegerLiteral"),
                                }
                                match &infix.right {
                                    Expression::IntegerLiteral(int) => assert_eq!(int.value, 5.0),
                                    _ => panic!("infix.right not IntegerLiteral"),
                                }
                            },
                            _ => panic!("arg[2] not Infix"),
                        }
                    },
                    _ => panic!("stmt.expression not Call"),
                }
            },
            _ => panic!("stmt not ExpressionStatement"),
        }
    }

    #[test]
    fn test_while_statement_parsing() {
        let input = "while (x < 10) { x = x + 1; }";
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        
        if !parser.errors.is_empty() {
             for err in parser.errors {
                 eprintln!("parser error: {}", err);
             }
             panic!("parser has errors");
        }
        
        if program.statements.len() != 1 {
            panic!("Expected 1 statement, got {}: {:?}", program.statements.len(), program.statements);
        }        
        match &program.statements[0] {
            Statement::While(while_stmt) => {
                // Check condition
                match &while_stmt.condition {
                    Expression::Infix(infix) => {
                        assert_eq!(infix.operator, "<");
                        match &infix.left {
                            Expression::Identifier(ident) => assert_eq!(ident.value, "x"),
                            _ => panic!("condition.left not Identifier"),
                        }
                        match &infix.right {
                            Expression::IntegerLiteral(int) => assert_eq!(int.value, 10.0),
                            _ => panic!("condition.right not IntegerLiteral"),
                        }
                    },
                    _ => panic!("condition not Infix"),
                }
                
                // Check body
                assert_eq!(while_stmt.body.statements.len(), 1);
                
                // Check body statement (Assignment)
                match &while_stmt.body.statements[0] {
                    Statement::Expression(stmt) => {
                        match &stmt.expression {
                            Expression::Assign(assign) => {
                                assert_eq!(assign.name.value, "x");
                                match &assign.value {
                                    Expression::Infix(infix) => {
                                        assert_eq!(infix.operator, "+");
                                        // Check left x
                                        match &infix.left {
                                             Expression::Identifier(ident) => assert_eq!(ident.value, "x"),
                                             _ => panic!("infix.left not Identifier"),
                                        }
                                        // Check right 1
                                        match &infix.right {
                                             Expression::IntegerLiteral(int) => assert_eq!(int.value, 1.0),
                                             _ => panic!("infix.right not IntegerLiteral"),
                                        }
                                    },
                                    _ => panic!("assign.value not Infix"),
                                }
                            },
                            _ => panic!("body stmt expression not Assign"),
                        }
                    },
                    _ => panic!("body stmt not ExpressionStatement"),
                }
            },
            _ => panic!("stmt not WhileStatement"),
        }
    }

    #[test]
    fn test_assignment_expression() {
        let input = "x = 5;";
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        
        if !parser.errors.is_empty() {
             for err in parser.errors {
                 eprintln!("parser error: {}", err);
             }
             panic!("parser has errors");
        }
        
        assert_eq!(program.statements.len(), 1);
        
        match &program.statements[0] {
            Statement::Expression(stmt) => {
                match &stmt.expression {
                    Expression::Assign(assign) => {
                        assert_eq!(assign.name.value, "x");
                        match &assign.value {
                            Expression::IntegerLiteral(i) => assert_eq!(i.value, 5.0),
                            _ => panic!("value not IntegerLiteral"),
                        }
                    },
                    _ => panic!("stmt.expression not Assign"),
                }
            },
            _ => panic!("stmt not ExpressionStatement"),
        }
    }

    #[test]
    fn test_array_literal_parsing() {
        let input = "[1, 2 * 2, 3 + 3]";
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        
        if !parser.errors.is_empty() {
             for err in parser.errors {
                 eprintln!("parser error: {}", err);
             }
             panic!("parser has errors");
        }
        
        assert_eq!(program.statements.len(), 1);
        
        match &program.statements[0] {
            Statement::Expression(stmt) => {
                match &stmt.expression {
                    Expression::Array(array) => {
                        assert_eq!(array.elements.len(), 3);
                        
                        match &array.elements[0] {
                            Expression::IntegerLiteral(i) => assert_eq!(i.value, 1.0),
                            _ => panic!("element[0] not IntegerLiteral"),
                        }
                        
                        match &array.elements[1] {
                            Expression::Infix(infix) => {
                                assert_eq!(infix.operator, "*");
                                match &infix.left {
                                    Expression::IntegerLiteral(i) => assert_eq!(i.value, 2.0),
                                    _ => panic!("infix.left not IntegerLiteral"),
                                }
                                match &infix.right {
                                    Expression::IntegerLiteral(i) => assert_eq!(i.value, 2.0),
                                    _ => panic!("infix.right not IntegerLiteral"),
                                }
                            },
                            _ => panic!("element[1] not Infix"),
                        }
                        
                        match &array.elements[2] {
                            Expression::Infix(infix) => {
                                assert_eq!(infix.operator, "+");
                                match &infix.left {
                                    Expression::IntegerLiteral(i) => assert_eq!(i.value, 3.0),
                                    _ => panic!("infix.left not IntegerLiteral"),
                                }
                                match &infix.right {
                                    Expression::IntegerLiteral(i) => assert_eq!(i.value, 3.0),
                                    _ => panic!("infix.right not IntegerLiteral"),
                                }
                            },
                            _ => panic!("element[2] not Infix"),
                        }
                    },
                    _ => panic!("stmt.expression not Array"),
                }
            },
            _ => panic!("stmt not ExpressionStatement"),
        }
    }

    #[test]
    fn test_hash_literal_parsing() {
        let input = "{ \"one\": 1, \"two\": 2, \"three\": 3 }";
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        
        if !parser.errors.is_empty() {
             for err in parser.errors {
                 eprintln!("parser error: {}", err);
             }
             panic!("parser has errors");
        }
        
        assert_eq!(program.statements.len(), 1);
        
        match &program.statements[0] {
            Statement::Expression(stmt) => {
                match &stmt.expression {
                    Expression::Hash(hash) => {
                        assert_eq!(hash.pairs.len(), 3);
                        
                        let expected = vec![
                            ("one", 1.0),
                            ("two", 2.0),
                            ("three", 3.0),
                        ];
                        
                        for (i, (key_str, val_num)) in expected.iter().enumerate() {
                            let (key, value) = &hash.pairs[i];
                            
                            match key {
                                Expression::StringLiteral(s) => assert_eq!(s.value, *key_str),
                                _ => panic!("key is not StringLiteral"),
                            }
                            
                            match value {
                                Expression::IntegerLiteral(int) => assert_eq!(int.value, *val_num),
                                _ => panic!("value is not IntegerLiteral"),
                            }
                        }
                    },
                    _ => panic!("stmt.expression not Hash"),
                }
            },
            _ => panic!("stmt not ExpressionStatement"),
        }
    }

    #[test]
    fn test_for_loop_parsing() {
        let input = "for (let i = 0; i < 10; i = i + 1) { x; }";
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        
        if !parser.errors.is_empty() {
             for err in parser.errors {
                 eprintln!("parser error: {}", err);
             }
             panic!("parser has errors");
        }
        
        assert_eq!(program.statements.len(), 1);
        
        match &program.statements[0] {
            Statement::For(for_stmt) => {
                // Check init
                assert!(for_stmt.init.is_some());
                
                // Check condition
                match &for_stmt.condition {
                    Some(Expression::Infix(infix)) => {
                        assert_eq!(infix.operator, "<");
                    },
                    _ => panic!("condition not Infix"),
                }
                
                // Check update
                assert!(for_stmt.update.is_some());
                match for_stmt.update.as_ref().unwrap().as_ref() {
                    Statement::Expression(stmt) => {
                        match &stmt.expression {
                            Expression::Assign(assign) => {
                                assert_eq!(assign.name.value, "i");
                            },
                            _ => panic!("update expression not Assign"),
                        }
                    },
                    _ => panic!("update stmt not ExpressionStatement"),
                }
                
                // Check body
                assert_eq!(for_stmt.body.statements.len(), 1);
            },
            _ => panic!("stmt not ForStatement"),
        }
    }

    #[test]
    fn test_member_expression() {
        let input = "obj.prop;";
        
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        
        if !parser.errors.is_empty() {
             for err in parser.errors {
                 eprintln!("parser error: {}", err);
             }
             panic!("parser has errors");
        }
        
        assert_eq!(program.statements.len(), 1);
        
        match &program.statements[0] {
            Statement::Expression(stmt) => {
                match &stmt.expression {
                    Expression::Member(member) => {
                        match &member.object {
                            Expression::Identifier(ident) => assert_eq!(ident.value, "obj"),
                            _ => panic!("object not Identifier"),
                        }
                        
                        match &member.property {
                            Expression::Identifier(ident) => assert_eq!(ident.value, "prop"),
                            _ => panic!("property not Identifier"),
                        }
                    },
                    _ => panic!("expression not Member"),
                }
            },
            _ => panic!("stmt not ExpressionStatement"),
        }
    }
}
