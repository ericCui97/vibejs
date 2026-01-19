pub mod token;

use token::Token;

pub struct Lexer {
    input: Vec<char>,
    position: usize,
    read_position: usize,
    ch: char,
}

#[cfg(test)]
mod tests_hash {
    use super::*;
    use crate::lexer::token::Token;
    
    #[test]
    fn test_hash_literal_lexing() {
        let input = "{ foo: 1, bar: 2 };";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::LBrace);
        assert_eq!(lexer.next_token(), Token::Identifier("foo".to_string()));
        assert_eq!(lexer.next_token(), Token::Colon);
        assert_eq!(lexer.next_token(), Token::Number(1.0));
        assert_eq!(lexer.next_token(), Token::Comma);
        assert_eq!(lexer.next_token(), Token::Identifier("bar".to_string()));
        assert_eq!(lexer.next_token(), Token::Colon);
        assert_eq!(lexer.next_token(), Token::Number(2.0));
        assert_eq!(lexer.next_token(), Token::RBrace);
        assert_eq!(lexer.next_token(), Token::SemiColon);
        assert_eq!(lexer.next_token(), Token::EOF);
    }
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        let mut l = Self {
            input: input.chars().collect(),
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        l.read_char();
        l
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }
    
    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position]
        }
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::Eq
                } else {
                    Token::Equal
                }
            }
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            '+' => Token::Plus,
            '-' => Token::Minus,
            '*' => Token::Asterisk,
            '/' => Token::Slash,
            '<' => Token::Lt,
            '>' => Token::Gt,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '{' => Token::LBrace,
            '}' => Token::RBrace,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            ':' => Token::Colon,
            ';' => Token::SemiColon,
            ',' => Token::Comma,
            '"' | '\'' => {
                 let quote = self.ch;
                 let str_val = self.read_string(quote);
                 return str_val; // read_string already advances, so return early
            }
            '\0' => Token::EOF,
            _ => {
                if self.ch.is_alphabetic() {
                    return self.read_identifier();
                } else if self.ch.is_numeric() {
                    return self.read_number();
                } else {
                    Token::Illegal
                }
            }
        };

        self.read_char();
        token
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }

    fn read_identifier(&mut self) -> Token {
        let start_pos = self.position;
        while self.ch.is_alphanumeric() {
            self.read_char();
        }
        
        let ident: String = self.input[start_pos..self.position].iter().collect();
        match ident.as_str() {
            "let" => Token::Let,
            "const" => Token::Const,
            "function" => Token::Function,
            "return" => Token::Return,
            "if" => Token::If,
            "else" => Token::Else,
            "while" => Token::While,
            "true" => Token::True,
            "false" => Token::False,
            _ => Token::Identifier(ident),
        }
    }

    fn read_number(&mut self) -> Token {
        let start_pos = self.position;
        while self.ch.is_numeric() || self.ch == '.' {
            self.read_char();
        }
        
        let num_str: String = self.input[start_pos..self.position].iter().collect();
        let value = num_str.parse::<f64>().unwrap_or(0.0);
        Token::Number(value)
    }

    fn read_string(&mut self, quote: char) -> Token {
        self.read_char(); // Skip opening quote
        let start_pos = self.position;
        
        while self.ch != quote && self.ch != '\0' {
            self.read_char();
        }
        
        if self.ch == '\0' {
             return Token::Illegal; // Unterminated string
        }
        
        let str_value: String = self.input[start_pos..self.position].iter().collect();
        self.read_char(); // Skip closing quote (will be advanced again by next_token caller logic if not handled carefully, but here we return directly)
        
        Token::String(str_value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use token::Token;

    #[test]
    fn test_simple_assignment() {
        let input = "let x = 10;";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::Let);
        assert_eq!(lexer.next_token(), Token::Identifier("x".to_string()));
        assert_eq!(lexer.next_token(), Token::Equal);
        assert_eq!(lexer.next_token(), Token::Number(10.0));
        assert_eq!(lexer.next_token(), Token::SemiColon);
        assert_eq!(lexer.next_token(), Token::EOF);
    }

    #[test]
    fn test_operators_and_strings() {
        let input = "let s = \"hello\"; const y = (1 + 2) * 3;";
        let mut lexer = Lexer::new(input);

        // let s = "hello";
        assert_eq!(lexer.next_token(), Token::Let);
        assert_eq!(lexer.next_token(), Token::Identifier("s".to_string()));
        assert_eq!(lexer.next_token(), Token::Equal);
        assert_eq!(lexer.next_token(), Token::String("hello".to_string()));
        assert_eq!(lexer.next_token(), Token::SemiColon);

        // const y = (1 + 2) * 3;
        assert_eq!(lexer.next_token(), Token::Const);
        assert_eq!(lexer.next_token(), Token::Identifier("y".to_string()));
        assert_eq!(lexer.next_token(), Token::Equal);
        assert_eq!(lexer.next_token(), Token::LParen);
        assert_eq!(lexer.next_token(), Token::Number(1.0));
        assert_eq!(lexer.next_token(), Token::Plus);
        assert_eq!(lexer.next_token(), Token::Number(2.0));
        assert_eq!(lexer.next_token(), Token::RParen);
        assert_eq!(lexer.next_token(), Token::Asterisk);
        assert_eq!(lexer.next_token(), Token::Number(3.0));
        assert_eq!(lexer.next_token(), Token::SemiColon);
        assert_eq!(lexer.next_token(), Token::EOF);
    }
    
    #[test]
    fn test_function_and_comparison() {
        let input = "
            function add(a, b) {
                if (a == b) {
                    return true;
                } else {
                    return a + b;
                }
            }
        ";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::Function);
        assert_eq!(lexer.next_token(), Token::Identifier("add".to_string()));
        assert_eq!(lexer.next_token(), Token::LParen);
        assert_eq!(lexer.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(lexer.next_token(), Token::Comma);
        assert_eq!(lexer.next_token(), Token::Identifier("b".to_string()));
        assert_eq!(lexer.next_token(), Token::RParen);
        assert_eq!(lexer.next_token(), Token::LBrace);
        
        assert_eq!(lexer.next_token(), Token::If);
        assert_eq!(lexer.next_token(), Token::LParen);
        assert_eq!(lexer.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(lexer.next_token(), Token::Eq);
        assert_eq!(lexer.next_token(), Token::Identifier("b".to_string()));
        assert_eq!(lexer.next_token(), Token::RParen);
        assert_eq!(lexer.next_token(), Token::LBrace);
        
        assert_eq!(lexer.next_token(), Token::Return);
        assert_eq!(lexer.next_token(), Token::True);
        assert_eq!(lexer.next_token(), Token::SemiColon);
        
        assert_eq!(lexer.next_token(), Token::RBrace);
        assert_eq!(lexer.next_token(), Token::Else);
        assert_eq!(lexer.next_token(), Token::LBrace);
        
        assert_eq!(lexer.next_token(), Token::Return);
        assert_eq!(lexer.next_token(), Token::Identifier("a".to_string()));
        assert_eq!(lexer.next_token(), Token::Plus);
        assert_eq!(lexer.next_token(), Token::Identifier("b".to_string()));
        assert_eq!(lexer.next_token(), Token::SemiColon);
        
        assert_eq!(lexer.next_token(), Token::RBrace);
        assert_eq!(lexer.next_token(), Token::RBrace);
        assert_eq!(lexer.next_token(), Token::EOF);
    }

    #[test]
    fn test_while_loop_lexing() {
        let input = "while (x < 10) { x = x + 1; }";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::While);
        assert_eq!(lexer.next_token(), Token::LParen);
        assert_eq!(lexer.next_token(), Token::Identifier("x".to_string()));
        assert_eq!(lexer.next_token(), Token::Lt);
        assert_eq!(lexer.next_token(), Token::Number(10.0));
        assert_eq!(lexer.next_token(), Token::RParen);
        assert_eq!(lexer.next_token(), Token::LBrace);
        
        assert_eq!(lexer.next_token(), Token::Identifier("x".to_string()));
        assert_eq!(lexer.next_token(), Token::Equal);
        assert_eq!(lexer.next_token(), Token::Identifier("x".to_string()));
        assert_eq!(lexer.next_token(), Token::Plus);
        assert_eq!(lexer.next_token(), Token::Number(1.0));
        assert_eq!(lexer.next_token(), Token::SemiColon);
        
        assert_eq!(lexer.next_token(), Token::RBrace);
        assert_eq!(lexer.next_token(), Token::EOF);
    }

    #[test]
    fn test_array_literal_lexing() {
        let input = "[1, 2];";
        let mut lexer = Lexer::new(input);

        assert_eq!(lexer.next_token(), Token::LBracket);
        assert_eq!(lexer.next_token(), Token::Number(1.0));
        assert_eq!(lexer.next_token(), Token::Comma);
        assert_eq!(lexer.next_token(), Token::Number(2.0));
        assert_eq!(lexer.next_token(), Token::RBracket);
        assert_eq!(lexer.next_token(), Token::SemiColon);
        assert_eq!(lexer.next_token(), Token::EOF);
    }
}
