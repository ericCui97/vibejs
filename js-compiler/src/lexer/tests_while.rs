use crate::lexer::Lexer;
use crate::lexer::token::Token;

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
