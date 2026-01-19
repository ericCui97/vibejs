use crate::ast::{Program, Statement, Expression};
use crate::object::{Object, BuiltinFunction};
use crate::object::environment::Environment;

pub fn eval(program: Program, env: &mut Environment) -> Object {
    let mut result = Object::Null;

    for statement in program.statements {
        result = eval_statement(statement, env);
        
        if let Object::ReturnValue(val) = result {
            return *val;
        }
        if let Object::Error(_) = result {
            return result;
        }
    }

    result
}

fn eval_statement(stmt: Statement, env: &mut Environment) -> Object {
    match stmt {
        Statement::Expression(expr_stmt) => eval_expression(expr_stmt.expression, env),
        _ => Object::Null, // TODO: Implement other statements
    }
}

fn eval_expression(expr: Expression, env: &mut Environment) -> Object {
    match expr {
        Expression::IntegerLiteral(int) => Object::Integer(int.value),
        Expression::StringLiteral(string) => Object::String(string.value),
        Expression::Identifier(ident) => eval_identifier(ident.value, env),
        Expression::Call(call) => {
            let function = eval_expression(call.function, env);
            if let Object::Error(_) = function {
                return function;
            }
            
            let args = eval_expressions(call.arguments, env);
            if args.len() == 1 && matches!(args[0], Object::Error(_)) {
                return args[0].clone();
            }
            
            apply_function(function, args)
        },
        _ => Object::Null, // TODO: Implement other expressions
    }
}

fn eval_identifier(name: String, env: &mut Environment) -> Object {
    if let Some(val) = env.get(&name) {
        return val;
    }
    
    if let Some(builtin) = get_builtin(&name) {
        return Object::Builtin(builtin);
    }

    Object::Error(format!("identifier not found: {}", name))
}

fn eval_expressions(exps: Vec<Expression>, env: &mut Environment) -> Vec<Object> {
    let mut result = vec![];

    for e in exps {
        let evaluated = eval_expression(e, env);
        if let Object::Error(_) = evaluated {
            return vec![evaluated];
        }
        result.push(evaluated);
    }

    result
}

fn apply_function(fn_obj: Object, args: Vec<Object>) -> Object {
    match fn_obj {
        Object::Builtin(func) => func(args),
        _ => Object::Error(format!("not a function: {}", fn_obj)),
    }
}

// Builtins

fn get_builtin(name: &str) -> Option<BuiltinFunction> {
    match name {
        "print" => Some(builtin_print),
        _ => None,
    }
}

fn builtin_print(args: Vec<Object>) -> Object {
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            print!(" ");
        }
        print!("{}", arg);
    }
    println!();
    Object::Null
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn test_eval(input: &str) -> Object {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let mut env = Environment::new();
        eval(program, &mut env)
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5.0),
            ("10", 10.0),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match evaluated {
                Object::Integer(val) => assert_eq!(val, expected),
                _ => panic!("object is not Integer. got={:?}", evaluated),
            }
        }
    }
    
    #[test]
    fn test_eval_string_expression() {
        let input = "\"Hello World!\"";
        let evaluated = test_eval(input);
        match evaluated {
            Object::String(val) => assert_eq!(val, "Hello World!"),
            _ => panic!("object is not String. got={:?}", evaluated),
        }
    }

    #[test]
    fn test_builtin_functions() {
        // print returns null, but side effect is printing to stdout
        // here we just check if it runs without error
        let input = "print(\"hello\", \"world\")";
        let evaluated = test_eval(input);
        assert_eq!(evaluated, Object::Null);
    }
}
