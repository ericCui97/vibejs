use crate::ast::{Program, Statement, Expression, BlockStatement};
use crate::object::{Object, BuiltinFunction, ObjectKey};
use crate::object::environment::Environment;
use std::collections::HashMap;

pub fn eval(program: Program, env: &mut Environment) -> Object {
    // Initialize console object
    init_console(env);

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

fn init_console(env: &mut Environment) {
    let mut console_hash = HashMap::new();
    console_hash.insert(
        ObjectKey::String("log".to_string()),
        Object::Builtin(builtin_print)
    );
    
    env.set("console".to_string(), Object::Hash(console_hash));
}

fn eval_statement(stmt: Statement, env: &mut Environment) -> Object {
    match stmt {
        Statement::Expression(expr_stmt) => eval_expression(expr_stmt.expression, env),
        Statement::Return(return_stmt) => {
            let val = eval_expression(return_stmt.return_value, env);
            if let Object::Error(_) = val {
                return val;
            }
            Object::ReturnValue(Box::new(val))
        },
        Statement::Let(let_stmt) => {
            let val = eval_expression(let_stmt.value, env);
            if let Object::Error(_) = val {
                return val;
            }
            env.set(let_stmt.name.value, val);
            Object::Null
        },
        Statement::Block(block) => eval_block_statement(block, env),
        _ => Object::Null, // TODO: Implement other statements
    }
}

fn eval_block_statement(block: BlockStatement, env: &mut Environment) -> Object {
    let mut result = Object::Null;

    for statement in block.statements {
        result = eval_statement(statement, env);

        if let Object::ReturnValue(_) = result {
            return result;
        }
        if let Object::Error(_) = result {
            return result;
        }
    }

    result
}

fn eval_expression(expr: Expression, env: &mut Environment) -> Object {
    match expr {
        Expression::IntegerLiteral(int) => Object::Integer(int.value),
        Expression::StringLiteral(string) => Object::String(string.value),
        Expression::Boolean(boolean) => Object::Boolean(boolean.value),
        Expression::Prefix(prefix) => {
            let right = eval_expression(prefix.right, env);
            if let Object::Error(_) = right {
                return right;
            }
            eval_prefix_expression(prefix.operator, right)
        },
        Expression::Infix(infix) => {
            let left = eval_expression(infix.left, env);
            if let Object::Error(_) = left {
                return left;
            }
            let right = eval_expression(infix.right, env);
            if let Object::Error(_) = right {
                return right;
            }
            eval_infix_expression(infix.operator, left, right)
        },
        Expression::If(if_expr) => {
            eval_if_expression(if_expr.condition, if_expr.consequence, if_expr.alternative, env)
        },
        Expression::Identifier(ident) => eval_identifier(ident.value, env),
        Expression::FunctionLiteral(func) => {
            let params = func.parameters;
            let body = func.body;
            Object::Function(params, body, env.clone())
        },
        Expression::Call(call) => {
            let function = eval_expression(call.function, env);
            if let Object::Error(_) = function {
                return function;
            }
            
            let args = eval_expressions(call.arguments, env);
            if args.len() == 1 && matches!(args[0], Object::Error(_)) {
                return args[0].clone();
            }
            
            apply_function(function, args, env)
        },
        Expression::Member(member) => {
            let left = eval_expression(member.object, env);
            if let Object::Error(_) = left {
                return left;
            }
            
            match member.property {
                Expression::Identifier(ident) => {
                    if let Object::Hash(pairs) = left {
                         let key = ObjectKey::String(ident.value.clone());
                         if let Some(val) = pairs.get(&key) {
                             return val.clone();
                         }
                         return Object::Null; // Property not found
                    }
                    Object::Error(format!("property access not supported on {}", left))
                },
                _ => Object::Error("property must be identifier".to_string()),
            }
        },
        _ => Object::Null, // TODO: Implement other expressions
    }
}

fn eval_if_expression(condition: Expression, consequence: BlockStatement, alternative: Option<BlockStatement>, env: &mut Environment) -> Object {
    let condition = eval_expression(condition, env);
    if let Object::Error(_) = condition {
        return condition;
    }

    if is_truthy(condition) {
        eval_block_statement(consequence, env)
    } else if let Some(alt) = alternative {
        eval_block_statement(alt, env)
    } else {
        Object::Null
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null => false,
        Object::Boolean(true) => true,
        Object::Boolean(false) => false,
        _ => true,
    }
}

fn eval_prefix_expression(operator: String, right: Object) -> Object {
    match operator.as_str() {
        "!" => eval_bang_operator_expression(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Object::Error(format!("unknown operator: {}{}", operator, right.type_name())),
    }
}

fn eval_bang_operator_expression(right: Object) -> Object {
    match right {
        Object::Boolean(true) => Object::Boolean(false),
        Object::Boolean(false) => Object::Boolean(true),
        Object::Null => Object::Boolean(true),
        _ => Object::Boolean(false),
    }
}

fn eval_minus_prefix_operator_expression(right: Object) -> Object {
    match right {
        Object::Integer(value) => Object::Integer(-value),
        _ => Object::Error(format!("unknown operator: -{}", right.type_name())),
    }
}

fn eval_infix_expression(operator: String, left: Object, right: Object) -> Object {
    match (left.clone(), right.clone()) {
        (Object::Integer(left_val), Object::Integer(right_val)) => {
            eval_integer_infix_expression(operator, left_val, right_val)
        },
        (Object::String(left_val), Object::String(right_val)) => {
            eval_string_infix_expression(operator, left_val, right_val)
        },
        (Object::Boolean(left_val), Object::Boolean(right_val)) => {
            eval_boolean_infix_expression(operator, left_val, right_val)
        },
        _ => Object::Error(format!("type mismatch: {} {} {}", left.type_name(), operator, right.type_name())),
    }
}

fn eval_integer_infix_expression(operator: String, left: f64, right: f64) -> Object {
    match operator.as_str() {
        "+" => Object::Integer(left + right),
        "-" => Object::Integer(left - right),
        "*" => Object::Integer(left * right),
        "/" => Object::Integer(left / right),
        "<" => Object::Boolean(left < right),
        ">" => Object::Boolean(left > right),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => Object::Error(format!("unknown operator: INTEGER {} INTEGER", operator)),
    }
}

fn eval_string_infix_expression(operator: String, left: String, right: String) -> Object {
    match operator.as_str() {
        "+" => Object::String(format!("{}{}", left, right)),
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => Object::Error(format!("unknown operator: STRING {} STRING", operator)),
    }
}

fn eval_boolean_infix_expression(operator: String, left: bool, right: bool) -> Object {
    match operator.as_str() {
        "==" => Object::Boolean(left == right),
        "!=" => Object::Boolean(left != right),
        _ => Object::Error(format!("unknown operator: BOOLEAN {} BOOLEAN", operator)),
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

fn apply_function(fn_obj: Object, args: Vec<Object>, env: &mut Environment) -> Object {
    match fn_obj {
        Object::Function(params, body, fn_env) => {
            let mut extended_env = extend_function_env(fn_env, params, args);
            // We need to share the output buffer with the extended environment
            extended_env.output = env.output.clone();
            
            let evaluated = eval_block_statement(body, &mut extended_env);
            unwrap_return_value(evaluated)
        },
        Object::Builtin(func) => func(args, env),
        _ => Object::Error(format!("not a function: {}", fn_obj)),
    }
}

fn extend_function_env(
    env: Environment,
    params: Vec<crate::ast::Identifier>,
    args: Vec<Object>,
) -> Environment {
    let mut enclosed = Environment::new_enclosed(env);

    for (i, param) in params.iter().enumerate() {
        if i < args.len() {
            enclosed.set(param.value.clone(), args[i].clone());
        }
    }

    enclosed
}

fn unwrap_return_value(obj: Object) -> Object {
    if let Object::ReturnValue(val) = obj {
        return *val;
    }
    obj
}

// Builtins

fn get_builtin(name: &str) -> Option<BuiltinFunction> {
    match name {
        "print" => Some(builtin_print),
        _ => None,
    }
}

fn builtin_print(args: Vec<Object>, env: &mut Environment) -> Object {
    let mut output = String::new();
    for (i, arg) in args.iter().enumerate() {
        if i > 0 {
            output.push(' ');
        }
        output.push_str(&format!("{}", arg));
    }
    
    // Print to stdout
    println!("{}", output);
    
    // Capture output
    env.output.borrow_mut().push(output);
    
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
    fn test_eval_boolean_expression() {
        let tests = vec![
            ("true", true),
            ("false", false),
            ("1 < 2", true),
            ("1 > 2", false),
            ("1 < 1", false),
            ("1 > 1", false),
            ("1 == 1", true),
            ("1 != 1", false),
            ("1 == 2", false),
            ("1 != 2", true),
            ("true == true", true),
            ("false == false", true),
            ("true == false", false),
            ("true != false", true),
            ("false != true", true),
            ("(1 < 2) == true", true),
            ("(1 < 2) == false", false),
            ("(1 > 2) == true", false),
            ("(1 > 2) == false", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match evaluated {
                Object::Boolean(val) => assert_eq!(val, expected, "input: {}", input),
                _ => panic!("object is not Boolean. got={:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_bang_operator() {
        let tests = vec![
            ("!true", false),
            ("!false", true),
            ("!5", false),
            ("!!true", true),
            ("!!false", false),
            ("!!5", true),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match evaluated {
                Object::Boolean(val) => assert_eq!(val, expected, "input: {}", input),
                _ => panic!("object is not Boolean. got={:?}", evaluated),
            }
        }
    }

    #[test]
    fn test_eval_integer_expression() {
        let tests = vec![
            ("5", 5.0),
            ("10", 10.0),
            ("-5", -5.0),
            ("-10", -10.0),
            ("5 + 5 + 5 + 5 - 10", 10.0),
            ("2 * 2 * 2 * 2 * 2", 32.0),
            ("-50 + 100 + -50", 0.0),
            ("5 * 2 + 10", 20.0),
            ("5 + 2 * 10", 25.0),
            ("20 + 2 * -10", 0.0),
            ("50 / 2 * 2 + 10", 60.0),
            ("2 * (5 + 10)", 30.0),
            ("3 * 3 * 3 + 10", 37.0),
            ("3 * (3 * 3) + 10", 37.0),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50.0),
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

    #[test]
    fn test_eval_if_expression() {
        let tests = vec![
            ("if (true) { 10 }", Object::Integer(10.0)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Integer(10.0)),
            ("if (1 < 2) { 10 }", Object::Integer(10.0)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Integer(20.0)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Integer(10.0)),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match (evaluated.clone(), expected.clone()) {
                (Object::Integer(val), Object::Integer(exp)) => assert_eq!(val, exp),
                (Object::Null, Object::Null) => {},
                _ => panic!("Expected {:?}, got {:?}", expected, evaluated),
            }
        }
    }

    #[test]
    fn test_eval_return_statements() {
        let tests = vec![
            ("return 10;", 10.0),
            ("return 10; 9;", 10.0),
            ("return 2 * 5; 9;", 10.0),
            ("9; return 2 * 5; 9;", 10.0),
            ("if (10 > 1) { if (10 > 1) { return 10; } return 1; }", 10.0),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match evaluated {
                Object::Integer(val) => assert_eq!(val, expected),
                _ => panic!("Expected Integer({}), got {:?}", expected, evaluated),
            }
        }
    }

    #[test]
    fn test_eval_let_statements() {
        let tests = vec![
            ("let a = 5; a;", 5.0),
            ("let a = 5 * 5; a;", 25.0),
            ("let a = 5; let b = a; b;", 5.0),
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15.0),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match evaluated {
                Object::Integer(val) => assert_eq!(val, expected),
                _ => panic!("Expected Integer({}), got {:?}", expected, evaluated),
            }
        }
    }

    #[test]
    fn test_function_object() {
        let input = "fn(x) { x + 2; };";
        let evaluated = test_eval(input);
        
        match evaluated {
            Object::Function(params, body, _) => {
                assert_eq!(params.len(), 1);
                assert_eq!(params[0].value, "x");
                assert_eq!(body.statements.len(), 1);
            },
            _ => panic!("object is not Function. got={:?}", evaluated),
        }
    }

    #[test]
    fn test_function_application() {
        let tests = vec![
            ("let identity = fn(x) { x; }; identity(5);", 5.0),
            ("let identity = fn(x) { return x; }; identity(5);", 5.0),
            ("let double = fn(x) { x * 2; }; double(5);", 10.0),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10.0),
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20.0),
            ("fn(x) { x; }(5)", 5.0),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match evaluated {
                Object::Integer(val) => assert_eq!(val, expected),
                _ => panic!("Expected Integer({}), got {:?}", expected, evaluated),
            }
        }
    }

    #[test]
    fn test_closures() {
        let input = "
            let newAdder = fn(x) {
                fn(y) { x + y };
            };
            let addTwo = newAdder(2);
            addTwo(2);
        ";
        let evaluated = test_eval(input);
        
        match evaluated {
            Object::Integer(val) => assert_eq!(val, 4.0),
            _ => panic!("Expected Integer(4), got {:?}", evaluated),
        }
    }

    #[test]
    fn test_error_handling() {
        let tests = vec![
            ("5 + true;", "type mismatch: INTEGER + BOOLEAN"),
            ("5 + true; 5;", "type mismatch: INTEGER + BOOLEAN"),
            ("-true", "unknown operator: -BOOLEAN"),
            ("true + false;", "unknown operator: BOOLEAN + BOOLEAN"),
            ("5; true + false; 5", "unknown operator: BOOLEAN + BOOLEAN"),
            ("if (10 > 1) { true + false; }", "unknown operator: BOOLEAN + BOOLEAN"),
            ("if (10 > 1) { if (10 > 1) { return true + false; } return 1; }", "unknown operator: BOOLEAN + BOOLEAN"),
            ("foobar", "identifier not found: foobar"),
        ];

        for (input, expected) in tests {
            let evaluated = test_eval(input);
            match evaluated {
                Object::Error(msg) => assert_eq!(msg, expected),
                _ => panic!("Expected Error({}), got {:?}", expected, evaluated),
            }
        }
    }
}
