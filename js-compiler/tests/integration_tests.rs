use js_compiler::lexer::Lexer;
use js_compiler::parser::Parser;
use js_compiler::evaluator::eval;
use js_compiler::object::environment::Environment;
use std::fs;
use std::path::Path;

#[test]
fn test_integration_fixtures() {
    let fixtures_dir = Path::new("tests/fixtures");
    if !fixtures_dir.exists() {
        // If running from root
        if Path::new("js-compiler/tests/fixtures").exists() {
             panic!("Please run tests from js-compiler directory");
        }
        return;
    }

    for entry in fs::read_dir(fixtures_dir).expect("Failed to read fixtures directory") {
        let entry = entry.expect("Failed to read entry");
        let path = entry.path();
        
        if path.extension().and_then(|s| s.to_str()) == Some("js") {
            let input = fs::read_to_string(&path).expect("Failed to read input file");
            let expected_path = path.with_extension("expected");
            
            if !expected_path.exists() {
                println!("Skipping {} (no .expected file)", path.display());
                continue;
            }
            
            let expected_output = fs::read_to_string(&expected_path)
                .expect("Failed to read expected output file")
                .trim()
                .to_string();

            println!("Running fixture: {}", path.display());

            let lexer = Lexer::new(&input);
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            
            if !parser.errors.is_empty() {
                panic!("Parser errors in {}:\n{:?}", path.display(), parser.errors);
            }

            let mut env = Environment::new();
            eval(program, &mut env);

            let actual_output = env.output.borrow().join("\n");
            
            // 打印测试特性和结果
            println!("测试文件: {}", path.display());
            println!("预期输出:\n{}", expected_output);
            println!("实际输出:\n{}", actual_output.trim());
            
            assert_eq!(
                actual_output.trim(), 
                expected_output, 
                "Output mismatch for fixture: {}", 
                path.display()
            );
        }
    }
}
