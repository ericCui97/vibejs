# JS 编译器 TODO (Feature-Based)

## 开发模式
本编译器采用 **Feature-Based (特性驱动)** 开发模式。每次只专注于一个具体特性（如 `let` 声明、`for` 循环），按以下流程完成：
1. **Lexer**: 实现 Token 解析 + 单元测试。
2. **Parser**: 实现 AST 解析 + 单元测试。
3. **Visualizer**: 更新前端网页以支持可视化 + 人工验证。
4. **Docs**: 在此文档记录完成情况。

## 已完成特性 (Done)
- [x] **基础设施**:
    - [x] 搭建 Rust 项目结构
    - [x] 搭建可视化环境 (React 前端 + Bun 后端)
    - [x] 定义 ES6 语法子集规范 (SPEC.md)
- [x] **基础词法分析 (Lexer)**:
    - [x] 变量声明 (`let`, `const`)
    - [x] 标识符 (Identifier)
    - [x] 数字字面量 (Number)
    - [x] 字符串字面量 (String)
    - [x] 基础运算符 (`+`, `-`, `*`, `/`, `=`, `==`, `!=`)
    - [x] 控制流关键字 (`if`, `else`, `return`, `function`, `true`, `false`)
    - [x] 分隔符 (`(`, `)`, `{`, `}`, `;`, `,`)
- [x] **基础语法分析 (Parser)**:
    - [x] Let Statement (`let x = 10;`)
    - [x] Return Statement (`return 5;`)
    - [x] Expression Statement (`foobar;`)
    - [x] Prefix Expressions (`-5`, `!true`)
    - [x] Infix Expressions (`1 + 2`, `a * b`)
    - [x] If Expression (`if (x) { y } else { z }`)
    - [x] Function Literal (`function(x, y) { ... }`)
    - [x] Assignment Expression (`x = 5`)

## 待开发特性 (Backlog)

### 阶段 2: 控制流
- [x] **If Expression**: 解析 `if (x) { ... } else { ... }`。
- [x] **Function Literal**: 解析 `function(x, y) { ... }`。
- [x] **Call Expression**: 解析函数调用 `add(1, 2)`。

### 阶段 3: 高级特性
- [x] **While Loop**: 解析 `while (x) { ... }`。
- [x] **For Loop**: 解析 `for (let i=0; i<10; i++) { ... }`。
- [x] **Array Literal**: 解析 `[1, 2, 3]`。
- [x] **Hash Literal**: 解析 `{ a: 1, b: 2 }`。

### 阶段 4: 解释器 (Interpreter)
- [x] **Object System**: 定义运行时对象系统 (`Integer`, `Boolean`, `Null`, `ReturnValue`, `Error`) 和环境 (`Environment`)。
- [x] **Evaluator Basics**: 实现基本的求值循环 (`Eval` trait)，支持字面量求值。
- [x] **Builtin Functions**: 实现 `print` 函数。
- [x] **Expression Evaluation**: 支持前缀 (`!`, `-`) 和中缀 (`+`, `-`, `*`, `/`, `>`, `<`, `==`, `!=`) 表达式求值。
- [x] **Statement Evaluation**: 支持 `let` 绑定, `return`, `if/else` 控制流，While/For 循环。
- [ ] **Function Application**: 支持函数调用和闭包。

## 历史记录
*(此处记录每次 Feature 开发完成后的简要说明)*
