---
name: "js-compiler-workflow"
description: "强制执行 TDD 工作流并更新可视化网页。在实现功能、添加测试或更新编译器时调用。"
---

# JS 编译器开发工作流 (Feature-Based)

本 Skill 旨在确保 JS 编译器项目采用**按特性驱动 (Feature-Based)** 的开发模式，并保持可视化工具与编译器进度的同步。

## 1. 特性开发流程
每次开发仅专注于一个具体特性（例如 `let` 声明、`for` 循环、函数定义等）。

### 步骤
1.  **Lexer 实现**：
    - 在 `lexer/token.rs` 添加必要的 Token。
    - 在 `lexer/mod.rs` 实现解析逻辑。
    - **测试**：在 `lexer/mod.rs` 添加对应的单元测试并确保存通过。
2.  **Parser 实现**：
    - 在 `ast/mod.rs` 定义 AST 节点。
    - 在 `parser/mod.rs` 实现解析逻辑。
    - **测试**：在 `parser/mod.rs` 添加对应的单元测试并确保通过。
3.  **Evaluator 实现**（如果适用）：
    - 在 `evaluator/mod.rs` 实现求值逻辑。
    - **测试**：添加对应的单元测试。
4.  **集成测试 (MANDATORY)**：
    - 在 `js-compiler/tests/fixtures/` 添加新的 `.js` 测试用例文件。
    - 添加对应的 `.expected` 预期输出文件。
    - 运行 `cargo test --test integration_tests` 确保通过。
5.  **可视化更新**：
    - 确认后端 API 是否需要调整（通常不需要，除非数据结构发生重大变化）。
    - 修改前端 `visualizer/frontend`，确保能正确展示新特性的 Token 或 AST 结构。
    - 运行可视化网页进行人工验证。
6.  **文档记录**：
    - 将已完成的特性更新到项目根目录的 `TODO.md`。

## 2. 版本控制 (MANDATORY)
- **立即提交**：在完成任何代码修改任务后（无论是编译器特性、可视化更新还是重构），必须**立即**执行 `git commit`。
- **禁止推迟**：严禁将多个不相关的任务合并提交，也不允许在用户未明确要求的情况下推迟提交。
- **提交规范**：
    - 格式：`type(scope): description`
    - 示例：`feat(parser): implement hash literal`, `style(visualizer): improve layout`
- **Push 代码**：在完成所有任务后，立即执行 `git push` 上传代码。

## 3. 测试要求
- **强制单元测试**：Lexer 和 Parser 的每个新特性都必须有对应的 `#[test]`。
- **强制集成测试**：任何涉及执行流或输出的变更，必须通过 Integration Tests。
- **运行测试**：开发过程中频繁运行 `cargo test`。

## 4. 可视化网页架构
- **后端**：Bun (无框架)，通过子进程调用 Rust 编译器。
- **前端**：React (Web 界面)。
- **同步**：前端展示逻辑需跟随 AST 结构更新。

## 5. Definition of Done (DoD) - 必须完成以下所有项才能结束回合
在向用户返回 Final Answer 之前，必须逐项检查：
- [ ] **代码实现**: Lexer/Parser/Evaluator 功能已实现。
- [ ] **测试通过**: `cargo test` (Unit) 和 `cargo test --test integration_tests` (Integration) 全部通过。
- [ ] **代码提交**: `git commit` 已执行。
- [ ] **CRITICAL - 任务规划**: 检查并更新 `TODO.md`：
    1.  勾选 [x] 当前已完成的任务。
    2.  **必须**在 `TODO.md` 中明确列出/确认下一步要做的具体任务（未勾选状态 [ ]）。
    3.  如果所有任务都已完成，必须规划新的阶段性任务。
- [ ] **Final Answer**: 在回答中明确告知用户下一步可以做什么。
