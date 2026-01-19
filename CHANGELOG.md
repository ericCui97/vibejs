# Changelog

## [2026-01-20]

### Added
- **Visualizer**: Integrated `Monaco Editor` to replace the standard textarea, enabling syntax highlighting and better editing experience.
- **Lexer**: Added support for single-line comments (`//`).
- **Evaluator**: Implemented Hash Map (Object) support.
  - Hash Literals `{ key: value }` with support for String, Integer, Boolean, and Identifier keys.
  - Index Expressions `obj["key"]` and `obj[key_var]`.
  - Member Access `obj.key`.
- **Tests**: Added comprehensive integration tests covering:
  - Control Flow (If/Else, Boolean Logic).
  - Functions (Recursion, Closures).
  - Return Statements (Early return, nested return).
  - Hash Maps (Creation, Access, Keys).
  - Comments.

### Changed
- **Environment**: Refactored `Environment` to use `Rc<RefCell<HashMap>>` to support shared state, enabling proper recursion and closure support.
- **Object Display**: Sorted Hash Map keys in `Display` implementation to ensure deterministic output for testing.
- **Workflow**: Updated `js-compiler-workflow` skill to strictly enforce unit testing requirements.

### Fixed
- **Lexer**: Fixed issue where comments were not correctly skipped.
- **Evaluator**: Fixed issue where unquoted Identifier keys in Hash Literals were not treated as strings.
