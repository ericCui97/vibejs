# VideJS Compiler

A feature-based JavaScript compiler written in Rust, accompanied by a web-based visualizer for inspecting Tokens and Abstract Syntax Trees (AST).

## Project Structure

- **`js-compiler/`**: The core compiler implementation in Rust.
    - **Lexer**: Tokenizes source code (supports `let`, `function`, control flow, etc.).
    - **Parser**: Generates AST (Pratt Parser implementation).
    - **AST**: Definitions of nodes (Statements, Expressions).
- **`visualizer/`**: A web tool to visualize the compilation process.
    - **`frontend/`**: React application (Vite + Bun).
    - **`backend/`**: Bun server that invokes the Rust compiler CLI.

## Features (Implemented)

See [js-compiler/TODO.md](js-compiler/TODO.md) for the detailed roadmap.

- **Lexer**:
    - Variables (`let`, `const`), Integers, Strings, Booleans.
    - Operators (`+`, `-`, `*`, `/`, `==`, `!=`, etc.).
    - Control Flow (`if`, `else`, `return`, `while`, `for`).
    - Data Structures (Arrays `[]`, Hashes `{}`).
- **Parser**:
    - Variable Declarations (`let`).
    - Return Statements.
    - Expressions (Prefix, Infix, Grouped).
    - Control Structures (`if/else`, `while`, `for`).
    - Function Literals & Calls.
    - Array & Hash Literals.

## Getting Started

### Prerequisites

- [Rust](https://www.rust-lang.org/)
- [Bun](https://bun.sh/) (for the visualizer)

### Running the Compiler (Rust)

```bash
cd js-compiler
# Run all unit tests
cargo test

# Run the CLI (if implemented)
cargo run
```

### Running the Visualizer

The visualizer allows you to type JS code and see the resulting AST in real-time.

1. **Start the Backend**:
   ```bash
   cd visualizer/backend
   bun install
   bun run index.ts
   # Server runs on http://localhost:3000
   ```

2. **Start the Frontend**:
   ```bash
   cd visualizer/frontend
   bun install
   bun run dev
   # Open browser at http://localhost:5173
   ```

## Development Workflow

We follow a **Feature-Based** TDD workflow:
1.  Implement Lexer support + Tests.
2.  Implement Parser support + Tests.
3.  Update Visualizer (if needed).
4.  Update Documentation (`TODO.md`).

Check `js-compiler/SPEC.md` for the supported grammar specification.
