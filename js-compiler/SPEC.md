# JavaScript Compiler Specification (ES6+)

本文档定义了本编译器支持的 JavaScript 语法子集。目标是直接支持 ES6+ 语法，不向后兼容旧版（如 `var`）。

## 1. 变量声明
- 仅支持 `let` 和 `const`。
- **不支持** `var`。
- 支持解构赋值（数组和对象）。

## 2. 数据类型
- **原始类型**:
  - `number` (双精度浮点数)
  - `string` (支持单引号 `'`、双引号 `"` 和模板字符串 `` ` ``)
  - `boolean` (`true`, `false`)
  - `null`
  - `undefined`
- **引用类型**:
  - `Object` (对象字面量 `{ key: value }`, 支持简写 `{ key }`)
  - `Array` (数组字面量 `[1, 2, 3]`)

## 3. 函数
- **函数声明**: `function foo() { ... }`
- **箭头函数**: `() => { ... }` 和 `x => x + 1`
- 支持默认参数。
- 支持剩余参数 (`...args`)。

## 4. 控制流
- `if` / `else` / `else if`
- `switch`
- 循环:
  - `for (let i = 0; i < n; i++)`
  - `for (const x of arr)` (迭代器)
  - `while`
  - `do...while`
- `break`, `continue`, `return`

## 5. 类 (Class)
- `class` 关键字声明。
- `constructor` 构造函数。
- `extends` 继承。
- `static` 静态方法。
- 方法定义 (不使用 `function` 关键字)。

## 6. 模块系统
- ES Modules (ESM)
- `import { foo } from './bar'`
- `export const foo = ...`
- `export default ...`

## 7. 运算符
- 算术: `+`, `-`, `*`, `/`, `%`, `**`
- 比较: `===`, `!==` (**不支持** `==`, `!=`), `<`, `<=`, `>`, `>=`
- 逻辑: `&&`, `||`, `!`, `??` (Nullish coalescing)
- 赋值: `=`, `+=`, `-=`, etc.

## 8. 其他特性
- 模板字符串 (Template Literals)
- 展开运算符 (`...`)
- 可选链 (`?.`)
