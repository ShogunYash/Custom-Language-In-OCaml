# Custom Language In OCaml

A custom programming language implemented in OCaml that supports matrix and vector operations as well as standard arithmetic and logical operations. The project comprises:
- Lexical Analysis (lexer.mll using ocamllex)
- Parsing (parser.mly using Menhir)
- Abstract Syntax Tree construction (ast.ml)
- Type Checking (ast_type_checker.ml)
- Interpretation (interpreter.ml)

## Project Overview

This project implements a simple yet powerful programming language from scratch. It covers the entire pipeline:
- A Lexer that tokenizes the source code.
- A Parser that builds an Abstract Syntax Tree (AST).
- A Type Checker that ensures the program is well-typed.
- An Interpreter that executes the program with proper scoping, matrix/vector computations, and control flow.

## Language Features

- **Data Types:**
  - Integers, floats, booleans, and strings.
  - Vectors (1D arrays) and matrices (2D arrays) for both integers and floats.

- **Operations:**
  - **Arithmetic:** Addition, subtraction, multiplication, division, modulo, and power operations (for both integers and floats).
  - **Matrix Operations:** Addition, subtraction, multiplication (including scalar multiplication), transpose, and determinant computation.
  - **Vector Operations:** Addition, subtraction, dot product, scalar multiplication, magnitude, and calculation of the angle between vectors.
  - **Comparisons:** Equality, inequality, less than, greater than, etc.
  - **Logical:** And, or, not, and xor.

- **Control Flow:**
  - **Variable Declaration & Assignment:** Using `let` for declaration and `:=` for assignment.
  - **Conditional Statements:** `if ... then ... else ... end`.
  - **Loops:** `while` and `for` loops.
  - **Input/Output:** `input(expression);` for reading values and `print(expression);` for displaying results.
  - **Block Statements:** Grouping multiple statements within `{ ... }`.

## Implementation Details

- **Lexer (lexer.mll):**  
  Handles tokenization including whitespaces, comments, numerals, identifiers, and specific tokens for vectors/matrices.

- **Parser (parser.mly):**  
  Uses Menhir to convert the token stream into an AST.

- **AST (ast.ml):**  
  Defines the structure for expressions and statements.

- **Type Checker (ast_type_checker.ml):**  
  Performs type checking to ensure consistency in operations and assignments.

- **Interpreter (interpreter.ml):**  
  Evaluates the AST and manages variable environments with proper scoping, executing matrix and vector computations and control flows.

## Supported Statements

- **Variable Declaration and Assignment:**
  ```
  let x := expression;
  x := expression;
  ```
- **Expression Evaluation:**  
  Basic arithmetic, matrix/vector operations, and comparisons.

- **Conditional Statements:**
  ```
  if condition then
    statements
  else
    statements
  end;
  ```
- **Loops:**
  ```
  while condition do
    statements
  end;

  for i := start to end do
    statements
  end;
  ```
- **Input/Output:**
  ```
  input(expression);
  print(expression);
  ```
- **Block Statements:**
  Group multiple statements inside `{ ... }`.

## Usage Examples

```
// Matrix and vector creation
let A := [[1,2,3], [4,5,6], [7,8,9]];
let v := [1, 2, 3];

// Matrix operations
let B := transpose(A);
let C := A * B;
let detA := det(A);

// Vector operations
let magnitude := norm(v);      // Computes vector magnitude
let dotProd := v * v;          // Dot product
let angle := angle(v, v);      // Angle between vectors

// Control flow
if v[0] > 0 then
  print("Positive");
else
  print("Non-positive");
end;
```

## Setup and Running

### Prerequisites
- OCaml (version 4.12 or higher recommended)
- opam (OCaml's package manager)
- Required libraries: Str, Menhir, and others as needed.

### Building the Project
```bash
make
```

### Running the Interpreter
```bash
./main.exe [options] input.txt
```
The executable supports several command-line options:
- -interpret : Type-check and execute the program.
- -typecheck or -check-only : Only perform type checking.
- -tokenize : Tokenize and display the input file.
- -run-test <filename> : Run a specific test file.
- -test : Run built-in test cases.
- -test-type : Run type checker test cases.
- -test-runtime : Run runtime error test cases.

### Running the Test Suite
```bash
./main.exe -test
```
Alternatively, run a specific test file:
```bash
./main.exe -run-test testfile.txt
```

## Future Enhancements
- Adding function definitions and calls.
- Advanced type checking.
- Compilation to bytecode.
- Optimization techniques.
- Expansion of the standard library.

## License
[License information]
