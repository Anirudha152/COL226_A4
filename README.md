# COL226 Assignment 4: Functional Language Abstract Machines (Krivine & SECD)

**Authors:** Anirudha Saraf

## Project Description

This project implements two abstract machines for executing toy functional languages based on lambda calculus: the Krivine Machine (Call-by-Name) and the SECD Machine (Call-by-Value).

The core objective is to simulate the reduction of lambda terms through specific state-transition rules. Beyond the pure lambda calculus, this implementation extends both machines to support primitive data types (integers and booleans) and basic arithmetic/logical operations. This allows for a more complete functional language experience while maintaining the theoretical underpinnings of the abstract machines.

### Key Features

* **Krivine Machine:** Implements Call-by-Name evaluation using closures and an environment. Includes a normalization step to "unload" closures back into readable lambda terms (specifically handling Church numerals).
* **SECD Machine:** Implements Call-by-Value evaluation using a Stack, Environment, Code, and Dump architecture. Includes a compiler that translates lambda terms into a list of opcodes.
* **Language Extensions:** Both machines support native Integers, Booleans, `If-Then-Else` constructs, and operators (`+`, `-`, `*`, `And`, `Or`, `Not`).

## Methodology

### 1. The Krivine Machine (Call-by-Name)

The Krivine machine operates on closures, represented as a pair `<<expression, environment>>`. The environment maps variables to these closures.

* **Closures:** Defined as `Closure of exp * gamma`.
* **Execution:** The machine repeatedly applies transition rules (App, Var, Op) until a head normal form is reached.
* **Unloading:** A distinct `unload` function is implemented to convert the final closure back into a syntactic lambda term. It includes logic to recognize and normalize Church numerals for human-readable output.
* **Lazy Evaluation:** The implementation supports non-strict evaluation, demonstrated by its ability to handle terms like `(true omega)`, where `omega` is an infinite loop.

### 2. The SECD Machine (Call-by-Value)

The SECD machine uses four registers: Stack (S), Environment (E), Control (C), and Dump (D).

* **Compilation:** A `compile` function transforms the high-level functional syntax into a list of low-level instructions (Opcodes).
* **Opcodes:** The instruction set includes `LOOKUP`, `MkCLOS` (Make Closure), `APP` (Apply), `RET` (Return), and primitive operations like `PushInt`, `PLUS`, `IF`, etc.
* **Execution:** The machine processes the opcode list, manipulating the stack and environment. When a function is applied, the current context is saved to the Dump, and restored upon `RET`.

### 3. Language Extensions

To support "Extra Credits" requirements, the following primitives were integrated directly into the evaluation logic of both machines:

* **Arithmetic:** Addition, Subtraction, Multiplication.
* **Logic:** Boolean literals, Not, And, Or.
* **Short-Circuiting:** The `And` and `Or` operations in the Krivine machine (and implicitly via control flow in SECD) utilize short-circuit evaluation.

## Installation and Usage

This project uses OCaml. A `Makefile` is provided for easy compilation.

### Prerequisites

* OCaml compiler (`ocamlc`)
* Make

### Compilation

To compile both machines and their test suites, run:

```bash
make all

```

### Running Tests

To execute the test suites for both machines:

```bash
make test

```

This will run the compiled executables `testkri` (Krivine) and `testsecd` (SECD).

## Results

The following table summarizes the test coverage and results based on the implemented test suites. Both machines were tested against Church encodings, Combinators, and Native Primitives.

| Test Category | Test Case Example            | Krivine (CBN) Result | SECD (CBV) Result |
| --- |------------------------------| --- | --- |
| **Combinators** | Identity, K-Combinator       | Pass | Pass |
| **Church Numerals** | Successor, Addition          | Pass (Normalized) | Pass (Closure Value) |
| **Church Booleans** | Conditional Selection        | Pass | Pass |
| **Primitives** | `15 + 27`                    | `42` | `42` |
| **Logic** | `true && (false \|\| true)`  | `true` | `true` |
| **Recursion** | Y-Combinator \/ Z-Combinator | Pass | Pass |
| **Lazy Eval** | `true`                        |  | `omega` (Short-circuit) |
| **Nested Arith** | `(3 + 7) * (9 - 4)`          | `50` | `50` |

### Comparison Notes

* **Krivine:** Successfully demonstrates lazy evaluation. For example, `And(false, omega)` returns `false` immediately without hanging.
* **SECD:** Requires the Z-combinator (or strict logic) for recursion to ensure arguments are evaluated only when necessary, but handles compiled control flow efficiently.

## Disclaimer

This code was developed as part of an academic assignment for the COL226 course at IIT Delhi. It is intended for educational purposes and was completed in April 2025, it is no longer being actively updated or maintained, please reach out to me over email or linkedin for any queries. Please cite appropriately if used in research or projects. Please refer to the included report for more details.