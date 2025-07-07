## Project Overview: MNML Programming Language

**MNML** (pronounced "minimal") is a custom-built programming language and compiler with the goal of creating a system that is simple, expressive, and enjoyable to use. Inspired by Unison, Elm, JavaScript, Gleam and Haskell, MNML targets a future-facing development experience focused on:

- Deep user customizability, with a compiler and desktop runtime designed to be hackable and introspectable
	
- Hot-reloading and live-updating programs
	
- Strong static typing with type inference
	
- Predictable semantics and control over runtime behavior
	
- A "code as database" design where code is stored in an append-only, content-addressed database

MNML is a component of a larger project (Katahdin) aimed at re-imagining the desktop environment (and/or operating system) as a programmable and inspectable system (["malleable software"]([https://www.inkandswitch.com/essay/malleable-software/](https://www.inkandswitch.com/essay/malleable-software/))). The MNML language serves as the foundation for this vision.

---

## Goals

### Language Goals

- Simplicity in syntax and semantics
	
- Expressive and flexible
	
- "Easy things should be easy, difficult things should be possible"

### Compiler Goals

- Clear, actionable error reporting, assisted by source spans tied to all AST nodes
	
- Constraint-based type inference and checking (type unification)
	
- Friendly, interactive tooling (LSP, REPL, etc.)

### Runtime/Platform Goals

- Clear, auditable permission model
	
- Users can inspect, fork, and hot-reload any component at runtime
	
- Encourages scripting, customization, and small-scale extension

---

## Syntax

- Elm-inspired surface syntax
	
- All functions are lambda expressions assigned to names, e.g. `double = (x => x * 2)`
	
- Function application uses parentheses and supports partial application with `_`
	
- `case` expressions with pattern matching:
	```
	case foo of
      [{name: firstName}, ...] -> firstName
      []                       -> "N/A"
	```
	
- Conditional logic uses Erlang-style `if` with guards:
    
    ```
    if a && b -> "a and b"
       a      -> "only a"
       True   -> "fallback"
    ```
	
- Function application can be done on any expression, not just identifiers (e.g. `foo.bar.baz(1)` or `((x) => x + 1)(2)`
	
- Modules use Unix-style paths (e.g. `foo/bar/baz`)
	
- Foreign references use `::`, e.g. `math/num::add(1, 2)`.  The combination of a module (e.g. `math/num`) and an identifier (type or value) (e.g. `add`) is known as a "qualified reference".  A name in isolation is "unqualified" (but the module may be able to be deduced from context).
	
- Foreign names are namespaced unless explicitly aliased

---

## Type System

- Hindley-Milner-style type inference
	
- Types include: `Int`, `Float`, `Bool`, `Char`, `String`, function types, records, and algebraic data types
	
- Records use row typing for extensibility and ad-hoc polymorphism

---

## Compiler Design and Approaches

There are three main operations of the compiler:

1. Load: Some code is loaded into the database.
2. Interpret: A given function is executed and its output returned.
3. Compile: A given function is compiled to a binary executable.

There will likely be more eventually, but these are the three core operations.

### Load

Loading consists of four stages:
1. Parse: Code is parsed into a "Span AST" (SAST), an AST that tracks the spans for each node.
2. Constrain: Type constraints are generated, and the Span AST is transformed into a "Typed span AST" (TAST) that tracks both the span and the type for each node.
3. Unify: Type constraints are unified, resolving the types in the TAST.
4. Store: Definitions in the TAST are stored in the database.

#### Parse

- Parser is hand-written using Haskell's `Parsec` (might switch to `megaparsec` for improved error messages)
	
- Parsing produces an Span AST (spans facilitate informative error reporting)

#### Constrain

- Each node in the Span AST is converted to a Typed span AST node.  Generally speaking, this consists of creating a type variable that is the type of that node, then defining some constraints on that type variable.  For instance, the `constrain'` function may return (wrapped in the `Constrain` type):
  ```
  (EVar "x" (SourceSpanType start end (MNML.Type.Var "x" [] 0)), [CEqual span (MNML.Type.Var "x" [] 0) MNML.Type.Int])
  ```
  This indicates that the node being constrained is a variable named "x", whose type corresponds to `MNML.Type.Var "x" [] 0`, which is in turn constrained by the constraint `CEqual span (MNML.Type.Var "x" [] 0) MNML.Type.Int`.  The details of `span`, `start`, and `end` have been omitted here because they are irrelevant.
	
- Currently there are `PartialRecord` and `Var Text (Set Trait)` "partial types" which correspond to a (possibly infinite) set of types.  A `PartialRecord` contains a "field spec" (map of names to types) that a compatible record type must have.  For instance, if a `PartialRecord` partial type has the field spec `Map.fromList [("x", MNML.Type.Int)]`, then the record type `Record (Map.fromList [("x", MNML.Type.Int)])` is compatible with that `PartialRecord` partial type, as are the types `Record (Map.fromList [("foo", MNML.Type.String), ("x", MNML.Type.Int)])` and `Record (Map.fromList [("x", MNML.Type.Int), ("y", MNML.Type.Int)]`.  Similarly, a `Var` partial type contains a list of traits that compatible types must implement.  During code generation, instances of these partial types will require either monomorphism or boxing.

#### Unify

- The current type unification algorithm is a variation on Martelli & Montanari that has been extended to support partial types.  This algorithm does not facilitate descriptive error messaging, and so will probably be replaced.

##### Circular Reference Handling

- Type inference avoids redundant work by tracking visited nodes
    
- Mutually recursive functions are supported by analyzing strongly connected components (planned)
    
- Parametric polymorphism is preserved by avoiding premature monomorphization

#### Store

- The store has two tables for each types and values: one that maps names to identifiers, and a second that maps identifiers to data.  e.g. The name `add` might map to identifier `123`, and identifier `123` might map to `ELambda ["x", "y"] ...`.
	
- The database is append-only and content-addressed, meaning that each entry is immutable and identified by a hash of its contents.  This allows for efficient caching and versioning.
	
- The store phase must hash the data to create its identifier, store the identifier to data mapping, then store the name to identifier mapping.

---

## Code Generation

- Compiler targets LLVM
    
- Hot-reloadable component design requires careful function boundaries and state transform hooks
    
- Codegen ensures that type information is preserved where needed (e.g. function exports)

---

## Future Plans

- Lightweight coroutine system for cooperative multitasking
    
- Full-featured standard library for data, math, concurrency, and I/O
    
- Interactive REPL powered by the compiler's query engine
    
- IDE features including go-to-definition and hover types
    
- In-browser playground and tutorial mode

---

## Errata

- Purity by default, with clear boundaries for side effects
	
- Predictable evaluation order (eager by default, lazy via opt-in streams)
	
- Explicit module system with namespaced imports and foreign references
