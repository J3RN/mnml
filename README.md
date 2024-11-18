![mnml logo](./mnml.svg)

Pronounced "minimal". A small functional language with strong, static typing, type inference, immutable data structures, and records.

```mnml
main = () => {
  println("Hello, World!")
}
```

mnml is inspired by [Elm], [JavaScript], [Grain], [Gleam], and [Haskell], see [Differences](#differences-to-other-languages).

## Terminology for mnml

mnml has three important terms:
1. Value: A value is something like a number or a person's name. `5` is a value. `"Hello!"` is a value.  Some values are more complex, like a lists and records—these are discussed below.
2. Types: Types describe values; specifically, a type specifies a set[^1] of values.  For instance, we would say the type of `5` is `Int`.  `2` is also an `Int`.  However, `"Hello!"` is not an `Int`, it is a member of a different type called `String`.
3. Definitions: Definitions assign a name to either data or a type.  `five = 5` is a value definition (also sometimes called a "binding"; the name "five" is "bound" to the value `5`).  `Bool = True | False` is a type definition (`Bool` describes the set of two values, `True` and `False`).  When a value is associated to a name, the name may be used in place of the value.  e.g. After `five = 5` you can write `five + 6` and this will evaluate to `11`.

## Modules

Every file ending in `.mnml` is considered its own module. The module's name is the file's name without the `.mnml` extension. e.g. `foo.mnml` defines module `foo`.

A module is a series of definitions, namely:

- Value definitions
  ```mnml
  five = 5
  ```
- Type definitions (see [algebraic data types](#algebraic-data-types), below)
  ```mnml
  Maybe(a) = Some(a) | Nothing
  ```
- Type alias definitions
  ```mnml
  alias { name: String, age: Int } as User
  ```

## Primitives

mnml has four primitive types of values:
- `Int` (short for "integer") (e.g. `1`)
- `Float` (short for "floating point number"") (e.g. `3.14`)
- `Char` (short for "character") (e.g. `'a'`)
- `String` (e.g. `"Hello!"`)

Chars must be wrapped in *single quotes* (`'...'`) and Strings must be wrapped in *double quotes* (`"..."`).

## Lists

A list is what it sounds like; it's an ordered set of values.  Those values must all be of the same type, for instance:
```mnml
[1,2,3,5,7]
```
The type for this list is written:
```mnml
[Int]
```

## Records

A record is a mapping of names to values.

A record value looks like this:
```mnml
{ name: "Jonathan", age: 30 }
```
That record's type is written:
```
{ name: String, age: Int }
```

This concept is similar to "maps" (Haskell) or "hashes" (Ruby), but those data structures allow for an arbitrary key type.  The keys for records have no type; they are simply labels for the data.  mnml's records are also similar to tuples in other languages (Haskell, Python), but with labels for the tuple elements.  mnml does not have tuples (at least, not yet), insisting on the use of records instead.

## Functions

A function is a kind of value that expresses a computation.  For instance, this function expresses the addition of `5` and `6`:
```mnml
() => { 5 + 6 }
```

To evaluate the computation, the function must be *invoked*, like so:
```mnml
() => { 5 + 6 }()
```
This expression evaluates to `11`.

This kind of function is, of course, of limited utility.  Functions become far more useful when they have *parameters*, or values that are passed in.  For instance, this function expresses the computation of incrementing the parameter `x`:
```mnml
(x) => { x + 1 }
```

It can be invoked like so:
```
(x) => { x + 1 }(5)
```
This expression evaluates the function with `x` bound to `5` and evaluates to `6`.

Functions can be given names just like any other data:

```mnml
increment = (x) => { x + 1 }
increment(5)
```
This expression evaluates to `6`, as above.

## Algebraic Data Types

mnml has algebraic data types, similar to Haskell, Elm, Rust, and other languages.

What this means is that you can define your own data structure that is a *product* of other types, meaning that it contains multiple types:
```mnml
Pair(a, b) = P(a, b)
```
Here, `Pair(a, b)` is a *type template*.  A type made from this template replaces the *type variables*, `a` and `b`, with types, e.g. `Pair(String, Int)`.  This definition specifies a single *constructor*, `P`, a function invoked to make a value of this type, e.g. `P("Hello", 5)`.

An algebraic data type can also be a *sum* of other types, meaning that it either contains one type or another:
```mnml
Result(a, b) = Success(a) | Failure(b)
```
Here, `Result(a, b)` is the type template, and `Success(a)` and `Failure(b)` are constructors of values of that type.  For instance, the type `Result(Int, String)` contains the values `Success(42)` and `Failure("Something went wrong")`.

An algebraic data type can also be a mix of both sums and products, for instance:
```mnml
Example(a, b, c) = Pair(a, b) | Singleton(c)
```

Or neither:
```mnml
Nil = Nil
```

The type `Nil` has exactly one value, `Nil`.

A truly minimal approach would have been to create constructors (i.e. functions) even when no argument is required (e.g. `Nil`).  That would means that the constructor would need to be invoked without any arguments to create a value (i.e. `Nil` is a constructor and `Nil()` is a value).  This, however, deviates from other similar languages and creates syntactic noise.

The syntax used here is a slight modification of the ML syntax used by Haskell, Elm, and others.  I personally find this confusing, and might move to something closer to Rust's syntax:
```rust
enum Result<T, E> {
	Ok(T),
	Err(E),
}
```

## Type Aliases

Since record types are a bit awkward to write often, mnml has an "alias" mechanism that can be used to assign a different name to a type, for convenience.  e.g.
```
alias { name: String, age: Int } as User
```

With this alias defined in your code, instead of needing to write `{ name: String, age: Int }` in multiple places, you can just write `User` instead.  It's nice for typing and helps with refactoring.

You can create aliases of other types too, if you want.  For instance:
```mnml
alias Pair(Int, Int) as IntPair
```

## Control Flow

At time of writing, mnml's only control flow construct is `case`.

The `case` expression is a mapping of *patterns* to expressions that should be executed if the pattern matches.

```mnml
case foo of
  [{name: firstName}, ...] -> firstName
  []                       -> "N/A"
```

In the example above, `foo` must be a list of records containing at least a `name` key associated to a String.  We know the value must be a String because `case` statements must have a consistent return type, and the second branch returns the String `"N/A"`.  If the list is non-empty, the first pattern (`[{name: firstName}, ...]`) matches, the value associated to the `name` key (bound to a new variable `firstName`) is returned.

If the list is empty, the second pattern (`[]`) matches, and `"N/A"` is returned.

`case` expressions *must* be "covering", which means that a pattern must always match.  `_` is a special pattern that matches anything, and so can be used as an "if no previous pattern matches" pattern in a `case` expression, e.g.:

```mnml
case answer of
  "y"   -> True
  "yes" -> True
  _     -> False
```

In the above example, if `answer` is either `"y"` or `"yes"`, the expression returns `True`.  Otherwise, the expression returns `False`.

This is another instance of borrowing ML syntax that I'm open to changing.

## Differences to Other Languages

### From Elm

Elm was probably the single largest influence on mnml's design.  However, Elm uses the ML syntax which I find difficult to visually parse.  Elm also doesn't alow for functions to have side-effects and, while I think this is an interesting constraint, I believe that, for now, this makes the code I want to write more difficult than it needs to be (see also: [From Haskell](#from-haskell)).  Elm has *limited* ad-hoc polymorphism, as in, there are typeclasses/traits built-in to the language, but users of Elm may not define new ones, whereas mnml allows the user to define new traits, like Haskell and Rust.

### From JavaScript

JavaScript is a controverisal language; while I believe that modern JavaScript contains a lot to love, some parts of the language (e.g. Dates) are severely broken.  mnml's departs from JavaScript in being more functional by design (e.g. having immutable data structures), and mnml provides only one way to do several things for which JavaScript provides many (e.g. define functions).  JavaScript is also the only object oriented language listed among my influences, and this is telling: mnml is not object oriented.

### From Grain

I found [Grain] part way through making mnml and believed momentarily that I had essentially redesigned this language.  mnml's largest departure from Grain is that mnml does not allow mutability.  In Grain, bindings and data structures are not mutable by default, but can be made mutable (by way of `mut` and `box`, respectively).  I believe that not allowing mutability is a healthy constraint that results in more understandable code.

Grain also requires the programmer to define their record types with the `record` keyword.  I understand why this constraint seems reasonable, but I think allowing arbitrary record types with the ability to name them provides for better productivity in practice without sacrificing safety.

Grain also, as far as I can tell, does not have ad-hoc polymorphism.

There are also some differences in module definition, imports, and algebraic data structures, but these are fairly minor.  If you want mnml but with mutation, try [Grain]!

### From Gleam

I worked on [Gleam] and its tooling on-and-off for several years.  mnml and Gleam have many of the same design features, e.g. immutable variables and data structures, being pure while allowing side-effects, etc.  However, I thought Gleam borrowed too much of Rust's complexity.  Gleam also lacks ad-hoc polymorphism (i.e. typeclasses or traits) which was a feature I wanted.

### From Haskell

[Haskell] is, alongside Elm, one of my favorite languages.  This is pretty unsurprising, because Haskell and Elm are very similar.  mnml's largest departures from Haskell are not using ML syntax and allowing functions to have side-effects (as mentioned with Elm).  I believe it is also widely accepted that Haskell's import system is unfriendly, and mnml tries to improve on it.

## Considerations

### Bindings

My current leaning is to not have a keyword for bindings, allowing you to simply write, e.g.
```
foo = 1
```

My current leaning is also to not allow shadowing.  i.e. the following is not allowed:

```
foo = 1
foo = foo + 1
```

When allowing shadowing, you occasionally run into issues where typos create a new variable when you intended to shadow a variable, e.g.

```
foo = 1
fop = foo + 1
do_thing(foo)
```

Here, the author intended to shadow `foo` with its value incremented, but accidentally created a `fop` varaible instead.  `do_thing` is then called with the unincremented `foo`, which is a bug.  However, since we don't (plan to) support shadowing, you would actually *need* to define a new variable (e.g. `fop`) to store the incremented value.

If we do decide to allow shadowing, we'll want to also use a `let` keyword to prevent accidental shadowing.

### If

Originally I didn't think I would include an `if` in mnml because it already has a control-flow construct in `case` (Gleam, for instance, has no `if`).  However, I've come to believe that `if` conveys intent more clearly in a variety of scenarios.  Which leads me to the choice between a few different designs:

1. The Haskell/Grain design: `if ... then ... else ... ` is an expression that can be chained, e.g.
    ```haskell
    if foo
    then "foo"
    else if bar
         then "bar"
         else "neither foo nor bar"
    ```
    This has a nice property in that the `if ... then ... else ...` construct is semantically quite simple, but is quite verbose in my opinion.
2. More of a `cond` or Erlang `if` style:
    ```erlang
    if foo -> "foo"
       bar -> "bar"
       true -> "neither foo nor bar"
    ```
    This style is terse which grants it a nice clarity.  However, my biggest gripe is that this `if`'s "else" is `true ->`.  Indeed, mnml, unlike in Erlang or many lisps, requires that every `if`/`cond` must be "covering", meaning that it must have an "else", so `true ->` would crop up everywhere.  A bit of a weird style, in my opinion, which leads us to the last option:
3. Custom hybrid:
    ```mnml
    if foo -> "foo"
       bar -> "bar"
       otherwise "neither foo nor bar"
    ```
    This style is somewhat alien; it's loosely inspired by Haskell's function guards, e.g.
    ```haskell
    foo x
      | x == "foo" = "foo"
      | x == "bar" = "bar"
      | otherwise  = "neither foo nor bar"
    ```
    `otherwise` is a somewhat peculiar choice of keyword, since it is so long, but when describing code aloud I find that I tend to say "if ... then ... *otherwise* ..." and so I'm inclined to use it.

[^1]: Technically type theory and set theory are distinct.  Specifically, in type theory, a value may only belong to exactly one type, whereas in set theory a value may belong to many sets.  Consider: 1.0 only belongs to the type `Float`, it does not belong to any other type like `Int`, `String`, etc.

<!-- References -->
[Elm]: https://elm-lang.org
[JavaScript]: https://developer.mozilla.org/en-US/docs/Web/JavaScript
[Grain]: https://grain-lang.org
[Gleam]: https://gleam.run
[Haskell]: https://haskell.org
