# mnml

Pronounced "minimal". A small language inspired by [Elm], [JavaScript], [Grain], [Gleam], and [Haskell], see [Differences](#differences).

```mnml
main = () => {
  println("Hello, World!")
}
```

mnml is strongly, statically typed, featuring immutable data structures and records.

## Modules

Every file ending in `.mnml` is considered its own module. The module's name is the file's name without the `.mnml` extension. e.g. `foo.mnml` defines module `foo`.

A module is a series of definitions, namely:

- Type definitions
  ```mnml
  Maybe(a) = Some(a) | Nothing
  ```
- Type alias definitions
  ```mnml
  alias { name: String, age: Int } as User
  ```
- Value definitions
  ```mnml
  five = 5
  ```

Many functional languages have first-class functions, but mnml takes this to it's logical conclusion by having function definitions simply be value definitions where the value is a lambda:

```mnml
main = () => {
  println("Hello, World!")
}
```

## Records

mnml borrows Elm's records: A record is a mapping of names to values, like Elixir's `Map`, Haskell's `Data.Map`, Ruby's `Hash`, etc. However, once a record is constructed, new name-value pairs may not be added.

## Types

mnml is strongly, statically typed with type inference.

## Differences

### From Elm

Elm was probably the single largest influence on mnml's design.  However, Elm uses the ML syntax which I find difficult to visually parse.  Elm also doesn't alow for functions to have side-effects and, while I think this is an interesting constraint, I believe that, for now, this makes the code I want to write more difficult than it needs to be (see also: [From Haskell](###from-haskell)).  Elm has *limited* ad-hoc polymorphism, as in, there are typeclasses/traits built-in to the language, but users of Elm may not define new ones, whereas mnml allows the user to define new traits, like Haskell and Rust.

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
foo = foo +1
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
    This style is terse which grants it a nice clarity.  However, my biggest gripe is that this `if`'s "else" else is `true ->`.  Indeed, in mnml, unlike in Erlang or many lisps, every `if`/`cond` must be "covering", meaning that it must have an "else", so `true ->` would crop up everywhere.  A bit of a weird style, in my opinion, which leads us to the last option:
3. Custom hybrid:
    ```mnml
    if foo -> "foo"
       bar -> "bar"
       otherwise "neither foo nor bar"
    ```

<!-- References -->
[Elm]: https://elm-lang.org
[JavaScript]: https://developer.mozilla.org/en-US/docs/Web/JavaScript
[Grain]: https://grain-lang.org
[Gleam]: https://gleam.run
[Haskell]: https://haskell.org
