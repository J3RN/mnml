# mnml

Pronounced "minimal". A small language inspired by Elm, JavaScript, Gleam, and Haskell.

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
  alias User = { name: String, age: Int }
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
