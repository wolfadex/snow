# Snow

A toy programming language to learn more about parsers, lambda calculus and the ML family of languages (Haskell, Elm, etc).

The parsing and compiling is done in Elm, and Deno is used to run the compiled Elm code. Velociraptor, a Deno based script runner, is used for building for development and production.

## Run

- Install [Deno](https://deno.land/#installation)
- Install [Elm](https://guide.elm-lang.org/install/elm.html)
- Install [Velociraptor](https://deno.land/x/velociraptor#install)
- Run `vr elm-dev` in 1 terminal (builds, and rebuilds, the compiler)
- Run `vr main-dev <path-to-file>` in a separate terminal (runs the compiler)

## Examples

Integer literals

```
5
```

Lambdas

```
\x -> x
```

Application

```
f x
```

Parentheses

```
f (x y ) 5
```
