# lambda
A collection of interpreters, type checkers, and REPLs implemented in Haskell. Currently, the following languages are supported:
* Untyped lambda calculus
* [SK combinator calculus]
* System F
* Hindley-Milner type system
* Calculus of constructions

## Untyped lambda calculus
When you enter a term, the interpreter will reduce it to βη-normal form if possible, and display the result. You can use  `\` instead of `λ` for abstractions.

```
> (\b. \c. b c (\x. \y. y)) (\x. \y. x) (\x. \y. y)
λx. λy. y
```

You can also define named constants using `=`.

```
> true = \x. \y. x
> false = \x. \y. y
> and = \b. \c. b c false
> and true false
λx. λy. y
```

When you define a constant, it is added as an argument to all succeeding terms. In the example above, when the user types `and true false`, the interpreter evaluates the expression `(λtrue. (λfalse. (λand. and true false) (λb. λc. b c false)) (λx. λy. y)) (λx. λy. x)`.

Any sequence of basic latin letters and/or digits is a valid variable. For example, `x`, `0`, `Abc`, and `add1` can all be used as variables.

## SK combinator calculus
SK combinator calculus is a restricted version of lambda calculus. It has variables and application, but no λ-abstraction. There are two variables with a special meaning: `s` and `k`. `sxyz` reduces to `xz(yz)`, and `kxy` reduces to `x` (where `x`, `y`, and `z` stand for any term). Variables are limited to a single character, so you don't need to put spaces between them.

```
> s(s(skk)(kx))(ky)z
zxy
```

You can define named constants using `=`.

```
> i = skk
> f = ki
> a = ss(k(kf))
> afk
k(skk)
```

## System F


## Hindley-Milner type system

## Calculus of constructions
