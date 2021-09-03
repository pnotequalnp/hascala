# hascala
Scala uses implicit variables in order to thread functionality through a code base the same way that
Haskell uses typeclasses. This is a very poor implementation of that strategy in Haskell, using
GHC's `ImplicitParams` feature. There is a major difference however, in how the complier finds values
for the implicit arguments. In Haskell, a function with implicit parameters must be used within the
context of a `let` or `where` that explicitly binds the implicit variables, i.e.
```haskell
let ?implicitArg = someValue in functionWithImplicitParam
```
or within a function that provides that implicit argument in the context, i.e.
```haskell
foo :: (?implicitArg :: SomeType) => Foo -> Bar
foo = bar functionWithImplicitParam
```
Additionally, GHC requires that the name of the implicit binding match the name of the
implicit parameter. So the following fails to compile:
```haskell
foo :: (?implicitArg :: SomeType) => Foo -> Bar
foo = bar functionWithImplicitParam

baz :: Foo -> Bar
baz x = foo x
  where
    ?anotherName = (someValue :: SomeType)
```
because `?anotherName` does not match the name `?implicitArg`.

These differences make using this functionality to replace typeclasses entirely impractical,
especially because you cannot have the same name with two different types in the available implicit
bindings at once, i.e.
```haskell
mconcat :: (?monoid :: Monoid a) => [a] -> a
mconcat = \case
  [] -> mempty
  x:xs -> x <> mconcat xs

foo :: Int
foo = length (fold nestedList) <> fold listOfInts
  where
    ?monoid = intMonoid
    ?monoid = listMonoid
    ?foldable = listFoldable
```
This is valid syntactically, but fails to appropriately dispatch the correct `?monoid` to each `fold`
based on its type. It's a cool idea and fun to learn about, but not useful for actual programming as
far as I know.
