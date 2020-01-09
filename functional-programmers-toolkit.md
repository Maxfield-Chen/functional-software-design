# The Functional Programmers Toolkit
* https://www.youtube.com/watch?v=Nrp_LZ-XGsY

## Combination / Aggregation = Monoid
## Mixing Effects and non-Effects = Functor
## Chaining Effects in series = Monad
## Effects in parallel = Applicative

## Monoids
Must follow laws of Closure (a->a->a), associativity (pairwise combo ordering doesn't matter), and has an identity element.
great for metrics

## Functors
Functors must have an effect type, a map function for lifting regular functions, and it must obey the functor laws
Functors can be used to call a regular function on value(s) in an effect.

## Applicative Functors
Applicative functors let you run a regular non-effect function on values within an effect. Usually used to combine effects
Applicatives take a function in an effect and apply it to a value in an effect to get a new value still wrapped in an effect!

## Bind
Bind provides a way to "adapt" functions which only want to handle success cases, so that they can also handle failure cases. This lets you chain together functions in a much more composable way and enables error handling logic to be distinct from functionality.

It can be more generic than this, where bind allows you to convert any functions input signature into an arbitrary input signature.

Bind lets you change "diagonal" functions (functions which move from one world to another) into "horizontal" functions, which operate purely within the world of an effect.

## Monads
Monads have an 
* effect type
* a pure or return function for lifting values into the effect world
* a bind function that converts "diagonal" (world crossing) functions into "horizontal" (E-World only) functions.
  Function adapter!

Often bind is represented as >>=
also obey the monad laws or go to jail

Monads take a function which returns an effect and forces it to take an effect as input!

## Return / Pure
Lifts values into effects world

## Map
Map for a given effect lifts a function into the world of an effect

## Effects
Effects are basically any generic type (a container of some kind, which holds a type)

## General Design Advice

Avoid jumping back and forth between worlds (effect v. non-effects) as much as possible. Lift functions or values using bind or map instead so that all computation happens in the highest world.

Avoid triangles of doom. If you find yourself doing lots of if null then blah checking, consider using bind (monads!). this pattern is called monadic bind
