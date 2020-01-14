# Various Laws

## Functor Laws

### Identity
fmap id = id

### Composition
fmap (g . f) = fmap g . fmap f

## Applicative Functor Laws

### Identity
pure id <\*> v = v

### Homomorphism  (function application order doesn't matter)

pure f <\*> pure x = pure (f x)

### Interchange (wrapping order doesn't matter for application)

u :: Applicative
u <\*> pure v = pure ($ v) <\*> u

### Composition

pure (.) <\*> u <\*> v <\*> w = u <\*> (v <\*> w)

## Monad Laws

### Identity

pure a >>= f = f

m >>= pure = m

### Associativity
(m >>= f) >>= g = (\x -> f x >>= g)
Essentially stating that you can move around paren order and it won't matter
Are all monads monoids? or semigroups?

## Verifying instance implementations

* Implement the Arbitrary instance for your new type ( can just use the arbitrary instances of inner prelude types )

* Write a function which represents one of the laws

* Then call quickCheck which will run many different tests to verify that your implementation is correct
