# Learning Path to become proficient in Haskell

[x] Learn to read function definition syntax, sans any extensions

[x] Learn to write and use basic ADTs to represent simple structures

    Encode your own Maybe, Either, linked list, binary tree, and rose tree types and some simple functions that operate on those types.

    Learn how record type syntax works

    Learn how to use type synonyms, and what they do and do not provide for you.

    Learn about deriving clauses - Not like, how they work, just when to use them.

[x] Learn to read a function type signature

[x] Learn to write recursive functions instead of loops

[x] Learn how to use higher order functions on list to do most of what you can do in step #4

    Learn why step 4 is better sometimes and when it makes sense to use either technique

[x] Learn do notation and do some stuff in IO to make actual programs

    [x] Learn how to desugar do notation into a sequence of calls to (>>=)

    [x] Learn the monad laws. Don't go too deep here, just know what they are.

[x] Learn Functor and Applicative

    Learn how to use fmap, pure, and <*> to avoid defining too much stuff using do notation

[x] Understand that steps 6 and 7 are all anyone actually means when they talk about learning to use monads

[x] Understand, again, that all a monad is is something that implements (>>=) and return, and follows the laws.

    [x] Learn the laws again, and try to understand why breaking the laws would be really bad.

[x] Use a bunch of stuff that has a monad instance in IO and get irritated about it. 1.You're not actually learning any skills in this step but it's still really valuable.
  Ugh, either in IO monad is very annoying.

[x] Learn about typeclasses

[x] The syntax for declaring classes and instances

[x] How to use 'minimal'

[x] Study the implementations for Functor, applicative, and monad instances in base for the Either and Maybe types

[x] Study implementations for Alternative and MonadPlus instances for List and Maybe
  [] Write some code using <|>, empty, some, many, mzero, mplus
  [] Study Alternative / MonadPlus Laws

[x] Write your own monad instance for a type that combines the effects of State and Either

[x] Learn about newtypes

Learn about the 'safe constructor' paradigm

[x] Understand what a 'partial function' is

[x] Learn about module imports/exports

[x] Learn either stack or cabal

Learn about hpack

[x] Make a project, install some dependencies

[x] Install some global haskell tools or packages on your system

[-] Learn how to use monad transformers so that you never have to roll your own ever again

[x] Learn to use lens to access and manipulate record types inside a collection

[x] Learn when and why to use bytestring, string, and text

[x] Write a parser using Parsec, Megaparsec, or Attoparsec

[-] Write a simple web server, CLI, or GUI app using the skills above.

You're now an intermediate Haskeller.

There is no set path to guru, but there should now be nothing in between you and learning to write an arbitrarily complex and idiomatic Haskell program.

There is a lot of stuff to learn past this point - a whole world. But it gets extremely non-linear past this point, with a lot of topics of varying complexity and utility, and it starts getting a lot less universally applicable.

## Additional Resources:
* http://lambdaconf.us/downloads/documents/lambdaconf_slfp.pdf
* http://softwaresimply.blogspot.com/2016/08/how-to-get-haskell-job.html
* https://typeclasses.com/phrasebook
* https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/
