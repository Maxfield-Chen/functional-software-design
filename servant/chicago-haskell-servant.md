# Servant

Need client bindings + server + docs + testing

## Motivation
  * Lots of duplication between client, server and docs
  * Syncing has to be done manually (error prone)

### Solutions?
  * Factor out commonalities into an API specification
  * Why are specs not a first class citizen?
    * swagger / blueprint (value level)
    * servant! (type level)

So you define yoru API as types, how to get a web server?

The nice thing is that you get exactly the type you want, no garbage parsing

Servant provides Nat. type. Essentially allows you to transform types between each other (natural transformation)
  "enter" is used to apply natural transform from our monad to other monads.
    Can be used to transform from custom monads to eitherT
  Handlers generated should be blind to HTTP handling
  ServantError should handle that for you
    To prevent mixing concerns, you can have your own domain specific error and nat transform to a servant error.


Type level grammar is extensible for things like cookies and auth.
  provide instance for hasServer, hasClient, hasDocs, then can be used

### Package breakdown

servant - grammar
servant-client / docs / server - interpreters
  Takes TLG and produces objects

requests, readme doc, applications - values

servant-jQuery -> get free javascript calling functions
  -jsforapi

lackey - same thing as above but for ruby

servant-docs -> produces markdown docs

ghcjs-servant-client -> Haskell code that compiles to javascript

servant-client -> autogens client functions in haskell for any web server that follows the spec
  - great for testing

servant-mocks -> Really good for creating a testing server with junk data that follows the spec. 


Can use the raw combinator to serve static resources or other applications
  - websockets via Engine-IO

## Why is a type level DSL better than a type level spec?

one-way! you can always go from types to values, but you can't go the other way.

### hello world example
Server is a type family, which lets you return types like EitherT when you say you're returning a Server
Why are there lists w/ backticks??
Why are there strings / numbers in my types??


### Type / Kinds review
  * Values have types
  * Types have kinds (shape of the type)

#### Data kinds / kind signatures extension
  * data kinds promotes values to types and types to kinds
    * Giving haskell a promotion - paper
  * also gives type level strings + numbers
  * Lets you specify the kinds of types in type declarations
  * single quote is used to denote type versions of data (like lists or data that has been promoted to a type)

#### Type operators extension
  * lets us define a type as an operator
  * It's like an either type but defined infix
  * Used by servant as sub / alternative

#### Polykinds
  * Kind polymorphism
  * all type variables in a type declaration get promoted as polymorphic kinds
  * whew, mind blown
  * Used by servant's proxy
  * used to get around the limitation that you can't pass types as arguments to functions
    * Proxy can be used to put the type into a value
    * :kind proxy = k -> *
    * Proxy does let you convert types to values
    * Show for type level things
  * You can do pattern matching at the type level!!
    * Can do recursion within our type classes
  * bnf grammar

#### Going from types to values
  * done via typeclasses
  * make class hasdocs, hasserver hasclient.
    * Functions at the type level
    * Take a proxy type which contains the API
    * hasdocs accumulates endpoints and actions and returns a single API
      * treat the types like monoids
    * server is more complicated because it keeps the types for safety
    * hasclient builds up a tree of requests and returns that

#### instances as interpretations
  * hmm, review this

### What are type families
  * at a high level, types as functions
  * Stripe package as example
  * HasStripe w/ type function type  a :: *
    * review this, still a little unclear

#### Alternative Content Types
  * ede -> jinja 2
  * lucid -> html templating language
  * juicypixels -> images
