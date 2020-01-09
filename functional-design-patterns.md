# Functional Design Patterns
* https://vimeo.com/113588389

## Totality
Achieving totality is important since it means your type signatures won't lie.

### How to handle functions which may return an error
* Change the type so that they cannot accept input which errors. This also serves as documentation.
* Use an optional type as the return value.

## Parameterize all the things!
* Avoid hard-coded constants
* Parameterize actions not just data
* Decouple behavior from the data 
* This can be done by taking functions as parameters and using partial functions 

## Continuations
* Hollywood principal ("don't call us, we'll call you")
* Instead of making the caller handle input requirements, ask them to provide an error function and a success function
* We can bake in these behaviors using partial functions so they don't have to be provided every time

## Chaining callbacks with continuations
