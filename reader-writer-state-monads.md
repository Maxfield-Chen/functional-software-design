# Reader, Writer, and State Monads

## Writer

The writer takes two fields, an accumulator and a return value. 

You can use *tell* to write to the accumulator and *return* to provide a new return value.

It provides *runWriter* to get the pair of (accumulator, return) from a writer.

So you end up writing functions that go a -> Writer a b and use *>>=* to deal with unwrapping the value from the effect and concating the logs
