module Main where


import           System.Random
import           Debug.Trace
import           Data.Typeable

main :: IO ()
main = do
  putStrLn "hello world"
  where z = 5

-- Side Effects / Debug Info
-- Writer Monad

f :: Int -> Int
f = id

g :: Int -> Int
g = id

f' :: Int -> (Int, String)
f' i = (i, "we found " ++ show i)

g' :: Int -> (Int, String)
g' i = (i, "we found " ++ show i)

-- (f' . g') 5
-- Cannot match type int with (int,string)
-- The f' doesn't expect an int, string!

-- A Naive fix
-- let (y,s) = g' 5
--     (x,s') = f' y in (x, s ++ s')
--
-- but note that we'd have to manually do this for each additional composition. gross.

-- So to fix this we use a higher order function which parameterizes on a function
-- This way we can "fix" the type signature of f' to accept (int, string) instead of int
-- TODO: Understand why we can pattern match arguments from the second parenthesized function
bind :: (Int -> (Int, String)) -> ((Int, String) -> (Int, String))
bind f (gx, gs) = let (fx, fs) = f gx in (fx, gs ++ fs)
-- Now we can do: (bind f' . g') 5

-- The identity element!
-- Note identity promotes a to m a
unit :: Int -> (Int, String)
unit i = (i, "")

-- Note lift promotes a -> a to a -> m a
lift :: (Int -> Int) -> (Int -> (Int, String))
lift f = unit . f

-- ((bind (lift f)) . (lift g)) 5
-- Outputs: (5, "")

-- lift (f . g) == lift f . lift g

-- Example 2: A Container - Multivalued Functions
-- (List Monad)

a :: Int -> Int
a = id

b :: Int -> Int
b = id

a' :: Int -> [Int]
a' = replicate 5
b' :: Int -> [Int]
b' = replicate 10

bind' :: (Int -> [Int]) -> ([Int] -> [Int])
bind' f gs = concatMap f gs

unit' :: Int -> [Int]
unit' x = [x]

lift' :: (Int -> Int) -> (Int -> [Int])
lift' f = unit' . f

-- Randomized Functions (State Monad)

rf :: Int -> StdGen -> (Int, StdGen)
rf x seed = let (rv, seed') = next seed in (x + rv, seed')

rlift :: (Int -> Int) -> (Int -> StdGen -> (Int, StdGen))
rlift f = runit . f

runit :: Int -> StdGen -> (Int, StdGen)
runit x seed = (x, seed)

-- Rbind is returning and passing in extra data to rf
-- * A "random" function rf
-- * A partially applied function g which needs a seed to return a new pair
-- * A starting seed from rf's function arguments
-- We call g with the seed, then pipe the output and the new seed to f

-- Rf used to take
-- Int -> StdGen, it now takes
-- (StdGen -> (Int, StdGen)) -> StdGen
-- where the first parameter represents another random function f,
-- (.) :: (b                                    -> c)                            
--        (StdGen -> (Int, StdGen))             -> (StdGen -> (Int, StdGen))
--
--        (a                        -> b)
--         Int                      -> (StdGen -> (Int, StdGen))
--
--        -> (a -> c)
--        -> Int -> (StdGen -> (Int, StdGen))
--
-- 
-- Main> :t (rbind rf . rf) 0
-- (rbind rf . rf) 0 :: StdGen -> (Int, StdGen)
-- So the integer is partially applied throughout (.) and rbind, and the final
-- result function takes the seed as the second argument.
rbind
  :: (Int -> StdGen -> (Int, StdGen)) -- Original function signature
  -> (StdGen -> (Int, StdGen)) -- New Function Signature 
  -> (StdGen -> (Int, StdGen)) -- Note that original Int input is captured in original sig
rbind f g seed = let (g', seed') = g seed in f g' seed'

-- Generalizing
-- A Monad is the triple of (m, unit, bind) where these functions satisfy the monad laws
-- Note that bind is usually called >>=
-- and  that unit is usually called return

-- Monads for Ordering computation
-- Because bind has a hard requirement on the output of a single function
-- it can be used in lazy languages to order computation.
-- All IO computation happens in the context of a monad to ensure that
-- ordering doesn't happen in a dangerous way. Brilliant!
