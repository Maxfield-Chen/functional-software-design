module Lib
  ( module Lib
  )
where

import           Data.Char                      ( toUpper )
import           Control.Monad.State
import Debug.Trace


--type Writer a = (a, String)

--(>=>) :: (a -> Writer b) -> (b -> Writer c) -> (a -> Writer c)
--m1 >=> m2 = \x ->
  --let (y, s1) = m1 x
      --(z, s2) = m2 y
  --in  (z, s1 ++ s2)

--return :: a -> Writer a
--return x = (x, "")

--process :: String -> Writer [String]
--process = upCase >=> toWords

--upCase :: String -> Writer String
--upCase s = (map toUpper s, "upCase ")

--toWords :: String -> Writer [String]
--toWords s = (words s, "toWords")




--data Optional a = Value a | Fail deriving Show

--(>==>) :: (a -> Optional b) -> (b -> Optional c) -> (a -> Optional c)
--m1 >==> m2 = \x -> case m1 x of
  --Fail     -> Fail
  --Value r1 -> m2 r1

--safeReciprocal :: Double -> Optional Double
--safeReciprocal a = if a == 0 then Fail else Value (1 / a)

--safeRoot :: Double -> Optional Double
--safeRoot x = if x == 0 then Fail else Value (sqrt x)

--safeRootReciprocal :: Double -> Optional Double
--safeRootReciprocal = safeReciprocal >==> safeRoot

--factorizer :: (c -> a) -> (c -> b) -> (c -> (a, b))
--factorizer p q x = (p x, q x)

--cofactorizer :: (a -> c) -> (b -> c) -> Either a b -> c
--cofactorizer i j (Left  a) = i a
--cofactorizer i j (Right b) = j b

--data Pair a b = Pair a b

--stmt :: Pair String Bool
--stmt = Pair "test" False

--data Stmt = Stmt String Bool
--stmt2 :: Stmt
--stmt2 = Stmt "test" True

--Named Record Examples (Product Types)
data Element = Element { name :: String
                       , symbol :: String
                       , atomicNumber:: Int }

tupleToElem :: (String, String, Int) -> Element
tupleToElem (n, s, a) = Element { name = n, symbol = s, atomicNumber = a }

elemToTuple :: Element -> (String, String, Int)
elemToTuple e = (name e, symbol e, atomicNumber e)


-- Sum Type Examples (Coproduct types)
data TriEither a b c = Sinstral a | Medial b | Dextral c

--data MaxMaybe a = Either a ()

-- Note that this implictly creates an overloaded constructor w/ 2 different type signatures
-- In algebraic terms, equals the following expression
-- x = 1 + a * x
-- Trying to solve this without subtraction or division is hard, but it eventually leads to 1 + infinite a * aaaaaaaa...
-- which expresses all the possible values of a list :) 
data List a = Nil | Cons a (List a)

--maybeTail :: List a -> Maybe (List a)
--maybeTail Nil        = Nothing
--maybeTail (Cons _ t) = Just t


 -- Algebraic Data Types

prodToSum :: (a, Either b c) -> Either (a, b) (a, c)
prodToSum (x, e) = case e of
  Left  y -> Left (x, y)
  Right z -> Right (x, z)

sumToProd :: Either (a, b) (a, c) -> (a, Either b c)
sumToProd e = case e of
  Left  (x, y) -> (x, Left y)
  Right (x, z) -> (x, Right z)

-- Algebraic DT Exercises
maybeToEither :: Maybe a -> Either () a
maybeToEither a = case a of
  Nothing -> Left ()
  Just a  -> Right a


data Shape = Circle Float | Rect Float Float | Square Float

area :: Shape -> Float
area (Circle r) = 3.14 * r * r
area (Rect w h) = w * h
area (Square s) = s * s


-- Either a a == (Bool, a)
s2p :: Either a a -> (Bool, a)
s2p (Left  a) = (True, a)
s2p (Right a) = (False, a)

p2s :: (Bool, a) -> Either a a
p2s (True , a) = Left a
p2s (False, a) = Right a

-- Chapter 7 - Functors
--fmap :: (a -> b) -> Maybe a -> Maybe b
--fmap _ Nothing  = Nothing
--fmap f (Just x) = Just (f x)

-- Typeclasses
-- Notice that even though this is part of the EQ typeclass, you don't have to state that at definiton 
data Point = Pt Float Float

instance Eq Point where
  (Pt x y) == (Pt x' y') = x == x' && y == y'

-- So f is a functor if there is a function genericFmap with this type signature.
-- lowecase f is a type variable, but the compiler knows its a type constructor rather than a type via context.
class MaxFunctor f where
  genericFmap :: (a -> b) -> f a -> f b

instance MaxFunctor Maybe where
  genericFmap _ Nothing  = Nothing
  genericFmap f (Just x) = Just (f x)

-- Convert the first element and the recursively convert the next elements
instance Functor List where
  fmap f Nil        = Nil
  fmap f (Cons x t) = Cons (f x) (fmap f t)

-- Reader Example
--instance Functor ((->) r) where
  -- f       | ( a -> b)
  -- g       | ( r -> a)
  -- returns | ( r -> b)
  --fmap f g = f . g
  --fmap f g = (.)

-- Functor Composition (acting on composed functors)
maybeTail :: [a] -> Maybe [a]
maybeTail []       = Nothing
maybeTail (x : xs) = Just xs

square x = x * x

mis :: Maybe [Int]
mis = Just [1, 2, 3]

mis2 = fmap (fmap square) mis
--mis2 = (fmap . fmap) square mis
someFunc :: IO ()
someFunc = print mis2


-- Practice with the state monad
type Stack = [Int]

-- Remember that the object should always be last parameter
push :: Int -> Stack -> ((), Stack)
push i s = ((), i : s)

pop :: Stack -> (Int, Stack)
pop (x : xs) = (x, xs)

stackStuff :: Stack -> (Int, Stack)
stackStuff s =
  let (_, s' ) = push 5 s
      (_, s'') = push 6 s'
  in  pop s''

-- Note that in type signature, goes State, Return
-- But in state function, goes (Return, newState)
-- Also note that if extra parameters are required, they go before state
pushS :: Int -> State Stack ()
pushS v = state (\xs -> ((), v : xs))

popS :: State Stack Int
popS = state (\(x : xs) -> (x, xs))

-- Note here that the >> and >>= operators are dealing with threading the state. 
stateStackStuff :: State Stack ()
stateStackStuff = pushS 5 >> pushS 6 >> popS >>= \a -> pushS a 


-- Monad writing practice
-- Here's a data type with a monad instance that combines
-- Either and State
data EitherState s a b = Bad b | Good (s -> (a, s))

