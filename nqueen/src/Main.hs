module Main where

import           Data.List

type ChessBoard = [Bool]
emptyBoard = map (pure False) [0 .. (boardSize - 1)]
allPositions =
  [ Pair x y | x <- [0 .. (boardSize - 1)], y <- [0 .. (boardSize - 1)] ]
boardSize = rowSize * rowSize
rowSize = 8

data Pair a = Pair a a deriving (Show,Eq)

instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
  pure x = Pair x x
  (Pair f f') <*> (Pair x y) = Pair (f x) (f' y)

type Position = Pair Int

posToIdx :: Position -> Int
posToIdx (Pair x y) = x + (y * rowSize)

unsafePositionLookup :: ChessBoard -> Position -> Bool
unsafePositionLookup c p = c !! posToIdx p

reach :: [Position]
reach =
  let deltas =
          permutations [1, 0] ++ permutations [-1, 0] ++ permutations [1, -1]
  in  map (\(x : y : _) -> Pair x y) deltas

killzone :: Position -> [Position]
killzone q = map (\p -> (+) <$> q <*> p) reach

withinBounds :: Position -> Bool
withinBounds (Pair x y) | x >= 0 && x < rowSize && y >= 0 && y < rowSize = True
                        | otherwise = False

isOccupied :: ChessBoard -> Position -> Bool
isOccupied c p = withinBounds p && unsafePositionLookup c p

isFree :: ChessBoard -> Position -> Bool
isFree c p = not $ isOccupied c p

isDeath :: ChessBoard -> Position -> Bool
isDeath c p = any (isOccupied c) (killzone p)

isSafe :: ChessBoard -> Position -> Bool
isSafe c p = not $ isDeath c p

markOccupied :: ChessBoard -> Position -> ChessBoard
markOccupied c p@(Pair x y)
  | withinBounds p
  = let start = take y c
        end   = drop y c
    in  start ++ [True] ++ end
  | otherwise
  = error ("Attempting to mark invalid position as occupied: " ++ show p)

-- Given a board, return all possible worlds
--placeQueen :: ChessBoard -> [ChessBoard]
--placeQueen c = let spaces = filter isDeath c

main :: IO ()
main = do
  putStrLn "hello world"
