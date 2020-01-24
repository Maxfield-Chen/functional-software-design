module Main where

import           Data.List
import           Debug.Trace

type ChessBoard = [Bool]
emptyBoard = map (pure False) [0 .. (boardSize - 1)]
allPositions =
  [ Pair x y | x <- [0 .. (rowSize - 1)], y <- [0 .. (rowSize - 1)] ]
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

idxToPos :: Int -> Position
idxToPos i = Pair (i `mod` rowSize) (floor (fromIntegral i / 8))

unsafePositionLookup :: ChessBoard -> Position -> Bool
unsafePositionLookup c p = c !! posToIdx p

reach :: [Position]
reach =
  let deltas =
          [ [0, 0]
          , [1, 0]
          , [-1, 0]
          , [0, 1]
          , [0, -1]
          , [1, 1]
          , [1, -1]
          , [-1, 1]
          , [-1, -1]
          ]
  in  map (\(x : y : _) -> Pair x y) deltas

killzone :: Position -> [Position]
killzone q = map (\p -> (+) <$> q <*> p) reach

withinBounds :: Position -> Bool
withinBounds (Pair x y) | x >= 0 && x < rowSize && y >= 0 && y < rowSize = True
                        | otherwise = False

isOccupied :: ChessBoard -> Position -> Bool
isOccupied c p = withinBounds p && unsafePositionLookup c p

isRowFree :: ChessBoard -> Position -> Bool
isRowFree c (Pair _ y) =
  let rs  = y * rowSize
      re  = (rs + rowSize) - 1
      row = [rs .. re]
  in  not $ any (isOccupied c . idxToPos) row

isFree :: ChessBoard -> Position -> Bool
isFree c p = not $ isOccupied c p

isDeath :: ChessBoard -> Position -> Bool
isDeath c p = any (isOccupied c) (killzone p)

isSafe :: ChessBoard -> Position -> Bool
isSafe c p = not $ isDeath c p

markOccupied :: ChessBoard -> Position -> ChessBoard
markOccupied c p
  | withinBounds p
  = let idx   = posToIdx p
        start = take idx c
        end   = drop (idx + 1) c
    in  start ++ [True] ++ end
  | otherwise
  = error
    (  "Attempting to mark invalid position as occupied: "
    ++ show p
    ++ "\n"
    ++ show c
    )

maybeFromList :: [a] -> Maybe (a, [a])
maybeFromList xs = case xs of
  (x : _) -> Just (x, xs)
  _       -> Nothing

-- Given a board, return all possible worlds
-- TODO: Refactor using filter with variant?
placeQueen :: ChessBoard -> [ChessBoard]
placeQueen c =
  let spaces = map
        ((&&) <$> (isRowFree c . idxToPos) <*> (isSafe c . idxToPos))
        [0 .. (boardSize - 1)]
  in  foldr
        (\(idx, q) r -> if q then markOccupied c (idxToPos idx) : r else r)
        []
        (zip [0 .. (boardSize - 1)] spaces)

placeNQueens :: Int -> [ChessBoard]
placeNQueens n =
  take n $ unfoldr (maybeFromList . concatMap placeQueen) [emptyBoard]

printSpace :: (Int, Bool) -> IO ()
printSpace (idx, b) =
  let nl = if idx `mod` rowSize == 0 then "\n" else ""
  in  if b then putStr (nl ++ "Q ") else putStr (nl ++ ". ")

printBoard :: ChessBoard -> IO [()]
printBoard c = mapM printSpace (zip [0 .. boardSize - 1] c)

main :: IO ()
main = do
  putStrLn "hello world"
