-- Name: My Name
-- UID: u12345678
-- Collaborators: Jane Doe, Joe Bloggs
module Sudoku
  ( allBlanks
  , isSudoku
  , noBlanks
  , printSudoku
  , fromString
  , toString
  , rows
  , cols
  , boxs
  , okBlock
  , okSudoku
  , blank
  , (!!=)
  , update
  , solve
  ) where

import Test.QuickCheck
import Data.List
import Data.Char
import Data.List.Split

-- A matrix is a list of rows.
type Matrix a = [Row a]

-- A row is a list of values
type Row a = [a]

-- A Sudoku puzzle is a matrix of cells
newtype Sudoku =
  Sudoku (Matrix Cell)
  deriving (Show, Eq)

-- | cells extracts the cells from a Sudoku
cells (Sudoku m) = m

-- Each cell may contain a number from 1 to 9, or nothing
type Cell = Maybe Int

example :: Sudoku
example =
  Sudoku
    [ [ Just 3, Just 6, Nothing, Nothing, Just 7, Just 1, Just 2, Nothing, Nothing]
    , [ Nothing, Just 5, Nothing, Nothing, Nothing, Nothing, Just 1, Just 8, Nothing]
    , [ Nothing, Nothing, Just 9, Just 2, Nothing, Just 4, Just 7, Nothing, Nothing]
    , [ Nothing, Nothing, Nothing, Nothing, Just 1, Just 3, Nothing, Just 2, Just 8]
    , [ Just 4, Nothing, Nothing, Just 5, Nothing, Just 2, Nothing, Nothing, Just 9]
    , [ Just 2, Just 7, Nothing, Just 4, Just 6, Nothing, Nothing, Nothing, Nothing]
    , [ Nothing, Nothing, Just 5, Just 3, Nothing, Just 8, Just 9, Nothing, Nothing]
    , [ Nothing, Just 8, Just 3, Nothing, Nothing, Nothing, Nothing, Just 6, Nothing]
    , [ Nothing, Nothing, Just 7, Just 6, Just 9, Nothing, Nothing, Just 4, Just 3]
    ]

eg =

    [ [ Just 3, Just 6, Nothing, Nothing, Just 7, Just 1, Just 2, Nothing, Nothing]
    , [ Nothing, Just 5, Nothing, Nothing, Nothing, Nothing, Just 1, Just 8, Nothing]
    , [ Nothing, Nothing, Just 9, Just 2, Nothing, Just 4, Just 7, Nothing, Nothing]
    , [ Nothing, Nothing, Nothing, Nothing, Just 1, Just 3, Nothing, Just 2, Just 8]
    , [ Just 4, Nothing, Nothing, Just 5, Nothing, Just 2, Nothing, Nothing, Just 9]
    , [ Just 2, Just 7, Nothing, Just 4, Just 6, Nothing, Nothing, Nothing, Nothing]
    , [ Nothing, Nothing, Just 5, Just 3, Nothing, Just 8, Just 9, Nothing, Nothing]
    , [ Nothing, Just 8, Just 3, Nothing, Nothing, Nothing, Nothing, Just 6, Nothing]
    , [ Nothing, Nothing, Just 7, Just 6, Just 9, Nothing, Nothing, Just 4, Just 3]
    ]
-- allBlanks is a Sudoku with just blanks
allBlanks :: Sudoku
allBlanks = Sudoku (replicate 9 (replicate 9 Nothing))

-- | isSudoku checks if a Sudoku has the proper dimensions
-- >>> isSudoku (Sudoku [])
-- False
-- >>> isSudoku allBlanks
-- True
-- >>> isSudoku example
-- True
-- >>> isSudoku (Sudoku (tail (cells example)))
-- False
isSudoku :: Sudoku -> Bool
isSudoku (Sudoku s) =
    length checksudoku == 9 && and checksudoku
        where
            checksudoku = map (\x -> length x == 9) s


-- | noBlanks checks if a Sudoku has no blanks
noBlanks :: Sudoku -> Bool
noBlanks (Sudoku s) = all (notElem Nothing) s

-- | printSudoku prints a Sudoku as a 9 x 9 grid
-- Example:
--    3 6 . . 7 1 2 . .
--    . 5 . . . . 1 8 .
--    . . 9 2 . 4 7 . .
--    . . . . 1 3 . 2 8
--    4 . . 5 . 2 . . 9
--    2 7 . 4 6 . . . .
--    . . 5 3 . 8 9 . .
--    . 8 3 . . . . 6 .
--    . . 7 6 9 . . 4 3
printSudoku :: Sudoku -> IO ()
printSudoku s = putStr $ toString s

-- | cell generates an arbitrary cell in a Sudoku
-- The frequency of Nothing versus Just n values is currently 90% versus 10%,
-- but you may want to change that ratio.
cell :: Gen (Maybe Int)
cell =
  frequency
    [(10, oneof [return (Just n) | n <- [1 .. 9]]), (90, return Nothing)]

-- | An instance for generating Arbitrary Sudokus
-- prop> isSudoku s
instance Arbitrary Sudoku where
  arbitrary = do
    rows <- sequence [sequence [cell | j <- [1 .. 9]] | i <- [1 .. 9]]
    return (Sudoku rows)

-- | fromString converts an 81-character canonical string encoding for a
-- | Sudoku into our internal representation
fromString :: String -> Sudoku
fromString str = Sudoku (chunksOf 9 (map fromChar (filter (not . isControl) str)))

fromChar :: Char -> Maybe Int
fromChar c = case c of
    '.'  -> Nothing
    c    -> Just (digitToInt c)

-- | toString converts a Sudoku into its canonical 81-character string
-- | encoding
-- prop> fromString (toString s) == s
toString :: Sudoku -> String
toString (Sudoku s) = case s of
    []   -> []
    x:xs -> case x of
        []   -> '\n' : toString (Sudoku xs)
        y:ys -> case y of
            Nothing -> '.' : toString (Sudoku (ys:xs))
            Just n  -> chr (n + ord '0') : toString (Sudoku (ys:xs))

type Block a = [a]

rows :: Matrix a -> [Block a]
rows m = m

cols :: Matrix a -> [Block a]
cols = transpose


--boxs :: Matrix a -> [Block a]
boxs m = map concat $ groupBy3 $ helper (concatMap groupBy3 m) 0 0
    where
        helper :: [a] -> Int -> Int -> [a]
        helper [] _ _   = []
        helper x i j
            | i < j+27 = head (drop i x) : helper x (i+3) j
            | j < 2    = helper x (j+1) (j+1)
            | otherwise = []

groupBy3 :: Row a -> [Block a]
groupBy3 a = case a of
    []       -> []
    a:b:c:ds -> [a,b,c] : groupBy3 ds

-- | Test if a block of cells does not contain the same integer twice
-- >>> okBlock [Just 1, Just 7, Nothing, Nothing, Just 3, Nothing, Nothing, Nothing, Just 2]
-- True
-- >>> okBlock [Just 1, Just 7, Nothing, Just 7, Just 3, Nothing, Nothing, Nothing, Just 2]
-- False
okBlock :: Block Cell -> Bool
okBlock b = filtered == nub filtered
    where filtered = filterNothing b

filterNothing :: Block Cell -> Block Cell
filterNothing b = case b of
    []   -> []
    x:xs -> case x of
        Nothing -> filterNothing xs
        _       -> x : filterNothing xs

-- | No block contains the same integer twice
-- >>> okSudoku allBlanks
-- True
-- >>> okSudoku $ fromString "36..712...5....18...92.47......13.284..5.2..927.46......53.89...83....6...769..43"
-- True
-- >>> okSudoku $ fromString "364871295752936184819254736596713428431582679278469351645328917983147562127695843"
-- True
okSudoku :: Sudoku -> Bool
okSudoku (Sudoku s) = isSudoku (Sudoku s) && all okBlock (rows s) && all okBlock (cols s) && all okBlock (boxs s)

type Pos = (Int, Int)

-- | Return a blank position in the Sudoku
-- >>> blank allBlanks
-- (0,0)
-- >>> blank example
-- (0,2)
blank :: Sudoku -> Pos
blank = undefined -- TODO

-- | Given a list, and a tuple containing an index in the list and a new value,
-- | update the given list with the new value at the given index.
-- >>> ["a","b","c","d"] !!= (1,"apa")
-- ["a","apa","c","d"]
-- >>> ["p","qq","rrr"] !!= (0,"bepa")
-- ["bepa","qq","rrr"]
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) = undefined -- TODO

-- | Given a Sudoku, a position, and a new cell value,
-- | update the given Sudoku at the given position with the new value.
update :: Sudoku -> Pos -> Int -> Sudoku
update = undefined -- TODO

-- | solve takes an 81-character encoding of a Sudoku puzzle and returns a
-- | list of solutions for it, if any
solve :: String -> [String]
solve = undefined -- TODO
