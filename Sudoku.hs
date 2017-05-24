-- Name: Ernest Kwan
-- UID: u6381103
-- Collaborators:
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
import Data.Maybe

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
printSudoku s = putStrLn $ unlines (chunksOf 9 (toString s))

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
        []   -> toString (Sudoku xs)
        y:ys -> case y of
            Nothing -> '.' : toString (Sudoku (ys:xs))
            Just n  -> chr (n + ord '0') : toString (Sudoku (ys:xs))

-- | Check structure of a Sudoku: 9 rows, 9 columns, 9 boxes, each of
-- | exactly 9 cells
-- prop> prop_Sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku (Sudoku s)
    = length (rows s) == 9 && all (\c -> length c == 9) (rows s)
   && length (cols s) == 9 && all (\c -> length c == 9) (cols s)
   && length (boxs s) == 9 && all (\c -> length c == 9) (boxs s)

type Block a = [a]

rows :: Matrix a -> [Block a]
rows m = m

cols :: Matrix a -> [Block a]
cols = transpose

boxs :: Matrix a -> [Block a]
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
blank sud = helper sud 0 0
    where
        helper :: Sudoku -> Int -> Int -> Pos
        helper (Sudoku s) i j = case s of
            x:xs -> case x of
                []   -> helper (Sudoku xs) (i+1) 0
                y:ys -> case y of
                    Nothing -> (i, j)
                    _       -> helper (Sudoku (ys:xs)) i (j+1)

blank' :: Sudoku -> Pos
blank' (Sudoku s)
    | minCols <= minRows && minCols <= minBoxs = case elemIndex minCols (countBlanks (cols s)) of
        Just j -> colhelper (cols s !! j) 0 j
    | minRows <= minCols && minRows <= minBoxs = case elemIndex minRows (countBlanks (rows s)) of
        Just i -> rowhelper (rows s !! i) i 0
    | otherwise = case elemIndex minBoxs (countBlanks (boxs s)) of
        Just i -> boxhelper (boxs s !! i) ((i `mod` 3) * 3) ((i `div` 3) * 3) 0
    where
        colhelper :: Block Cell -> Int -> Int -> Pos
        colhelper (x:xs) i j = case x of
            Nothing -> (i, j)
            _       -> colhelper xs (i+1) j

        rowhelper :: Block Cell -> Int -> Int -> Pos
        rowhelper (x:xs) i j = case x of
            Nothing -> (i, j)
            _       -> rowhelper xs i (j+1)

        boxhelper :: Block Cell -> Int -> Int -> Int -> Pos
        boxhelper (x:xs) i j k = case x of
            Nothing -> (i + (k `div` 3), j + (k `mod` 3))
            _       -> boxhelper xs i j (k+1)

        countBlanks = map (length . filter isNothing)
        minElem x = minimum (countBlanks x)
        minCols = minElem (cols s)
        minRows = minElem (rows s)
        minBoxs = minElem (boxs s)

countBlanks = map (length . filter isNothing)
minElem x = minimum (countBlanks x)
minCols (Sudoku s) = minElem (cols s)
minRows (Sudoku s) = minElem (rows s)
minBoxs (Sudoku s) = minElem (boxs s)
-- | Given a list, and a tuple containing an index in the list and a new value,
-- | update the given list with the new value at the given index.
-- >>> ["a","b","c","d"] !!= (1,"apa")
-- ["a","apa","c","d"]
-- >>> ["p","qq","rrr"] !!= (0,"bepa")
-- ["bepa","qq","rrr"]
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) a (i, e) = case splitAt i a of
    (x,y) -> x ++ e : tail y

-- | Given a Sudoku, a position, and a new cell value,
-- | update the given Sudoku at the given position with the new value.
update :: Sudoku -> Pos -> Int -> Sudoku
update (Sudoku s) (i,j) n = case splitAt i s of
    (x, y:ys) -> Sudoku (x ++ y !!= (j, Just n) : ys)

-- | solve takes an 81-character encoding of a Sudoku puzzle and returns a
-- | list of solutions for it, if any
solve :: String -> [String]
solve str = case str of
    [] -> []
    _  -> case fromString str of
        s -> map toString (solve' s)
            --concatMap solve [toString (update s (blank s) i) | i <- [1..9] ]
    where
        solve' :: Sudoku -> [Sudoku]
        solve' s
            | not (okSudoku s) = []
            | noBlanks s       = [s]
            | otherwise        = do
                i <- [1..9]
                let s' = update s (blank s) i
                solve' s'

test :: String
test = "8149765326591234787328..16.9.8.......7.....9.......2.5.91....5...7439.2.4....7..."

eg :: Matrix Cell
eg =
    [ [ Just 3, Just 6, Nothing, Just 8, Just 7, Just 1, Just 2, Nothing, Nothing]
    , [ Nothing, Just 5, Nothing, Just 9, Nothing, Nothing, Just 1, Just 8, Nothing]
    , [ Nothing, Nothing, Just 9, Just 2, Nothing, Just 4, Just 7, Nothing, Nothing]
    , [ Nothing, Nothing, Nothing, Nothing, Just 1, Just 3, Nothing, Just 2, Just 8]
    , [ Just 4, Nothing, Nothing, Just 5, Nothing, Just 2, Nothing, Nothing, Just 9]
    , [ Just 2, Just 7, Nothing, Just 4, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [ Nothing, Nothing, Just 5, Just 3, Nothing, Just 8, Just 9, Nothing, Nothing]
    , [ Nothing, Just 8, Just 3, Nothing, Nothing, Nothing, Nothing, Just 6, Nothing]
    , [ Nothing, Nothing, Just 7, Just 6, Just 9, Nothing, Nothing, Just 4, Just 3]
    ]