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
    [(20, oneof [return (Just n) | n <- [1 .. 9]]), (80, return Nothing)]

-- | An instance for generating Arbitrary Sudokus
-- prop> isSudoku s
instance Arbitrary Sudoku where
  arbitrary = do
    rows <- sequence [sequence [cell | j <- [1 .. 9]] | i <- [1 .. 9]]
    return (Sudoku rows)

-- | fromString converts an 81-character canonical string encoding for a
-- | Sudoku into our internal representation
fromString :: String -> Sudoku
fromString str = case map fromChar (filter (not . isControl) str) of
    s
        | length s /= 81 -> error "not a valid string"
        | otherwise      -> Sudoku (chunksOf 9 s)
-- | fromChar converts a character into Cell, which is Maybe Int
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
            Just n  -> intToDigit n : toString (Sudoku (ys:xs))

-- | Check structure of a Sudoku: 9 rows, 9 columns, 9 boxes, each of
-- | exactly 9 cells
-- prop> prop_Sudoku
prop_Sudoku :: Sudoku -> Bool
prop_Sudoku (Sudoku s)
    = length (rows s) == 9 && all (\c -> length c == 9) (rows s)
   && length (cols s) == 9 && all (\c -> length c == 9) (cols s)
   && length (boxs s) == 9 && all (\c -> length c == 9) (boxs s)

type Block a = [a]
-- | rows, cols and boxs return lists of blocks,
-- | which are the rows, columns and 3x3 boxes of the matrix respectively
rows :: Matrix a -> [Block a]
rows m = m

cols :: Matrix a -> [Block a]
cols = transpose

-- | boxs convert the matrix into 3x3 boxes with indices
-- | 0 3 6
-- | 1 4 7
-- | 2 5 8
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
okBlock []     = True
okBlock (x:xs) = case x of
    Nothing -> okBlock xs
    _       -> notElem x xs && okBlock xs

-- | No block contains the same integer twice
-- >>> okSudoku allBlanks
-- True
-- >>> okSudoku $ fromString "36..712...5....18...92.47......13.284..5.2..927.46......53.89...83....6...769..43"
-- True
-- >>> okSudoku $ fromString "364871295752936184819254736596713428431582679278469351645328917983147562127695843"
-- True
-- >>> okSudoku $ fromString "36..712...5....18...92.47......13.284..1.2..927.46......53.89...83....6...769..43"
-- False
-- | Check that all blocks (rows, columns and boxes) do not contain the same digit twice
okSudoku :: Sudoku -> Bool
okSudoku (Sudoku s) = all okBlock (rows s) && all okBlock (cols s) && all okBlock (boxs s)

type Pos = (Int, Int)

-- | Return a blank position in the Sudoku
-- >>> blank allBlanks
-- (0,0)

-- | Check that the cell at the blank position is Nothing
-- prop> prop_Blank
prop_Blank :: Sudoku -> Bool
prop_Blank s = case blank s of
    (i, j) -> isNothing (cells s !! i !! j)

-- | Finds a blank cell in the sudoku with fewest candidates by rows and columns
{-
Original naive blank function which finds the first blank cell
blank :: Sudoku -> Pos
blank sud = helper sud 0 0
    where
        helper :: Sudoku -> Int -> Int -> Pos
        helper (Sudoku s) i j = case s of
            []   -> error "can't find blank for empty sudoku"
            x:xs -> case x of
                []   -> helper (Sudoku xs) (i+1) 0
                y:ys -> case y of
                    Nothing -> (i, j)
                    _       -> helper (Sudoku (ys:xs)) i (j+1)
-}
blank :: Sudoku -> Pos
blank (Sudoku s)
    | minCols <= minRows = case elemIndex minCols (countBlanks (cols s)) of
        Nothing -> error "cannot find minCols in blank"
        Just j  -> colhelper (cols s !! j) 0 j
    | otherwise = case elemIndex minRows (countBlanks (rows s)) of
        Nothing -> error "cannot find minRows in blank"
        Just i  -> rowhelper (rows s !! i) i 0
    where
        colhelper :: Block Cell -> Int -> Int -> Pos
        colhelper [] _ _ = error "empty col in colhelper"
        colhelper (x:xs) i j = case x of
            Nothing -> (i, j)
            _       -> colhelper xs (i+1) j

        rowhelper :: Block Cell -> Int -> Int -> Pos
        rowhelper [] _ _ = error "empty row in rowhelper"
        rowhelper (x:xs) i j = case x of
            Nothing -> (i, j)
            _       -> rowhelper xs i (j+1)

        minElem :: [Int] -> Int
        minElem a = case a of
            []  -> 10
            x:xs
                | x > 0 -> min x (minElem xs)
                | otherwise -> minElem xs
        minCols = minElem (countBlanks (cols s))
        minRows = minElem (countBlanks (rows s))
-- | Return the number of blanks for a list of blocks
countBlanks :: [Block Cell] -> [Int]
countBlanks b = map (length . filter isNothing) b

-- | Given a list, and a tuple containing an index in the list and a new value,
-- | update the given list with the new value at the given index.
-- >>> ["a","b","c","d"] !!= (1,"apa")
-- ["a","apa","c","d"]
-- >>> ["p","qq","rrr"] !!= (0,"bepa")
-- ["bepa","qq","rrr"]
(!!=) :: [a] -> (Int, a) -> [a]
(!!=) [] _ = []
(!!=) a (i, e) = case splitAt i a of
    (x,y) -> x ++ e : tail y

-- | Given a Sudoku, a position, and a new cell value,
-- | update the given Sudoku at the given position with the new value.
update :: Sudoku -> Pos -> Int -> Sudoku
update (Sudoku s) (i,j) n = case splitAt i s of
    (_, [])   -> error "error in update: index not in range"
    (x, y:ys) -> Sudoku (x ++ y !!= (j, Just n) : ys)

-- | solve takes an 81-character encoding of a Sudoku puzzle and returns a
-- | list of solutions for it, if any
solve :: String -> [String]
solve [] = []
solve str = case fromString str of
    s -> map toString (solve' s)
    where
        solve' :: Sudoku -> [Sudoku]
        solve' s
            | not (okSudoku s)    = []
            | noBlanks propagated = [propagated]
            | otherwise           = do
                i <- choices
                let s' = update propagated (blank propagated) i
                solve' s'
                where
                    propagated = propagate s
                    row i = rows (cells propagated) !! i
                    col j = cols (cells propagated) !! j
                    box i j = boxs (cells propagated) !! (i `div` 3 + j `div` 3 * 3)
                    choices = case blank propagated of
                        (i, j) ->
                            missingValues (box i j) (
                                missingValues (col j) (
                                    missingValues (row i) [1..9]))


-- | use other methods to fill in more blanks in the sudoku
-- | based on the number of blanks in each block
propagate :: Sudoku -> Sudoku
propagate (Sudoku s)
     -- check for cols/rows
    | 2 `elem` colsBlanks = case elemIndex 2 colsBlanks of
        Just j -> case cols s !! j of
            col -> case elemIndices Nothing col of
                [i1, i2] -> case missingValues col [1..9] of
                    [v1, v2]
                        | v1 `elemOfRow` i1
                            -> update (update (Sudoku s) (i1, j) v2) (i2, j) v1
                        | v1 `elemOfRow` i2
                            -> update (update (Sudoku s) (i1, j) v1) (i2, j) v2
                        | i1 `div` 3 /= i2 `div` 3 && elemOfBox v1 i1 j
                            -> update (update (Sudoku s) (i1, j) v2) (i2, j) v1
                        | i1 `div` 3 /= i2 `div` 3 && elemOfBox v1 i2 j
                            -> update (update (Sudoku s) (i1, j) v1) (i2, j) v2
                        | otherwise -> Sudoku s
    | 2 `elem` rowsBlanks = case elemIndex 2 rowsBlanks of
        Just i -> case rows s !! i of
            row -> case elemIndices Nothing row of
                [j1, j2] -> case missingValues row [1..9] of
                    [v1, v2]
                        | v1 `elem` toInts (cols s !! j1)
                            -> update (update (Sudoku s) (i, j1) v2) (i, j2) v1
                        | v1 `elem` toInts (cols s !! j2)
                            -> update (update (Sudoku s) (i, j1) v1) (i, j2) v2
                        | j1 `div` 3 /= j2 `div` 3 && elemOfBox v1 i j1
                            -> update (update (Sudoku s) (i, j1) v2) (i, j2) v1
                        | j1 `div` 3 /= j2 `div` 3 && elemOfBox v1 i j2
                            -> update (update (Sudoku s) (i, j1) v1) (i, j2) v2
                        | otherwise -> Sudoku s
    | otherwise = Sudoku s

    where
        missingValue b = 45 - sum(map (fromMaybe 0) b)
        colsBlanks = countBlanks (cols s)
        rowsBlanks = countBlanks (rows s)
        boxsBlanks = countBlanks (boxs s)
        elemOfRow e i = e `elem` toInts (rows s !! i)
        elemOfCol e j = e `elem` toInts (cols s !! j)
        elemOfBox e i j = e `elem` toInts (boxs s !! (i `div` 3 + j `div` 3 * 3))

-- | Takes a block and returns all the digits as a list of Int
toInts :: Block Cell -> [Int]
toInts b = map fromJust (filter isJust b)

-- | Takes a block and a list of numbers as the arguments
-- | and find the missing numbers in a block
missingValues :: Block Cell -> [Int] -> [Int]
missingValues b a = case b of
    [] -> a
    x:xs -> case x of
        Nothing -> missingValues xs a
        Just x  -> missingValues xs (filter (/= x) a)

{-
choices :: [Int] -> Block Cell -> [Int]
choices values b = case values of
    [] -> []
    x:xs
        | x `elem` toInts b -> choices xs b
        | otherwise   -> x : choices xs b-}
test :: String
test = "4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"
