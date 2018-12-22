module Sudoku
    ( solve
    , display
    , GridValues
    ) where

import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad

cross :: String -> String -> [String]
cross a b = [ x : y : [] | x <- a, y <- b ]

digits = "123456789"
rows   = "ABCDEFGHI"
cols   = digits

type Square = String
type Digit = Char

-- [
--   "A1","A2","A3","A4","A5","A6","A7","A8","A9",
--   "B1","B2","B3","B4","B5","B6","B7","B8","B9",
--   "C1","C2","C3","C4","C5","C6","C7","C8","C9",
--   "D1","D2","D3","D4","D5","D6","D7","D8","D9",
--   "E1","E2","E3","E4","E5","E6","E7","E8","E9",
--   "F1","F2","F3","F4","F5","F6","F7","F8","F9",
--   "G1","G2","G3","G4","G5","G6","G7","G8","G9",
--   "H1","H2","H3","H4","H5","H6","H7","H8","H9",
--   "I1","I2","I3","I4","I5","I6","I7","I8","I9"
-- ]
squares :: [Square]
squares = cross rows cols

-- [
--   ["A1","B1","C1","D1","E1","F1","G1","H1","I1"],
--   ["A2","B2","C2","D2","E2","F2","G2","H2","I2"],
--   ["A3","B3","C3","D3","E3","F3","G3","H3","I3"],
--   ["A4","B4","C4","D4","E4","F4","G4","H4","I4"],
--   ["A5","B5","C5","D5","E5","F5","G5","H5","I5"],
--   ["A6","B6","C6","D6","E6","F6","G6","H6","I6"],
--   ["A7","B7","C7","D7","E7","F7","G7","H7","I7"],
--   ["A8","B8","C8","D8","E8","F8","G8","H8","I8"],
--   ["A9","B9","C9","D9","E9","F9","G9","H9","I9"],
--
--   ["A1","A2","A3","A4","A5","A6","A7","A8","A9"],
--   ["B1","B2","B3","B4","B5","B6","B7","B8","B9"],
--   ["C1","C2","C3","C4","C5","C6","C7","C8","C9"],
--   ["D1","D2","D3","D4","D5","D6","D7","D8","D9"],
--   ["E1","E2","E3","E4","E5","E6","E7","E8","E9"],
--   ["F1","F2","F3","F4","F5","F6","F7","F8","F9"],
--   ["G1","G2","G3","G4","G5","G6","G7","G8","G9"],
--   ["H1","H2","H3","H4","H5","H6","H7","H8","H9"],
--   ["I1","I2","I3","I4","I5","I6","I7","I8","I9"],
--
--   ["A1","A2","A3","B1","B2","B3","C1","C2","C3"],
--   ["A4","A5","A6","B4","B5","B6","C4","C5","C6"],
--   ["A7","A8","A9","B7","B8","B9","C7","C8","C9"],
--   ["D1","D2","D3","E1","E2","E3","F1","F2","F3"],
--   ["D4","D5","D6","E4","E5","E6","F4","F5","F6"],
--   ["D7","D8","D9","E7","E8","E9","F7","F8","F9"],
--   ["G1","G2","G3","H1","H2","H3","I1","I2","I3"],
--   ["G4","G5","G6","H4","H5","H6","I4","I5","I6"],
--   ["G7","G8","G9","H7","H8","H9","I7","I8","I9"]
-- ]
unitlist :: [[Square]]
unitlist =
  [ cross rows (c:[]) | c <- cols ] ++
  [ cross (r:[]) cols | r <- rows ] ++
  [ cross rs cs | rs <- ["ABC", "DEF", "GHI"], cs <- ["123", "456", "789" ] ]

-- Map where each square is the key and values are lists of units that the
-- square belongs to.
units :: Map Square [[Square]]
units = toMap [(s, u) | s <- squares, u <- unitlist, elem s u]

toMap :: [(Square, [Square])] -> Map Square [[Square]]
toMap xs = foldl addToMap Map.empty xs where
  addToMap m (k, ys) = case Map.lookup k m of
    Just zs -> Map.insert k (ys : zs) m
    Nothing -> Map.insert k [ys] m

-- Map where the each square is the key and the value is a list of the peers
-- which does not include the key. Each square has 20 peers.
peers :: Map Square (Set Square)
peers = Map.mapWithKey f units where
  f k xss = Set.fromList $ filter (\x -> x /= k)(concat xss)

-- Textual representation of the puzzle.
type Grid = String

-- Representation of the puzzle at any state. The key is a Square and the values
-- are a String representing the possible values of the square. If the length of
-- the String is one for all keys the puzzle has been solved.
type GridValues = Map Square String

-- Parse textual representation of the grid.
parseGrid :: Grid -> Maybe GridValues
parseGrid grid = do
  let zero = Just (Map.fromList [(s, digits) | s <- squares])
  xs <- gridValues grid
  let gridValues' = filter (\(_, c) -> c `elem` digits) xs
  foldl (\acc pair -> acc >>= (\m -> assign m pair)) zero gridValues'

-- Convert grid into a dict of {square: char} with '0' or '.' for empties.
-- In our implementation we return a list of pairs and let the caller
-- function convert it into a map.
gridValues :: Grid -> Maybe ([(Square, Char)])
gridValues grid =
  let validChars = '0' : '.' : digits
      chars = [ c | c <- grid, elem c validChars ] in
    if length chars /= 81 then Nothing
    else Just (zip squares chars)

-- Constraint propagation
-- 1. If a square has only one possible value, then eliminate that value from the square's peers.
-- 2. If a unit has only one possible place for a value, then put the value there.
assign :: GridValues -> (Square, Digit) -> Maybe GridValues
assign values (s, d) = do
  let otherValues = List.delete d (Maybe.fromMaybe "" (Map.lookup s values))
  foldl eliminate' (Just values) otherValues where
    eliminate' mValues d2 = mValues >>= (\values' -> eliminate values' (s, d2))

-- Eliminate d from values[s]; propagate when values or places <= 2.
-- Return values, except return Nothing if a contradiction is detected.
eliminate :: GridValues -> (Square, Digit) -> Maybe GridValues
eliminate vs (s, d) =
  case Map.lookup s vs of
    -- Our data is messed up
    Nothing -> Nothing
    -- digits as candidates
    Just ds ->
      -- already eliminated if not found
      case List.elemIndex d ds of
        Nothing -> Just vs
        Just _  -> do
          -- remove digit
          let ds' = List.delete d ds
          -- Contradiction if there are zero candidates, otherwise
          -- update the map with the candidate removed
          vs' <- if length ds' == 0 then Nothing
                 else Just $ Map.insert s ds' vs
          -- (1) If a square s is reduced to one value d2, then
          -- eliminate d2 from the peers.
          vs'' <- if length ds' == 1
                  then
                    let d2 = head ds'
                        ss = Maybe.fromMaybe Set.empty (Map.lookup s peers) in
                      -- Short-circuit if any of the eliminate results is Nothing
                      foldl (\mValues s2 -> do
                        values <- mValues
                        eliminate values (s2, d2)) (Just vs') ss
                  else Just vs'
          -- (2) If a unit u is reduced to only one place for a valud d,
          -- then put it here.
          uss   <- Map.lookup s units
          vs''' <- foldl
            (\acc us -> do
                acc' <- acc
                let dplaces = [s | s <- us, d `elem` (Maybe.fromMaybe [] $ Map.lookup s acc')] in
                  case length dplaces of
                    0 -> Nothing -- Contradiction: no place for this value
                    1 -> assign acc' ((head dplaces), d)
                    _ -> acc) (Just vs'') uss
          return vs'''

-- Display these values as 2D grid.
display :: GridValues -> IO ()
display values = do
  let width = 1 + maximum [ Maybe.fromMaybe 0 $
                            fmap length $
                            Map.lookup s values | s <- squares]
      line  = List.intercalate "+" $ replicate 3 $ replicate (width * 3) '-'
      table = [ buildCell r c | r <- rows, c <- cols] where
        buildCell r c =
          let vs  = Maybe.fromMaybe "" $ Map.lookup (r : c : []) values
              pre = replicate (width - length vs) ' '
              pos = if c `elem` "36" then " |" else if c == '9' then "\n" else ""
              ln  = if r `elem` "CF" && c == '9' then (line ++ "\n") else "" in
            pre ++ vs ++ pos ++ ln
  putStrLn $ List.concat table

search :: GridValues -> Maybe GridValues
search values =
  let xs            = Map.toList values
      allSizeOne    = List.all (\(_, vs) -> length vs == 1) xs
      -- Choose the infilled square s with the fewest possibilities
      ys            = filter (\(_, ds) -> length ds > 1) xs
      (s, ds)       = List.minimumBy (\(s1, v1) (s2, v2) ->
                                        compare (length v1) (length v2)) ys
      assignments   = fmap (\d -> assign values (s, d) >>= search) ds in
    if allSizeOne then Just values -- Solved
    else join $ List.find (Maybe.isJust) assignments

solve :: Grid -> Maybe GridValues
solve grid = parseGrid grid >>= search
