module Grid exposing (Sudoku, empty, parse, Grid, mapGrid, gridGet, gridSet, indexedMapGrid, Cell(..), example)

import List
import String

type alias Grid a = List (List a)
type Cell = Unknown
        | Clue Int
        | Guess Int
type alias Sudoku = Grid Cell

example : Sudoku
example = parse "
53..7....
6..195...
.98....6.
8...6...3
4..8.3..1
7...2...6
.6....28.
...419..5
....8..79
"

empty : Sudoku
empty = parse "
.........
.........
.........
.........
.........
.........
.........
.........
.........
"

parse : String -> Sudoku
parse s =
    parseGrid s

parseGrid : String -> Grid Cell
parseGrid s =
    let
        pc : Char -> Cell
        pc c =
            case String.toInt <| String.fromChar c of
                Ok i -> Clue i
                Err _ -> Unknown
    in
        List.map (\l -> List.map pc (String.toList l)) (
            List.filter (\l -> String.length l == 9) (
                String.lines s
            )
        )

indexedMapGrid : (Int -> Int -> a -> b) -> (Grid a)  -> (Grid b)
indexedMapGrid f g =
    let
        lineF y l = List.indexedMap (\x c -> f x y c) l
    in
        List.indexedMap lineF g

mapGrid : (a -> b) -> Grid a -> Grid b
mapGrid f g =
    indexedMapGrid (\x y e -> f e) g

gridGet : Int -> Int -> Grid a -> Maybe a
gridGet x y g =
    case List.head <| List.drop y g of
        Nothing -> Nothing
        Just r -> List.head <| List.drop x r

gridSet : Int -> Int -> Grid a -> a -> Grid a
gridSet x y g e =
    let
        row ry r =
            if ry == y then
                List.indexedMap (\rx re -> if rx == x then e else re) r
            else
                r
    in
        List.indexedMap row g
