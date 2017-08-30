module Sudoku exposing (..)

import List
import Keyboard
import Char
import String
import Array
import Html exposing (Html, div, text)
import Html as Html
import Html.Attributes as Attr
import Html.Events as Events
import Json.Encode

import Grid


squareSize : Int
squareSize = 25

main =
  Html.program
    { init = (initialState Grid.example, Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- GameState has the board with full history. 'selectedField' is highlighted;
-- changing it doesn't go into the history.
type alias GameState =
    { history: Array.Array Grid.Sudoku
    , current: Int
    , selectedField : Maybe (Int, Int)
    }

type Msg = Click (Int, Int) | KeyPress Char | Undo | Redo

subscriptions : GameState -> Sub Msg
subscriptions model =
  -- Sub.batch [ Keyboard.ups (\id -> KeyPress <| Char.fromCode id) ]
  Keyboard.downs (\id -> KeyPress <| Char.fromCode id)

update : Msg -> GameState -> (GameState, Cmd Msg)
update msg state =
    let
        setSelectedCell c = 
            case state.selectedField of
                Nothing -> state
                Just (x, y) -> 
                    case Grid.gridGet x y (current state) of
                        Nothing -> state
                        Just old ->
                            case old of
                                Grid.Clue _ -> state
                                _ -> doMove state (x, y) c
        setSelect xy =
                {state | selectedField = Just xy}
        moveSelect delta =
            let
                b = case state.selectedField of
                    Nothing -> (0, 0)
                    Just xy ->
                        delta xy
                fst (x, y) = x
                snd (x, y) = y
            in
                {state | selectedField = Just (clamp 0 8 <| fst b, clamp 0 8 <| snd b)}
        r = case msg of
            Click xy ->
                setSelect xy
            KeyPress k ->
                case k of
                    'H' -> moveSelect (\(x, y) -> (x-1,y))
                    'J' -> moveSelect (\(x, y) -> (x,y+1))
                    'K' -> moveSelect (\(x, y) -> (x,y-1))
                    'L' -> moveSelect (\(x, y) -> (x+1,y))
                    ' ' -> setSelectedCell Grid.Unknown
                    _ -> if Char.isDigit k then
                            case String.toInt <| String.fromChar k of
                                Ok i -> setSelectedCell <| Grid.Guess i
                                Err _ -> setSelectedCell Grid.Unknown
                        else
                            state
            Undo ->
                undo state
            Redo ->
                redo state
    in
        (r, Cmd.none)

-- current gives the board we're looking at. It's the last from 'history',
-- unless we're in 'undo' state.
current : GameState -> Grid.Sudoku
current s =
    case Array.get s.current s.history of
        Nothing -> Grid.empty -- impossible
        Just g -> g

doMove : GameState -> (Int, Int) -> Grid.Cell -> GameState
doMove state (x, y) cell =
    let
        old = current state
        new = Grid.gridSet x y old cell
        -- drop the 'redo' future, if any
        his = Array.slice 0 (state.current+1) state.history
    in
        {   state |
            history = Array.push new his,
            current = state.current + 1
        }

undo : GameState -> GameState
undo state =
    { state |
        current = max 0 (state.current - 1)
    }

undoAvailable : GameState -> Bool
undoAvailable state =
    state.current > 0

redo : GameState -> GameState
redo state =
    { state |
        current = min ((Array.length state.history) - 1) (state.current + 1)
    }

redoAvailable : GameState -> Bool
redoAvailable state =
    state.current < (Array.length state.history - 1)

view : GameState -> Html Msg
view state =
    div []
        [ drawGrid state
        , Html.button [
            Events.onClick Undo,
            Attr.disabled <| not <| undoAvailable state
        ] [ text "< undo" ]
        , Html.button [
            Events.onClick Redo,
            Attr.disabled <| not <| redoAvailable state
        ] [ text "redo >" ]
    ]

initialState : Grid.Sudoku -> GameState
initialState puzzle =
    { history = Array.push puzzle Array.empty
    , current = 0
    , selectedField = Nothing
    }


borderThin = "solid 1px black"
borderThick = "solid 2px black"

-- styles for cell borders
borders : Int -> Int -> List (String, String)
borders x y =
    let left = if x == 0 || x % 3 == 0 then
                [("border-left", borderThick)]
            else
                [("border-left", borderThin)]
        right = if x == 8 then
                [("border-right", borderThick)]
            else
                []
        top = if y == 0 || y % 3 == 0 then
                [("border-top", borderThick)]
            else
                [("border-top", borderThin)]
        bottom = if y == 8 then
                [("border-bottom", borderThick)]
            else
                []
    in
        left ++ right ++ top ++ bottom

cell : List (String, String) -> Html Msg -> Html Msg
cell style v =
    let
      s = 
        [ ("width", (toString squareSize) ++ "px")
        , ("height", (toString squareSize) ++ "px")
        , ("display", "inline-block")
        , ("margin", "auto")
        , ("text-align", "center")
        , ("vertical-align", "middle")
        , ("line-height", (toString squareSize) ++ "px")
        ] ++ style
    in
      div [ Attr.style s ] [ v ]

blockStyle = Attr.style
        [ ("display", "inline-block")
        , ("border", "solid 1px green")
        ]

drawGrid : GameState -> Html Msg
drawGrid state =
    let
        rendCell : Int -> Int -> Grid.Cell -> Html Msg
        rendCell x y c =
            let stl = 
                if Just (x, y) == state.selectedField then
                    [ ("background-color", "lightgreen" ) ]
                else
                    []
                borderStl = borders x y
            in
                cell (stl ++ borderStl) <| case c of
                    Grid.Clue n -> Html.b [] <| [ text <| toString n ]
                    Grid.Guess n -> div [ Events.onClick (Click (x, y))] [ text <| toString n ]
                    Grid.Unknown -> div [ Events.onClick (Click (x, y))] [ nbsp ]
        cells = Grid.indexedMapGrid rendCell (current state)
    in
        divGrid [] cells

divGrid : List (Html.Attribute Msg) -> Grid.Grid (Html Msg) -> Html Msg
divGrid attr g =
    let
        rendLine l = div [] l
    in
        div attr <| List.map rendLine g

nbsp : Html Msg
-- from the faq: http://faq.elm-community.org/#how-can-i-output-literal-html-and-avoid-escaping-of-entities
nbsp = Html.span [ Attr.property "innerHTML" (Json.Encode.string "&nbsp;") ] []
