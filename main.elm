module Sudoku exposing (..)

import List
import Keyboard
import Char
import String
import Html exposing (Html, div, text)
import Html.App as Html
import Html.Attributes as Attr
import Html.Events as Events
import Json.Encode

import Grid


squareSize : Int
squareSize = 25

main =
  Html.program
    { init = (initialState, Cmd.none)
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias GameState =
    {  game : Grid.Sudoku
    , selectedField : Maybe (Int, Int)
    }

type Msg = Click (Int, Int) | KeyPress Char

subscriptions : GameState -> Sub Msg
subscriptions model =
  -- Sub.batch [ Keyboard.ups (\id -> KeyPress <| Char.fromCode id) ]
  Keyboard.downs (\id -> KeyPress <| Char.fromCode id)

update : Msg -> GameState -> (GameState, Cmd Msg)
update msg state =
    let
        setCell x y c = {state | game = Grid.gridSet x y state.game c}
        setSel c = 
            case state.selectedField of
                Nothing -> state
                Just (x, y) -> 
                    case Grid.gridGet x y state.game of
                        Nothing -> state
                        Just old ->
                            case old of
                                Grid.Clue _ -> state
                                _ -> setCell x y c
        moveSelect delta =
            let
                limit n = if n < 0 then 0
                    else if n > 8 then 8
                    else n
                b = case state.selectedField of
                    Nothing -> (0, 0)
                    Just xy ->
                        delta xy
            in
                {state | selectedField = Just (limit <| fst b, limit <| snd b)}
        setSelect xy =
                {state | selectedField = Just xy}
        r = case msg of
            Click xy ->
                setSelect xy
            KeyPress k ->
                case k of
                    'H' -> moveSelect (\(x, y) -> (x-1,y))
                    'J' -> moveSelect (\(x, y) -> (x,y+1))
                    'K' -> moveSelect (\(x, y) -> (x,y-1))
                    'L' -> moveSelect (\(x, y) -> (x+1,y))
                    ' ' -> setSel Grid.Unknown
                    _ -> if Char.isDigit k then
                            case String.toInt <| String.fromChar k of
                                Ok i -> setSel <| Grid.Guess i
                                Err _ -> setSel Grid.Unknown
                        else
                            state
    in
        (r, Cmd.none)

view : GameState -> Html Msg
view state =
    drawGrid state

initialState : GameState
initialState =
    { game = Grid.example
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
                    [ ("background-color", "green" ) ]
                else
                    []
                borderStl = borders x y
            in
                cell (stl ++ borderStl) <| case c of
                    Grid.Clue n -> Html.b [] <| [ text <| toString n ]
                    Grid.Guess n -> div [ Events.onClick (Click (x, y))] [ text <| toString n ]
                    Grid.Unknown -> div [ Events.onClick (Click (x, y))] [ nbsp ]
        cells = Grid.indexedMapGrid rendCell state.game
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
