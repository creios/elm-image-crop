import Html exposing (..)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing(toInt)


main =
  beginnerProgram { model = init, view = view, update = update }
  
init : Model
init =
  { top = 10
  , left = 20
  , width = 120
  , height = 70
  }
  
type alias Model =
  { top: Int
  , left: Int
  , width: Int
  , height: Int
  }

borders : List (Html Msg)
borders =
  List.map
  border
  [ PositionTop
  , PositionRight
  , PositionBottom
  , PositionLeft
  ]

dragbars : List (Html Msg)
dragbars =
  List.map
    dragbar
    [ PositionTop
    , PositionRight
    , PositionBottom
    , PositionLeft
    ]

handles : List (Html Msg)
handles =
  List.map
    handle
    [ NorthWest
    , North
    , NorthEast
    , East
    , SouthEast
    , South
    , SouthWest
    , West
    ]

px : Int -> String
px value =
  toString value ++ "px"

selectionStyle model =
  style
    [ ("position", "absolute")
    , ("top", px model.top)
    , ("left", px model.left)
    , ("width", px model.width)
    , ("height", px model.height)
    ]

view : Model -> Html Msg
view model =
  div
    []
    [ placeholdit 200 100
    , div
      [ selectionStyle model ]
        (borders ++ dragbars ++ handles)
    , Html.form
      [
      ]
      [ label [] [ text "Links" ]
      , input [ type' "number", value (toString model.left), onInput Left ] []
      , label [] [ text "Oben" ]
      , input [ type' "number", value (toString model.top), onInput Top ] []
      , label [] [ text "Breite" ]
      , input [ type' "number", value (toString model.width), onInput Width ] []
      , label [] [ text "Höhe" ]
      , input [ type' "number", value (toString model.height), onInput Height ] []
      ]
    ]

placeholdit : Int -> Int -> Html Msg
placeholdit w h =
  img [ src ("https://placehold.it/" ++ toString w ++ "x" ++ toString h)
      , width w
      , height h
      ] []

type Position = PositionTop | PositionRight | PositionBottom | PositionLeft
type Orientation = Horizontal | Vertical

border : Position -> Html Msg
border position =
  let
    (cssPosition, orientation) = positionCssHelper position
  in
    div
      [ style
        [ ("position", "absolute")
        , (cssPosition, "-1px")
        , ("width", if orientation == Horizontal then "100%" else "1px")
        , ("height", if orientation == Vertical then "100%" else "1px")
        , ("overflow", "hidden")
        , ("background", "#fff url(http://jcrop.org/css/Jcrop.gif)")
        ]
      ] []

type HandlePosition
  = North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest

handle : HandlePosition -> Html Msg
handle orientation =
  let
    (horizontalPosition, horizontalSpacing) =
      if List.member orientation [NorthWest, SouthWest, West] then
        ("left", "0")

      else if List.member orientation [North, South] then
        ("left", "50%")

      else
        ("right", "0")

    (verticalPosition, verticalSpacing) =
      if List.member orientation [NorthWest, North, NorthEast] then
        ("top", "0")

      else if List.member orientation [East, West] then
        ("top", "50%")

      else
        ("bottom", "0")

    cursor =
      case orientation of
        NorthWest ->
          "nw"
        
        North ->
          "n"
    
        NorthEast ->
          "ne"

        East ->
          "e"

        SouthEast ->
          "se"

        South ->
          "s"

        SouthWest ->
          "sw"

        West ->
          "w"
  in
    div
      [ style
          [ ("background-color", "rgba(49,28,28,0.58)")
          , ("border", "1px #eee solid")
          , ("width", "9px")
          , ("height", "9px")
          , ("position", "absolute")
          , ("opacity", "0.8")
          , (horizontalPosition, horizontalSpacing)
          , ("margin-" ++ horizontalPosition, "-6px")
          , (verticalPosition, verticalSpacing)
          , ("margin-" ++ verticalPosition, "-6px")
          , ("cursor", cursor ++ "-resize")
          ]
      ] []

dragbar : Position -> Html Msg
dragbar position =
  let
    (cssPosition, orientation) = positionCssHelper position

    cursor =
      case position of
        PositionTop ->
          "n"

        PositionRight ->
          "e"

        PositionBottom ->
          "s"

        PositionLeft ->
          "w"
  in
    div
      [ style
          [ ("position", "absolute")
          , ("width", if orientation == Horizontal then "100%" else "9px")
          , ("height", if orientation == Vertical then "100%" else "9px")
          , (cssPosition, "0")
          , ("margin-" ++ cssPosition, "-5px")
          , ("cursor", cursor ++ "-resize")
          ]
      ]
      []

positionCssHelper : Position -> (String, Orientation)
positionCssHelper position =
  case position of
    PositionTop ->
      ("top", Horizontal)
    
    PositionRight ->
      ("right", Vertical)

    PositionBottom ->
      ("bottom", Horizontal)

    PositionLeft ->
      ("left", Vertical)



type Msg
  = Left String
  | Top String
  | Width String
  | Height String


update msg model =
  case msg of
    Left value ->
      { model | left = Result.withDefault model.left (toInt value) }
    Top value ->
      { model | top = Result.withDefault model.top (toInt value) }
    Width value ->
      { model | width = Result.withDefault model.width (toInt value) }
    Height value ->
      { model | height = Result.withDefault model.height (toInt value) }
