import Html exposing (..)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing(toInt)
import Result


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

view : Model -> Html Msg
view model =
  div
    []
    [ placeholdit 200 100
    , div
      [ style
        [ ("position", "absolute")
        , ("top", toString model.top ++ "px")
        , ("left", toString model.left ++ "px")
        , ("width", toString model.width ++ "px")
        , ("height", toString model.height ++ "px")
        ] ]
      [ border PositionTop
      , border PositionRight
      , border PositionBottom
      , border PositionLeft
      , handle NorthWest
      , handle North
      , handle NorthEast
      , handle East
      , handle SouthEast
      , handle South
      , handle SouthWest
      , handle West
      ]
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

type BorderPosition = PositionTop | PositionRight | PositionBottom | PositionLeft
type Orientation = Horizontal | Vertical

border : BorderPosition -> Html Msg
border position =
  let
    (cssPosition, orientation) =
      case position of
        PositionTop ->
          ("top", Horizontal)
        
        PositionRight ->
          ("right", Vertical)

        PositionBottom ->
          ("bottom", Horizontal)

        PositionLeft ->
          ("left", Vertical)
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

type HandleOrientation
  = North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest

handle : HandleOrientation -> Html Msg
handle orientation =
  let
    (horizontalPosition, horizontalMargin) =
      if orientation == NorthWest || orientation == SouthWest || orientation == West then
        (("left", "0"), ("margin-left", "-6px"))

      else if orientation == North || orientation == South then
        (("left", "50%"), ("margin-left", "-6px"))

      else
        (("right", "0"), ("margin-right", "-6px"))

    (verticalPosition, verticalMargin) =
      if orientation == NorthWest || orientation == North || orientation == NorthEast then
        (("top", "0"), ("margin-top", "-6px"))

      else if orientation == East || orientation == West then
        (("top", "50%"), ("margin-top", "-6px"))

      else
        (("bottom", "0"), ("margin-bottom", "-6px"))

    cursor =
      (case orientation of
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
          "w") ++ "-resize"
  in
    div
      [ style
          [ ("background-color", "rgba(49,28,28,0.58)")
          , ("border", "1px #eee solid")
          , ("width", "9px")
          , ("height", "9px")
          , ("position", "absolute")
          , ("opacity", "0.8")
          , horizontalPosition
          , verticalPosition
          , horizontalMargin
          , verticalMargin
          , ("cursor", cursor)
          ]
      ] []

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
