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
      [ borderDiv PositionTop
      , borderDiv PositionRight
      , borderDiv PositionBottom
      , borderDiv PositionLeft
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

borderDiv : BorderPosition -> Html Msg
borderDiv position =
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
        , (cssPosition, "0")
        , ("width", if orientation == Horizontal then "100%" else "1px")
        , ("height", if orientation == Vertical then "100%" else "1px")
        , ("overflow", "hidden")
        , ("background", "url(http://jcrop.org/css/Jcrop.gif)")
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
