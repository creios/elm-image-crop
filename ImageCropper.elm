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
        , ("border", "1px solid black")
        , ("width", toString model.width ++ "px")
        , ("height", toString model.height ++ "px")
        ] ]
      []
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
      , height h ] []


type Msg
  = Left String
  | Top String
  | Width String
  | Height String


update msg model =
  case msg of
    Left value ->
      { model | left = case toInt value of
                         Err msg
                           -> model.left
                         Ok val
                           -> val }
    Top value ->
      { model | top = case toInt value of
                        Err msg
                          -> model.top
                        Ok val
                          -> val }
    Width value ->
      { model | width = case toInt value of
                          Err msg
                            -> model.width
                          Ok val
                            -> val }
    Height value ->
      { model | height = case toInt value of
                           Err msg
                             -> model.height
                           Ok val
                             -> val }
