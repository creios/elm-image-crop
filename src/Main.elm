import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (style, src, width, height, value, type')
import Html.Events exposing (onInput)
import ImageCropper
import Platform.Cmd
import String exposing (toInt)

main : Program Never
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- Model

type alias Model = ImageCropper.Model

type alias Size =
  { width : Int
  , height : Int
}

init : (Model, Cmd Msg)
init = 
  ( ImageCropper.init
      { width = 900
      , height = 600
      }
      { topLeft =
          { x = 20
          , y = 10
          }
      , bottomRight =
          { x = 140
          , y = 80
          }
      }
  , Cmd.none
  )

-- Update

type Msg
  = ImageCropperMsg ImageCropper.Msg
  | TopLeftX String
  | TopLeftY String
  | BottomRightX String
  | BottomRightY String

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    {selection} = model
    {topLeft,bottomRight} = selection
  in
    case msg of
      ImageCropperMsg msg ->
        let
          (model, cmd) = ImageCropper.update msg model
        in
           ( model
           , Platform.Cmd.map ImageCropperMsg cmd
           )

      TopLeftX value ->
        let
          newTopLeft =
            { topLeft | x = Result.withDefault topLeft.x (toInt value) }

          newSelection =
            { selection | topLeft = newTopLeft  }
        in
          ({ model | selection = newSelection }, Cmd.none)

      TopLeftY value ->
        let
          newTopLeft =
            { topLeft | y = Result.withDefault topLeft.y (toInt value) }

          newSelection =
            { selection | topLeft = newTopLeft }
        in
          ({ model | selection = newSelection }, Cmd.none)

      BottomRightX value ->
        let
          newBottomRight =
            { bottomRight | x = Result.withDefault bottomRight.x (toInt value) }

          newSelection =
            { selection | bottomRight = newBottomRight  }
        in
          ({ model | selection = newSelection }, Cmd.none)

      BottomRightY value ->
        let
          newBottomRight =
            { bottomRight | y = Result.withDefault bottomRight.y (toInt value) }

          newSelection =
            { selection | bottomRight = newBottomRight  }
        in
          ({ model | selection = newSelection }, Cmd.none)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map ImageCropperMsg <| ImageCropper.subscriptions model

-- View

view : Model -> Html Msg
view model =
  div
    []
    [ Html.App.map
        ImageCropperMsg
        (ImageCropper.view
          (unsplashit model.imageSize)
          model)
    , debugForm model.selection
    ]

unsplashit size =
  [ src ("https://unsplash.it/" ++ toString size.width ++ "/" ++ toString size.height ++ "?image=1067")
  , width size.width
  , height size.height
  ]

debugForm : ImageCropper.Rectangle -> Html Msg
debugForm {topLeft,bottomRight} =
  Html.form
    []
    [ text "("
    , coordinateInput TopLeftX topLeft.x
    , text "|"
    , coordinateInput TopLeftY topLeft.y
    , text "), ("
    , coordinateInput BottomRightX bottomRight.x
    , text "|"
    , coordinateInput BottomRightY bottomRight.y
    , text ")"
    ]

coordinateInput : (String -> Msg) -> Int -> Html Msg
coordinateInput msg val =
  input
    [ type' "number"
    , value (toString val)
    , onInput msg
    ] []
