import Html exposing (..)
import Html.App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String exposing (toInt)
import Mouse exposing (Position)
import Json.Decode as Json


main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
  
init : (Model, Cmd Msg)
init =
  ( { selection =
      { x = 20
      , y = 10
      , width = 120
      , height = 70
      }
    , drag = Nothing
    }
  , Cmd.none
  )
  
type alias Model =
  { selection : Area
  , drag : Maybe Mouse.Position
  }

type alias Area =
  { x : Int
  , y : Int
  , width : Int
  , height : Int
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

selectionStyle : Area -> Attribute Msg
selectionStyle selection =
  style
    [ ("position", "absolute")
    , ("left", px selection.x)
    , ("top", px selection.y)
    , ("width", px selection.width)
    , ("height", px selection.height)
    , ("cursor", "move")
    ]

onMouseDown : Attribute Msg
onMouseDown =
  on "mousedown" (Json.map DragStart Mouse.position)

view : Model -> Html Msg
view model =
  div
    []
    [ placeholdit 200 100
    , div
        [ selectionStyle model.selection
        , onMouseDown
        ]
        (borders ++ dragbars ++ handles)
    , debugForm model.selection
    ]

debugForm : Area -> Html Msg
debugForm selection =
  Html.form
    []
    [ label [] [ text "X" ]
    , input
      [ type' "number"
      , value (toString selection.x)
      , onInput Left
      ] []
    , label [] [ text "Y" ]
    , input
      [ type' "number"
      , value (toString selection.y)
      , onInput Top
      ] []
    , label [] [ text "Breite" ]
    , input
      [ type' "number"
      , value (toString selection.width)
      , onInput Width
      ] []
    , label [] [ text "Höhe" ]
    , input
      [ type' "number"
      , value (toString selection.height)
      , onInput Height
      ] []
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
  | DragStart Mouse.Position
  | DragAt Mouse.Position
  | DragEnd Mouse.Position

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    selection = model.selection
  in
    case msg of
      Left value ->
        let
          newSelection = { selection | x = Result.withDefault selection.x (toInt value) }
        in
          ({ model | selection = newSelection }, Cmd.none)
      Top value ->
        let
          newSelection = { selection | y = Result.withDefault selection.y (toInt value) }
        in
          ({ model | selection = newSelection }, Cmd.none)
      Width value ->
        let
          newSelection = { selection | width = Result.withDefault selection.width (toInt value) }
        in
          ({ model | selection = newSelection }, Cmd.none)
      Height value ->
        let
          newSelection = { selection | height = Result.withDefault selection.height (toInt value) }
        in
          ({ model | selection = newSelection }, Cmd.none)
      DragStart position ->
        let
          newDrag = Just position
        in
          ({ model | drag = newDrag }, Cmd.none)
      DragAt position ->
        let
          drag = Maybe.withDefault { x = 0, y = 0 } model.drag
          x = model.selection.x + position.x - drag.x
          y = model.selection.y + position.y - drag.y
          newSelection =
            { selection | x = x, y = y }
          newDrag = Just position
        in
          ({ model | selection = newSelection, drag = newDrag }, Cmd.none)
      DragEnd position ->
        ({ model | drag = Nothing }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]
