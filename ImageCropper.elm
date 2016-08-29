import Html exposing (..)
import Html.App
import Html.Attributes exposing (style, src, width, height, value, type')
import Html.Events exposing (onInput, onWithOptions)
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

-- Model

type alias Model =
  { imageSize : Size
  , selection : Area
  , move : Maybe Move
  , resize : Maybe Resize
  }

type alias Size =
  { width : Int
  , height : Int
  }

type alias Area =
  { x : Int
  , y : Int
  , width : Int
  , height : Int
  }

type alias Move =
  { start : Mouse.Position
  , current : Mouse.Position
  }

type alias Resize =
  { direction : Direction
  , start : Mouse.Position
  , current : Mouse.Position
  }

type alias Point =
  { x : Int
  , y : Int
  }

init : (Model, Cmd Msg)
init =
  ( { imageSize =
        { width = 400
        , height = 200
        }
    , selection =
        { x = 20
        , y = 10
        , width = 120
        , height = 70
        }
    , move = Nothing
    , resize = Nothing
    }
  , Cmd.none
  )

-- Update

type Msg
  = Left String
  | Top String
  | Width String
  | Height String
  | MoveStart Mouse.Position
  | MoveAt Mouse.Position
  | MoveEnd Mouse.Position
  | ResizeStart Direction Mouse.Position
  | ResizeAt Mouse.Position
  | ResizeEnd Mouse.Position

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (updateHelper msg model, Cmd.none)

updateHelper : Msg -> Model -> Model
updateHelper msg model =
  let
    {imageSize,selection,move,resize} = model
  in
    case msg of
      Left value ->
        let
          newSelection = { selection | x = Result.withDefault selection.x (toInt value) }
        in
          { model | selection = newSelection }

      Top value ->
        let
          newSelection = { selection | y = Result.withDefault selection.y (toInt value) }
        in
          { model | selection = newSelection }

      Width value ->
        let
          newSelection = { selection | width = Result.withDefault selection.width (toInt value) }
        in
          { model | selection = newSelection }

      Height value ->
        let
          newSelection = { selection | height = Result.withDefault selection.height (toInt value) }
        in
          { model | selection = newSelection }

      MoveStart xy ->
        { model | move = Just { start = xy, current = xy } }

      MoveAt xy ->
        let
          updateCurrent =
            \move -> { move | current = xy }
        in
          { model | move = (Maybe.map updateCurrent move) }

      MoveEnd _ ->
        { model | selection = (applyMove model selection), move = Nothing }

      ResizeStart direction xy ->
        let
          resize =
            { direction = direction
            , start = xy
            , current = xy
            }
        in
          { model | resize = Just resize }

      ResizeAt xy ->
        let
          updateCurrent =
            \resize -> { resize | current = xy}
        in
          { model | resize = (Maybe.map updateCurrent resize) }

      ResizeEnd xy ->
        { model | selection = (applyResize model selection), resize = Nothing }

getSelection : Model -> Area
getSelection model =
  model.selection
  |> applyMove model
  |> applyResize model

applyMove : Model -> Area -> Area
applyMove model selection =
  case model.move of
    Nothing ->
      selection

    Just move ->
      let
        x = selection.x + move.current.x - move.start.x
            |> max 0
            |> min (model.imageSize.width - selection.width)

        y = selection.y + move.current.y - move.start.y
            |> max 0
            |> min (model.imageSize.height - selection.height)
      in
        { selection | x = x, y = y }

applyResize : Model -> Area -> Area
applyResize model selection =
  case model.resize of
    Nothing ->
      selection

    Just resize ->
      let

        horizontalMovement = resize.current.x - resize.start.x
        verticalMovement = resize.current.y - resize.start.y

        x =
          if List.member resize.direction [NorthWest, West, SouthWest] then
            (selection.x + horizontalMovement)
              |> max 0
              |> min (selection.x + selection.width)
          else
            selection.x

        y =
          if List.member resize.direction [NorthWest, North, NorthEast] then
            (selection.y + verticalMovement)
              |> max 0
              |> min (selection.y + selection.height)
          else
            selection.y

        width =
          if List.member resize.direction [SouthWest, West, NorthWest] then
            selection.width - horizontalMovement
              |> max 0
              |> min (selection.x + selection.width)

          else if List.member resize.direction [NorthEast, East, SouthEast] then
            selection.width + horizontalMovement
              |> max 0
              |> min (model.imageSize.width - selection.x)

          else
            selection.width

        height =
          if List.member resize.direction [NorthWest, North, NorthEast] then
            selection.height - verticalMovement
              |> max 0
              |> min (selection.y + selection.height)

          else if List.member resize.direction [SouthWest, South, SouthEast] then
            selection.height + verticalMovement
              |> max 0
              |> min (model.imageSize.height - selection.y)

          else
            selection.height
      in
        Debug.log "applyResize" (Area x y width height)

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  let
    moveSubscriptions =
      case model.move of
        Nothing ->
          []

        Just _ ->
          [ Mouse.moves MoveAt, Mouse.ups MoveEnd ]

    resizeSubscriptions =
      case model.resize of
        Nothing ->
          []

        Just _ ->
          [ Mouse.moves ResizeAt, Mouse.ups ResizeEnd ]
  in
    Sub.batch (moveSubscriptions ++ resizeSubscriptions)

-- View

view : Model -> Html Msg
view model =
  div
    []
    [ placeholdit model.imageSize
    , div
        [ selectionStyle model
        , onMouseDown MoveStart
        ]
        (borders ++ dragbars ++ handles)
    , debugForm model.selection
    ]

placeholdit : Size -> Html Msg
placeholdit size =
  img [ src ("https://placehold.it/" ++ toString size.width ++ "x" ++ toString size.height)
      , width size.width
      , height size.height
      ] []

selectionStyle : Model -> Attribute Msg
selectionStyle model =
  let
    {x,y,width,height} = getSelection model
  in
    style
      [ ("position", "absolute")
      , ("left", px x)
      , ("top", px y)
      , ("width", px width)
      , ("height", px height)
      , ("cursor", "move")
      ]

borders : List (Html Msg)
borders =
  List.map
    border
    [ PositionTop
    , PositionRight
    , PositionBottom
    , PositionLeft
    ]

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

dragbars : List (Html Msg)
dragbars =
  List.map
    dragbar
    [ PositionTop
    , PositionRight
    , PositionBottom
    , PositionLeft
    ]

dragbar : Position -> Html Msg
dragbar position =
  let
    (cssPosition, orientation) = positionCssHelper position

    (direction, cursor) =
      case position of
        PositionTop ->
          (North, "n")

        PositionRight ->
          (East, "e")

        PositionBottom ->
          (South, "s")

        PositionLeft ->
          (West, "w")
  in
    div
      [ onMouseDown (ResizeStart direction)
      , style
          [ ("position", "absolute")
          , ("width", if orientation == Horizontal then "100%" else "9px")
          , ("height", if orientation == Vertical then "100%" else "9px")
          , (cssPosition, "0")
          , ("margin-" ++ cssPosition, "-5px")
          , ("cursor", cursor ++ "-resize")
          ]
      ]
      []

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

handle : Direction -> Html Msg
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
      [ onMouseDown (ResizeStart orientation)
      , style
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

type Position
  = PositionTop
  | PositionRight
  | PositionBottom
  | PositionLeft

type Orientation
  = Horizontal
  | Vertical

type Direction
  = North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest

px : Int -> String
px value =
  toString value ++ "px"

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

onMouseDown : (Mouse.Position -> Msg) -> Attribute Msg
onMouseDown msg =
  onWithOptions
    "mousedown"
    { stopPropagation = True
    , preventDefault = True
    }
    (Json.map msg Mouse.position)
