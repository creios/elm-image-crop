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
  , selection : Rectangle
  , originalSelection : Maybe Rectangle
  , move : Maybe Move
  , resize : Maybe Resize
  }

type alias Size =
  { width : Int
  , height : Int
  }

type alias Point =
  { x : Int
  , y : Int
  }

type alias Rectangle =
  { topLeft : Point
  , bottomRight : Point
  }

type alias Movement =
  { horizontal : Int
  , vertical : Int
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

init : (Model, Cmd Msg)
init =
  ( { imageSize =
        { width = 400
        , height = 200
        }
    , selection =
        { topLeft =
            { x = 20
            , y = 10
            }
        , bottomRight =
            { x = 140
            , y = 80
            }
        }
    , originalSelection = Nothing
    , move = Nothing
    , resize = Nothing
    }
  , Cmd.none
  )

-- Update

type Msg
  = TopLeftX String
  | TopLeftY String
  | BottomRightX String
  | BottomRightY String
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
    {imageSize,selection,originalSelection,move,resize} = model
    {topLeft, bottomRight} = selection
  in
    case msg of
      TopLeftX value ->
        let
          newTopLeft =
            { topLeft | x = Result.withDefault topLeft.x (toInt value) }

          newSelection =
            { selection | topLeft = newTopLeft  }
        in
          { model | selection = newSelection }

      TopLeftY value ->
        let
          newTopLeft =
            { topLeft | y = Result.withDefault topLeft.y (toInt value) }

          newSelection =
            { selection | topLeft = newTopLeft }
        in
          { model | selection = newSelection }

      BottomRightX value ->
        let
          newBottomRight =
            { bottomRight | x = Result.withDefault bottomRight.x (toInt value) }

          newSelection =
            { selection | bottomRight = newBottomRight  }
        in
          { model | selection = newSelection }

      BottomRightY value ->
        let
          newBottomRight =
            { bottomRight | y = Result.withDefault bottomRight.y (toInt value) }

          newSelection =
            { selection | bottomRight = newBottomRight  }
        in
          { model | selection = newSelection }

      MoveStart xy ->
        let
          move = { start = xy, current = xy }
        in
          { model | move = Just move, originalSelection = Just selection }

      MoveAt xy ->
        let
          updateCurrent move = { move | current = xy }
          moveUpdated = { model | move = (Maybe.map updateCurrent move) }
          -- Silly default value
          baseSelection = Maybe.withDefault selection originalSelection
        in
          { moveUpdated | selection = (applyMove moveUpdated baseSelection) }

      MoveEnd _ ->
        { model | originalSelection = Nothing, move = Nothing }

      ResizeStart direction xy ->
        let
          resize =
            { direction = direction
            , start = xy
            , current = xy
            }
        in
          { model | resize = Just resize, originalSelection = Just selection }

      ResizeAt xy ->
        let
          updateCurrent resize = { resize | current = xy }
          resizeUpdated = { model | resize = (Maybe.map updateCurrent resize) }
          -- Silly default value
          baseSelection = Maybe.withDefault selection originalSelection
        in
          { resizeUpdated | selection = (applyResize resizeUpdated baseSelection) }

      ResizeEnd xy ->
        { model | originalSelection = Nothing, resize = Nothing }

atLeast : comparable -> comparable -> comparable
atLeast = max

atMost : comparable -> comparable -> comparable
atMost = min

applyMove : Model -> Rectangle -> Rectangle
applyMove model selection =
  case model.move of
    Nothing ->
      selection

    Just move ->
      let
        movement =
          { horizontal =
              move.current.x - move.start.x
              |> atLeast (-selection.topLeft.x)
              |> atMost (model.imageSize.width - selection.bottomRight.x)

          , vertical =
              move.current.y - move.start.y
              |> atLeast (-selection.topLeft.y)
              |> atMost (model.imageSize.height - selection.bottomRight.y)
          }
      in
         moveRectangle movement selection

moveRectangle : Movement -> Rectangle -> Rectangle
moveRectangle movement rectangle =
  { topLeft = movePoint movement rectangle.topLeft
  , bottomRight = movePoint movement rectangle.bottomRight
  }
    
movePoint : Movement -> Point -> Point
movePoint movement point =
  { x = point.x + movement.horizontal
  , y = point.y + movement.vertical
  }

applyResize : Model -> Rectangle -> Rectangle
applyResize model selection =
  case model.resize of
    Nothing ->
      selection

    Just resize ->
      let

        horizontalMovement = resize.current.x - resize.start.x
        verticalMovement = resize.current.y - resize.start.y

        {topLeft,bottomRight} = selection
        {width,height} = rectangleSize selection

        topLeftX =
          if List.member resize.direction [NorthWest, West, SouthWest] then
            (topLeft.x + horizontalMovement)
              |> atLeast 0
              |> atMost (topLeft.x + width)
          else
            topLeft.x

        topLeftY =
          if List.member resize.direction [NorthWest, North, NorthEast] then
            (topLeft.y + verticalMovement)
              |> atLeast 0
              |> atMost (topLeft.y + height)
          else
            topLeft.y

        bottomRightX =
          if List.member resize.direction [NorthEast, East, SouthEast] then
            (bottomRight.x + horizontalMovement)
              |> atLeast topLeft.x
              |> atMost (model.imageSize.width)
          else
            bottomRight.x

        bottomRightY =
          if List.member resize.direction [SouthWest, South, SouthEast] then
            (bottomRight.y + verticalMovement)
              |> atLeast topLeft.y
              |> atMost (model.imageSize.height)
          else
            bottomRight.y

      in
        { topLeft =
            { x = topLeftX
            , y = topLeftY
            }
        , bottomRight =
            { x = bottomRightX
            , y = bottomRightY
            }
        }

rectangleSize : Rectangle -> Size
rectangleSize {topLeft,bottomRight} =
  { width = bottomRight.x - topLeft.x
  , height = bottomRight.y - topLeft.y
  }

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
    selection = model.selection
    {x,y} = selection.topLeft
    {width,height} = rectangleSize selection
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

debugForm : Rectangle -> Html Msg
debugForm selection =
  Html.form
    []
    [ text "("
    , input
      [ type' "number"
      , value (toString selection.topLeft.x)
      , onInput TopLeftX
      ] []
    , text "|"
    , input
      [ type' "number"
      , value (toString selection.topLeft.y)
      , onInput TopLeftY
      ] []
    , text "), ("
    , input
      [ type' "number"
      , value (toString selection.bottomRight.x)
      , onInput BottomRightX
      ] []
    , text "|"
    , input
      [ type' "number"
      , value (toString selection.bottomRight.y)
      , onInput BottomRightY
      ] []
    , text ")"
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
