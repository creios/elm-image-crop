import Html exposing (..)
import Html.App
import Html.Attributes exposing (style, src, width, height, value, type')
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

-- Model
  
type alias Model =
  { imageSize : Size
  , selection : Area
  , drag : Maybe Drag
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

type alias Drag =
  { start : Mouse.Position
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
    , drag = Nothing
    }
  , Cmd.none
  )

-- Update

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
    {imageSize,selection,drag} = model
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
      DragStart xy ->
        ({ model | drag = Just { start = xy, current = xy } }, Cmd.none)
      DragAt xy ->
        ({ model | drag = (Maybe.map (\{start} -> Drag start xy) drag) }, Cmd.none)
      DragEnd _ ->
        let
          {x,y} = getPosition model
          newSelection = { selection | x = x, y = y }
        in
        ({ model | selection = newSelection, drag = Nothing }, Cmd.none)

getPosition : Model -> Point
getPosition model =
  case model.drag of
    Nothing ->
      { x = model.selection.x, y = model.selection.y }
    
    Just drag ->
      let
        x = model.selection.x + drag.current.x - drag.start.x
            |> max 0
            |> min (model.imageSize.width - model.selection.width)
    
        y = model.selection.y + drag.current.y - drag.start.y
            |> max 0
            |> min (model.imageSize.height - model.selection.height)
      in
        { x = x, y = y }
      

-- Subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.drag of
    Nothing ->
      Sub.none

    Just _ ->
      Sub.batch [ Mouse.moves DragAt, Mouse.ups DragEnd ]

-- View

view : Model -> Html Msg
view model =
  div
    []
    [ placeholdit model.imageSize
    , div
        [ selectionStyle model
        , on "mousedown" (Json.map DragStart Mouse.position)
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
    {x,y} = getPosition model
  in
    style
      [ ("position", "absolute")
      , ("left", px x)
      , ("top", px y)
      , ("width", px model.selection.width)
      , ("height", px model.selection.height)
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

type HandlePosition
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
