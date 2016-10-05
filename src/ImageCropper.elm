module ImageCropper
    exposing
        ( Model
        , Point
        , Size
        , Rectangle
        , Msg
        , init
        , update
        , view
        , subscriptions
        )

import Html exposing (..)
import Html.App
import Html.Attributes exposing (style)
import Html.Events exposing (onWithOptions)
import Mouse exposing (Position)
import Json.Decode as Json


-- Model


type alias Model =
    { image : Size
    , selection : Maybe Rectangle
    , move : Maybe Move
    , resize : Maybe Resize
    , select : Maybe Select
    }


type alias Point =
    { x : Int
    , y : Int
    }


type alias Size =
    { width : Int
    , height : Int
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
    , originalSelection : Rectangle
    }


type alias Resize =
    { direction : Direction
    , start : Mouse.Position
    , originalSelection : Rectangle
    }


type alias Select =
    { start : Mouse.Position
    }


init : Size -> Maybe Rectangle -> Model
init image selection =
    { image = image
    , selection = selection
    , move = Nothing
    , resize = Nothing
    , select = Nothing
    }



-- Update


type Msg
    = MoveStart Mouse.Position
    | MoveAt Mouse.Position
    | MoveEnd Mouse.Position
    | ResizeStart Direction Mouse.Position
    | ResizeAt Mouse.Position
    | ResizeEnd Mouse.Position
    | SelectStart Mouse.Position
    | SelectAt Mouse.Position
    | SelectEnd Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelper msg model, Cmd.none )


updateHelper : Msg -> Model -> Model
updateHelper msg model =
    case msg of
        MoveStart xy ->
            case model.selection of
                Just selection ->
                    let
                        move =
                            { start = xy
                            , originalSelection = selection
                            }
                    in
                        { model | move = Just move }

                Nothing ->
                    model

        MoveAt xy ->
            case model.move of
                Just move ->
                    let
                        selection =
                            moveSelection model.image move xy
                    in
                        { model | selection = Just selection }

                Nothing ->
                    model

        MoveEnd _ ->
            { model | move = Nothing }

        ResizeStart direction xy ->
            case model.selection of
                Just selection ->
                    let
                        resize =
                            { direction = direction
                            , start = xy
                            , originalSelection = selection
                            }
                    in
                        { model | resize = Just resize }

                Nothing ->
                    model

        ResizeAt xy ->
            case model.resize of
                Just resize ->
                    let
                        selection =
                            resizeSelection model.image resize xy
                    in
                        { model | selection = Just selection }

                Nothing ->
                    model

        ResizeEnd _ ->
            { model | resize = Nothing }

        SelectStart xy ->
            { model | selection = Nothing, select = Just { start = xy } }

        SelectAt xy ->
            case model.select of
                Just select ->
                    let
                        selection =
                            createSelection model.image select xy
                    in
                        { model | selection = selection }

                Nothing ->
                    model

        SelectEnd _ ->
            { model | select = Nothing }


atLeast : comparable -> comparable -> comparable
atLeast =
    max


atMost : comparable -> comparable -> comparable
atMost =
    min


moveSelection : Size -> Move -> Mouse.Position -> Rectangle
moveSelection image move current =
    let
        selection =
            move.originalSelection

        movement =
            { horizontal =
                current.x
                    - move.start.x
                    |> atLeast (-selection.topLeft.x)
                    |> atMost (image.width - selection.bottomRight.x)
            , vertical =
                current.y
                    - move.start.y
                    |> atLeast (-selection.topLeft.y)
                    |> atMost (image.height - selection.bottomRight.y)
            }
    in
        moveRectangle movement move.originalSelection


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


resizeSelection : Size -> Resize -> Mouse.Position -> Rectangle
resizeSelection image resize current =
    let
        movement =
            { horizontal = current.x - resize.start.x
            , vertical = current.y - resize.start.y
            }

        { topLeft, bottomRight } =
            resize.originalSelection

        { width, height } =
            rectangleSize resize.originalSelection

        topLeftX =
            if List.member resize.direction [ NorthWest, West, SouthWest ] then
                (topLeft.x + movement.horizontal)
                    |> atLeast 0
                    |> atMost (topLeft.x + width)
            else
                topLeft.x

        topLeftY =
            if List.member resize.direction [ NorthWest, North, NorthEast ] then
                (topLeft.y + movement.vertical)
                    |> atLeast 0
                    |> atMost (topLeft.y + height)
            else
                topLeft.y

        bottomRightX =
            if List.member resize.direction [ NorthEast, East, SouthEast ] then
                (bottomRight.x + movement.horizontal)
                    |> atLeast topLeft.x
                    |> atMost image.width
            else
                bottomRight.x

        bottomRightY =
            if List.member resize.direction [ SouthWest, South, SouthEast ] then
                (bottomRight.y + movement.vertical)
                    |> atLeast topLeft.y
                    |> atMost image.height
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
rectangleSize { topLeft, bottomRight } =
    { width = bottomRight.x - topLeft.x
    , height = bottomRight.y - topLeft.y
    }


createSelection : Size -> Select -> Mouse.Position -> Maybe Rectangle
createSelection image select xy =
    if select.start == xy then
        Nothing
    else
        let
            selection =
                { topLeft =
                    { x = min select.start.x xy.x |> atLeast 0
                    , y = min select.start.y xy.y |> atLeast 0
                    }
                , bottomRight =
                    { x = max select.start.x xy.x |> atMost image.width
                    , y = max select.start.y xy.y |> atMost image.height
                    }
                }
        in
            Just selection



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        moveSubscriptions =
            case model.move of
                Just _ ->
                    [ Mouse.moves MoveAt, Mouse.ups MoveEnd ]

                Nothing ->
                    []

        resizeSubscriptions =
            case model.resize of
                Just _ ->
                    [ Mouse.moves ResizeAt, Mouse.ups ResizeEnd ]

                Nothing ->
                    []

        selectSubscriptions =
            case model.select of
                Just _ ->
                    [ Mouse.moves SelectAt, Mouse.ups SelectEnd ]

                Nothing ->
                    []
    in
        Sub.batch (moveSubscriptions ++ resizeSubscriptions ++ selectSubscriptions)



-- View


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "top", "0" )
            , ( "width", px model.image.width )
            , ( "height", px model.image.height )
            ]
        ]
        (selectionView model)


selectionView : Model -> List (Html Msg)
selectionView model =
    case model.selection of
        Just selection ->
            [ div
                [ selectionStyle selection
                , onMouseDown MoveStart
                ]
                (borders ++ dragbars ++ handles)
            , shadow
                [ ( "left", "0" )
                , ( "top", "0" )
                , ( "width", px (selection.topLeft.x - 1 |> atLeast 0) )
                , ( "height", "100%" )
                ]
            , shadow
                [ ( "right", "0" )
                , ( "top", "0" )
                , ( "width", px (model.image.width - selection.bottomRight.x - 1 |> atLeast 0) )
                , ( "height", "100%" )
                ]
            , shadow
                [ ( "left", px (selection.topLeft.x - 1) )
                , ( "top", "0" )
                , ( "width", px ((rectangleSize selection).width + 2) )
                , ( "height", px (selection.topLeft.y - 1 |> atLeast 0) )
                ]
            , shadow
                [ ( "left", px (selection.topLeft.x - 1) )
                , ( "bottom", "0" )
                , ( "width", px ((rectangleSize selection).width + 2) )
                , ( "height", px (model.image.height - selection.bottomRight.y - 1 |> atLeast 0) )
                ]
            ]

        Nothing ->
            [ shadow
                [ ( "left", "0" )
                , ( "top", "0" )
                , ( "width", px model.image.width )
                , ( "height", px model.image.height )
                ]
            ]


selectionStyle : Rectangle -> Attribute Msg
selectionStyle selection =
    let
        { x, y } =
            selection.topLeft

        { width, height } =
            rectangleSize selection
    in
        style
            [ ( "position", "absolute" )
            , ( "left", px x )
            , ( "top", px y )
            , ( "width", px width )
            , ( "height", px height )
            , ( "cursor", "move" )
            , ( "z-index", "100" )
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
        ( cssPosition, orientation ) =
            positionCssHelper position
    in
        div
            [ style
                [ ( "position", "absolute" )
                , ( cssPosition, "-1px" )
                , ( "width"
                  , if orientation == Horizontal then
                        "100%"
                    else
                        "1px"
                  )
                , ( "height"
                  , if orientation == Vertical then
                        "100%"
                    else
                        "1px"
                  )
                , ( "overflow", "hidden" )
                , ( "background", "#fff url(http://jcrop.org/css/Jcrop.gif)" )
                ]
            ]
            []


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
        ( cssPosition, orientation ) =
            positionCssHelper position

        ( direction, cursor ) =
            case position of
                PositionTop ->
                    ( North, "n" )

                PositionRight ->
                    ( East, "e" )

                PositionBottom ->
                    ( South, "s" )

                PositionLeft ->
                    ( West, "w" )
    in
        div
            [ onMouseDown (ResizeStart direction)
            , style
                [ ( "position", "absolute" )
                , ( "width"
                  , if orientation == Horizontal then
                        "100%"
                    else
                        "9px"
                  )
                , ( "height"
                  , if orientation == Vertical then
                        "100%"
                    else
                        "9px"
                  )
                , ( cssPosition, "0" )
                , ( "margin-" ++ cssPosition, "-5px" )
                , ( "cursor", cursor ++ "-resize" )
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
        ( horizontalPosition, horizontalSpacing ) =
            if List.member orientation [ NorthWest, SouthWest, West ] then
                ( "left", "0" )
            else if List.member orientation [ North, South ] then
                ( "left", "50%" )
            else
                ( "right", "0" )

        ( verticalPosition, verticalSpacing ) =
            if List.member orientation [ NorthWest, North, NorthEast ] then
                ( "top", "0" )
            else if List.member orientation [ East, West ] then
                ( "top", "50%" )
            else
                ( "bottom", "0" )

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
                [ ( "background-color", "rgba(49,28,28,0.58)" )
                , ( "border", "1px #eee solid" )
                , ( "width", "9px" )
                , ( "height", "9px" )
                , ( "position", "absolute" )
                , ( "opacity", "0.8" )
                , ( horizontalPosition, horizontalSpacing )
                , ( "margin-" ++ horizontalPosition, "-6px" )
                , ( verticalPosition, verticalSpacing )
                , ( "margin-" ++ verticalPosition, "-6px" )
                , ( "cursor", cursor ++ "-resize" )
                ]
            ]
            []


shadow : List ( String, String ) -> Html Msg
shadow positioning =
    let
        styles =
            [ ( "background-color", "#000000" )
            , ( "opacity", "0.5" )
            , ( "position", "absolute" )
            ]
    in
        div
            [ style (styles ++ positioning)
            , onMouseDown SelectStart
            ]
            []


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


positionCssHelper : Position -> ( String, Orientation )
positionCssHelper position =
    case position of
        PositionTop ->
            ( "top", Horizontal )

        PositionRight ->
            ( "right", Vertical )

        PositionBottom ->
            ( "bottom", Horizontal )

        PositionLeft ->
            ( "left", Vertical )


onMouseDown : (Mouse.Position -> Msg) -> Attribute Msg
onMouseDown msg =
    onWithOptions
        "mousedown"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.map msg Mouse.position)
