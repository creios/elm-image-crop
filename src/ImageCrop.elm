module ImageCrop
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
import Json.Decode as Json exposing ((:=))


-- Model


type alias Model =
    { image : Size
    , cropAreaWidth : Int
    , offset : Point
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


init : Size -> Int -> Point -> Maybe Rectangle -> Model
init image cropAreaWidth offset selection =
    { image = image
    , cropAreaWidth = cropAreaWidth
    , offset = offset
    , selection = selection
    , move = Nothing
    , resize = Nothing
    , select = Nothing
    }



-- Update


type Msg
    = MoveStart MouseButtonEvent
    | MoveAt Mouse.Position
    | MoveEnd Mouse.Position
    | ResizeStart Direction MouseButtonEvent
    | ResizeAt Mouse.Position
    | ResizeEnd Mouse.Position
    | SelectStart MouseButtonEvent
    | SelectAt Mouse.Position
    | SelectEnd Mouse.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( updateHelper msg model, Cmd.none )


updateHelper : Msg -> Model -> Model
updateHelper msg model =
    case msg of
        MoveStart event ->
            if event.button == 0 then
                case model.selection of
                    Just selection ->
                        let
                            move =
                                { start = event.position
                                , originalSelection = selection
                                }
                        in
                            { model | move = Just move }

                    Nothing ->
                        model
            else
                model

        MoveAt xy ->
            case model.move of
                Just move ->
                    let
                        selection =
                            moveSelection model.image model.cropAreaWidth move xy
                    in
                        { model | selection = Just selection }

                Nothing ->
                    model

        MoveEnd _ ->
            { model | move = Nothing }

        ResizeStart direction event ->
            if event.button == 0 then
                case model.selection of
                    Just selection ->
                        let
                            resize =
                                { direction = direction
                                , start = event.position
                                , originalSelection = selection
                                }
                        in
                            { model | resize = Just resize }

                    Nothing ->
                        model
            else
                model

        ResizeAt xy ->
            case model.resize of
                Just resize ->
                    let
                        selection =
                            resizeSelection model.image model.cropAreaWidth resize xy
                    in
                        { model | selection = Just selection }

                Nothing ->
                    model

        ResizeEnd _ ->
            { model | resize = Nothing }

        SelectStart event ->
            if event.button == 0 then
                let
                    select =
                        Just { start = event.position }
                in
                    { model | selection = Nothing, select = select }
            else
                model

        SelectAt xy ->
            case model.select of
                Just select ->
                    let
                        selection =
                            createSelection select model xy
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


moveSelection : Size -> Int -> Move -> Mouse.Position -> Rectangle
moveSelection image cropAreaWidth move current =
    let
        selection =
            move.originalSelection

        factor =
            toFloat image.width / toFloat cropAreaWidth

        scale value =
            round (toFloat value * factor)

        movement =
            { horizontal =
                current.x
                    - move.start.x
                    |> scale
                    |> atLeast (-selection.topLeft.x)
                    |> atMost (image.width - selection.bottomRight.x)
            , vertical =
                current.y
                    - move.start.y
                    |> scale
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


resizeSelection : Size -> Int -> Resize -> Mouse.Position -> Rectangle
resizeSelection image cropAreaWidth resize current =
    let
        factor =
            toFloat image.width / toFloat cropAreaWidth

        scale value =
            round (toFloat value * factor)

        movement =
            { horizontal = current.x - resize.start.x |> scale
            , vertical = current.y - resize.start.y |> scale
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


createSelection : Select -> Model -> Mouse.Position -> Maybe Rectangle
createSelection select model xy =
    if select.start == xy then
        Nothing
    else
        let
            factor =
                toFloat model.image.width / toFloat model.cropAreaWidth

            scale value =
                round (toFloat value * factor)

            normalizePoint point =
                { x = point.x - model.offset.x
                , y = point.y - model.offset.y
                }

            normalizedStart =
                normalizePoint select.start

            normalizedXy =
                normalizePoint xy

            selection =
                { topLeft =
                    { x = min normalizedStart.x normalizedXy.x |> scale |> atLeast 0
                    , y = min normalizedStart.y normalizedXy.y |> scale |> atLeast 0
                    }
                , bottomRight =
                    { x = max normalizedStart.x normalizedXy.x |> scale |> atMost model.image.width
                    , y = max normalizedStart.y normalizedXy.y |> scale |> atMost model.image.height
                    }
                }
        in
            Just selection


scaleRectangle : Float -> Rectangle -> Rectangle
scaleRectangle factor rectangle =
    { topLeft = scalePoint factor rectangle.topLeft
    , bottomRight = scalePoint factor rectangle.bottomRight
    }


scalePoint : Float -> Point -> Point
scalePoint factor point =
    { x = round (toFloat point.x * factor)
    , y = round (toFloat point.y * factor)
    }



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
    let
        cropArea =
            calculateCropArea model.image model.cropAreaWidth
    in
        div
            [ style
                [ ( "position", "absolute" )
                , ( "top", "0" )
                , ( "width", px cropArea.width )
                , ( "height", px cropArea.height )
                ]
            ]
            (selectionView model cropArea)


calculateCropArea : Size -> Int -> Size
calculateCropArea image cropAreaWidth =
    { width = cropAreaWidth
    , height = round (toFloat image.height / toFloat image.width * toFloat cropAreaWidth)
    }


selectionView : Model -> Size -> List (Html Msg)
selectionView model cropArea =
    case model.selection of
        Just selection ->
            let
                factor =
                    toFloat model.cropAreaWidth / toFloat model.image.width

                displaySelection =
                    scaleRectangle factor selection
            in
                [ div
                    [ selectionStyle displaySelection
                    , onMouseDown MoveStart
                    ]
                    (borders ++ dragbars ++ handles)
                , shadow
                    [ ( "left", "0" )
                    , ( "top", "0" )
                    , ( "width", px (displaySelection.topLeft.x - 1 |> atLeast 0) )
                    , ( "height", "100%" )
                    ]
                , shadow
                    [ ( "right", "0" )
                    , ( "top", "0" )
                    , ( "width", px (cropArea.width - displaySelection.bottomRight.x - 1 |> atLeast 0) )
                    , ( "height", "100%" )
                    ]
                , shadow
                    [ ( "left", px (displaySelection.topLeft.x - 1) )
                    , ( "top", "0" )
                    , ( "width", px ((rectangleSize displaySelection).width + 2) )
                    , ( "height", px (displaySelection.topLeft.y - 1 |> atLeast 0) )
                    ]
                , shadow
                    [ ( "left", px (displaySelection.topLeft.x - 1) )
                    , ( "bottom", "0" )
                    , ( "width", px ((rectangleSize displaySelection).width + 2) )
                    , ( "height", px (cropArea.height - displaySelection.bottomRight.y - 1 |> atLeast 0) )
                    ]
                ]

        Nothing ->
            [ shadow
                [ ( "left", "0" )
                , ( "top", "0" )
                , ( "width", px cropArea.width )
                , ( "height", px cropArea.height )
                ]
            ]


selectionStyle : Rectangle -> Attribute Msg
selectionStyle displaySelection =
    let
        { x, y } =
            displaySelection.topLeft

        { width, height } =
            rectangleSize displaySelection
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


onMouseDown : (MouseButtonEvent -> Msg) -> Attribute Msg
onMouseDown msg =
    onWithOptions
        "mousedown"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.map msg mouseEventDecoder)


type alias MouseButtonEvent =
    { position : Mouse.Position
    , button : Int
    }


mouseEventDecoder =
    Json.object3
        (\x y button -> MouseButtonEvent (Mouse.Position x y) button)
        ("pageX" := Json.int)
        ("pageY" := Json.int)
        ("button" := Json.int)
