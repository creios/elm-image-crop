module ImageCrop
    exposing
        ( Model
        , Point
        , Size
        , Rectangle
        , AspectRatio
        , Msg
        , Notification(..)
        , init
        , update
        , view
        , subscriptions
        , changeAspectRatio
        , receiveOffset
        )

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onWithOptions)
import Mouse exposing (Position)
import Json.Decode as Json exposing (field)
import ImageCrop.Internal as Internal exposing (..)


-- Model


type alias Model =
    { image : Size
    , cropAreaWidth : Int
    , selection : Maybe Rectangle
    , aspectRatio : Maybe AspectRatio
    , action : Action
    }


type Action
    = NoAction
    | Move MoveData
    | Resize ResizeData
    | WaitingForOffset WaitingForOffsetData
    | Select SelectData


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


type alias MoveData =
    { start : Mouse.Position
    , originalSelection : Rectangle
    }


type alias ResizeData =
    { direction : Direction
    , start : Mouse.Position
    , originalSelection : Rectangle
    }


type alias WaitingForOffsetData =
    { start : Mouse.Position
    }


type alias SelectData =
    { start : Mouse.Position
    , offset : Point
    }


type alias AspectRatio =
    { width : Float
    , height : Float
    }


init : Size -> Int -> Maybe Rectangle -> Maybe AspectRatio -> Model
init image cropAreaWidth selection aspectRatio =
    { image = image
    , cropAreaWidth = cropAreaWidth
    , selection = selection
    , aspectRatio = aspectRatio
    , action = NoAction
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


type Notification
    = NoNotification
    | SelectionChanged (Maybe Rectangle)
    | RequestOffset


update : Msg -> Model -> ( Model, Cmd Msg, Notification )
update msg model =
    let
        ( newModel, notification ) =
            updateHelper msg model
    in
        ( newModel, Cmd.none, notification )


updateHelper : Msg -> Model -> ( Model, Notification )
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
                            ( { model | action = Move move }, NoNotification )

                    Nothing ->
                        ( model, NoNotification )
            else
                ( model, NoNotification )

        MoveAt xy ->
            case model.action of
                Move move ->
                    let
                        selection =
                            Just (moveSelection model.image model.cropAreaWidth move xy)
                    in
                        ( { model | selection = selection }, SelectionChanged selection )

                _ ->
                    ( model, NoNotification )

        MoveEnd _ ->
            ( { model | action = NoAction }, NoNotification )

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
                            ( { model | action = Resize resize }, NoNotification )

                    Nothing ->
                        ( model, NoNotification )
            else
                ( model, NoNotification )

        ResizeAt xy ->
            case model.action of
                Resize resize ->
                    let
                        selection =
                            Just (resizeSelection model resize xy)
                    in
                        ( { model | selection = selection }, SelectionChanged selection )

                _ ->
                    ( model, NoNotification )

        ResizeEnd _ ->
            ( { model | action = NoAction }, NoNotification )

        SelectStart event ->
            if event.button == 0 then
                let
                    action =
                        WaitingForOffset { start = event.position }
                in
                    ( { model | selection = Nothing, action = action }, RequestOffset )
            else
                ( model, NoNotification )

        SelectAt xy ->
            case model.action of
                Select select ->
                    let
                        selection =
                            createSelection select model xy
                    in
                        ( { model | selection = selection }, SelectionChanged selection )

                _ ->
                    ( model, NoNotification )

        SelectEnd _ ->
            ( { model | action = NoAction }, NoNotification )


atLeast : comparable -> comparable -> comparable
atLeast =
    max


atMost : comparable -> comparable -> comparable
atMost =
    min


moveSelection : Size -> Int -> MoveData -> Mouse.Position -> Rectangle
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


normalizeEdges : Size -> Point -> Point -> Rectangle
normalizeEdges canvas first second =
    let
        ordered =
            orderEdges first second
    in
        { topLeft =
            { x = ordered.topLeft.x |> atLeast 0
            , y = ordered.topLeft.y |> atLeast 0
            }
        , bottomRight =
            { x = ordered.bottomRight.x |> atMost canvas.width
            , y = ordered.bottomRight.y |> atMost canvas.height
            }
        }


resizeSelection : Model -> ResizeData -> Mouse.Position -> Rectangle
resizeSelection model resize position =
    let
        { topLeft, bottomRight } =
            resize.originalSelection

        movement =
            { horizontal = position.x - resize.start.x
            , vertical = position.y - resize.start.y
            }

        factor =
            toFloat model.image.width / toFloat model.cropAreaWidth

        scale value =
            round (toFloat value * factor)

        scaleMovement movement =
            { horizontal = scale movement.horizontal
            , vertical = scale movement.vertical
            }

        normalizedMovement =
            scaleMovement movement

        startingPointX =
            if List.member resize.direction [ NorthWest, West, SouthWest ] then
                resize.originalSelection.topLeft.x
            else if List.member resize.direction [ NorthEast, East, SouthEast ] then
                resize.originalSelection.bottomRight.x
            else
                round (toFloat (resize.originalSelection.bottomRight.x - resize.originalSelection.topLeft.x) / 2)

        startingPointY =
            if List.member resize.direction [ NorthWest, North, NorthEast ] then
                resize.originalSelection.topLeft.y
            else if List.member resize.direction [ SouthWest, South, SouthEast ] then
                resize.originalSelection.bottomRight.y
            else
                round (toFloat (resize.originalSelection.bottomRight.y - resize.originalSelection.topLeft.y) / 2)

        startingPoint =
            { x = startingPointX
            , y = startingPointY
            }

        normalizedPosition =
            movePoint normalizedMovement startingPoint
    in
        case model.aspectRatio of
            Just aspectRatio ->
                let
                    anchor =
                        calculateAnchor resize
                in
                    createAspectRatioSelection
                        model.image
                        aspectRatio
                        (generalizeDirection resize.direction)
                        anchor
                        normalizedPosition

            Nothing ->
                let
                    firstX =
                        if List.member resize.direction [ NorthEast, East, SouthEast, South ] then
                            topLeft.x
                        else
                            bottomRight.x

                    firstY =
                        if List.member resize.direction [ East, SouthEast, South, SouthWest ] then
                            topLeft.y
                        else
                            bottomRight.y

                    secondX =
                        if resize.direction == South then
                            bottomRight.x
                        else if resize.direction == North then
                            topLeft.x
                        else
                            normalizedPosition.x

                    secondY =
                        if resize.direction == East then
                            bottomRight.y
                        else if resize.direction == West then
                            topLeft.y
                        else
                            normalizedPosition.y

                    first =
                        Point firstX firstY

                    second =
                        Point secondX secondY
                in
                    normalizeEdges model.image first second


type GeneralDirection
    = HorizontalDirection
    | VerticalDirection
    | DiagonalDirection


generalizeDirection : Direction -> GeneralDirection
generalizeDirection direction =
    if List.member direction [ West, East ] then
        HorizontalDirection
    else if List.member direction [ North, South ] then
        VerticalDirection
    else
        DiagonalDirection


createAspectRatioSelection image aspectRatio generalDirection anchor position =
    let
        mouseRectangle =
            createResizeRectangleFromMouse
                image
                generalDirection
                aspectRatio
                anchor
                position

        edgeConstraints =
            createMaxRectangles
                generalDirection
                image
                aspectRatio
                anchor
                position
    in
        List.foldr
            (minBy rectangleArea)
            mouseRectangle
            edgeConstraints


calculateAnchor : ResizeData -> Point
calculateAnchor { direction, originalSelection } =
    let
        { topLeft, bottomRight } =
            originalSelection

        anchorX =
            if List.member direction [ NorthEast, East, SouthEast ] then
                topLeft.x
            else if List.member direction [ North, South ] then
                round (toFloat (topLeft.x + bottomRight.x) / 2)
            else
                bottomRight.x

        anchorY =
            if List.member direction [ SouthWest, South, SouthEast ] then
                topLeft.y
            else if List.member direction [ West, East ] then
                round (toFloat (topLeft.y + bottomRight.y) / 2)
            else
                bottomRight.y
    in
        Point anchorX anchorY


createResizeRectangleFromMouse : Size -> GeneralDirection -> AspectRatio -> Point -> Point -> Rectangle
createResizeRectangleFromMouse image generalDirection aspectRatio anchor position =
    case generalDirection of
        HorizontalDirection ->
            let
                x =
                    position.x
                        |> atLeast 0
                        |> atMost image.width

                width =
                    abs (x - anchor.x)

                height =
                    heightFromWidth aspectRatio width

                first =
                    { x = anchor.x
                    , y = anchor.y - round (toFloat height / 2)
                    }

                second =
                    { x = x
                    , y = anchor.y + round (toFloat height / 2)
                    }
            in
                orderEdges first second

        VerticalDirection ->
            let
                y =
                    position.y
                        |> atLeast 0
                        |> atMost image.height

                height =
                    abs (y - anchor.y)

                width =
                    widthFromHeight aspectRatio height

                first =
                    { x = anchor.x - round (toFloat width / 2)
                    , y = anchor.y
                    }

                second =
                    { x = anchor.x + round (toFloat width / 2)
                    , y = y
                    }
            in
                orderEdges first second

        DiagonalDirection ->
            createSelectionFromMouse image aspectRatio anchor position


createSelectionFromMouse image aspectRatio anchor position =
    let
        horizontallyAlignedRectangle =
            let
                width =
                    abs (position.x - anchor.x)

                factor =
                    if position.y < anchor.y then
                        -1
                    else
                        1

                height =
                    factor * heightFromWidth aspectRatio width

                target =
                    { x = position.x
                    , y = anchor.y + height
                    }
            in
                orderEdges anchor target

        verticallyAlignedRectangle =
            let
                height =
                    abs (position.y - anchor.y)

                factor =
                    if position.x < anchor.x then
                        -1
                    else
                        1

                width =
                    factor * widthFromHeight aspectRatio height

                target =
                    { x = anchor.x + width
                    , y = position.y
                    }
            in
                orderEdges anchor target
    in
        maxBy
            -- You can use minBy here instead to alter the behaviour
            rectangleArea
            horizontallyAlignedRectangle
            verticallyAlignedRectangle


createMaxRectangles generalDirection image aspectRatio anchor position =
    case generalDirection of
        HorizontalDirection ->
            let
                topBoundaryRectangle =
                    let
                        height =
                            anchor.y * 2

                        factor =
                            if position.x < anchor.x then
                                -1
                            else
                                1

                        width =
                            factor * widthFromHeight aspectRatio height

                        first =
                            { x = anchor.x
                            , y = 0
                            }

                        second =
                            { x = anchor.x + width
                            , y = height
                            }
                    in
                        orderEdges first second

                bottomBoundaryRectangle =
                    let
                        height =
                            (image.height - anchor.y) * 2

                        factor =
                            if position.x < anchor.x then
                                -1
                            else
                                1

                        width =
                            factor * widthFromHeight aspectRatio height

                        first =
                            { x = anchor.x
                            , y = image.height - height
                            }

                        second =
                            { x = anchor.x + width
                            , y = image.height
                            }
                    in
                        orderEdges first second
            in
                [ topBoundaryRectangle, bottomBoundaryRectangle ]

        VerticalDirection ->
            let
                leftBoundaryRectangle =
                    let
                        width =
                            anchor.x * 2

                        factor =
                            if position.y < anchor.y then
                                -1
                            else
                                1

                        height =
                            factor * heightFromWidth aspectRatio width

                        first =
                            { x = 0
                            , y = anchor.y
                            }

                        second =
                            { x = width
                            , y = anchor.y + height
                            }
                    in
                        orderEdges first second

                rightBoundaryRectangle =
                    let
                        width =
                            (image.width - anchor.x) * 2

                        factor =
                            if position.y < anchor.y then
                                -1
                            else
                                1

                        height =
                            factor * heightFromWidth aspectRatio width

                        first =
                            { x = image.width - width
                            , y = anchor.y
                            }

                        second =
                            { x = image.width
                            , y = anchor.y + height
                            }
                    in
                        orderEdges first second
            in
                [ leftBoundaryRectangle, rightBoundaryRectangle ]

        DiagonalDirection ->
            let
                horizontalBoundary =
                    let
                        boundaryValue =
                            if position.x < anchor.x then
                                0
                            else
                                image.width

                        width =
                            abs (boundaryValue - anchor.x)

                        factor =
                            if position.y < anchor.y then
                                -1
                            else
                                1

                        height =
                            factor * heightFromWidth aspectRatio width

                        target =
                            { x = boundaryValue
                            , y = anchor.y + height
                            }
                    in
                        orderEdges anchor target

                verticalBoundary =
                    let
                        boundaryValue =
                            if position.y < anchor.y then
                                0
                            else
                                image.height

                        height =
                            abs (boundaryValue - anchor.y)

                        factor =
                            if position.x < anchor.x then
                                -1
                            else
                                1

                        width =
                            factor * widthFromHeight aspectRatio height

                        target =
                            { x = anchor.x + width
                            , y = boundaryValue
                            }
                    in
                        orderEdges anchor target
            in
                [ horizontalBoundary, verticalBoundary ]


widthFromHeight aspectRatio height =
    round (toFloat height / aspectRatio.height * aspectRatio.width)


heightFromWidth aspectRatio width =
    round (toFloat width / aspectRatio.width * aspectRatio.height)


orderEdges first second =
    { topLeft =
        { x = min first.x second.x
        , y = min first.y second.y
        }
    , bottomRight =
        { x = max first.x second.x
        , y = max first.y second.y
        }
    }


rectangleSize : Rectangle -> Size
rectangleSize { topLeft, bottomRight } =
    { width = bottomRight.x - topLeft.x
    , height = bottomRight.y - topLeft.y
    }


createSelection : SelectData -> Model -> Mouse.Position -> Maybe Rectangle
createSelection select model position =
    if select.start == position then
        Nothing
    else
        let
            relativeCoordinates point =
                { x = point.x - select.offset.x
                , y = point.y - select.offset.y
                }

            factor =
                toFloat model.image.width / toFloat model.cropAreaWidth

            scale value =
                round (toFloat value * factor)

            scalePoint point =
                { x = scale point.x
                , y = scale point.y
                }

            normalizedStart =
                scalePoint (relativeCoordinates select.start)

            normalizedPosition =
                scalePoint (relativeCoordinates position)

            selection =
                case model.aspectRatio of
                    Just aspectRatio ->
                        createAspectRatioSelection
                            model.image
                            aspectRatio
                            DiagonalDirection
                            normalizedStart
                            normalizedPosition

                    Nothing ->
                        normalizeEdges model.image normalizedStart normalizedPosition
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


changeAspectRatio : Maybe AspectRatio -> Model -> Model
changeAspectRatio maybeAspectRatio model =
    let
        selectionUpdated =
            case maybeAspectRatio of
                Just aspectRatio ->
                    case model.selection of
                        Just selection ->
                            let
                                newSelection =
                                    recalculateSelection model.image aspectRatio selection
                            in
                                { model | selection = Just newSelection }

                        Nothing ->
                            model

                Nothing ->
                    model
    in
        { selectionUpdated | aspectRatio = maybeAspectRatio }


receiveOffset : Point -> Model -> Model
receiveOffset offset model =
    case model.action of
        WaitingForOffset { start } ->
            let
                action =
                    Select { start = start, offset = offset }
            in
                { model | action = action }

        _ ->
            model


recalculateSelection : Size -> AspectRatio -> Rectangle -> Rectangle
recalculateSelection image aspectRatio selection =
    let
        area =
            rectangleArea selection

        height =
            round (sqrt (toFloat area / (aspectRatio.width / aspectRatio.height)))

        width =
            round (sqrt (toFloat area * (aspectRatio.width / aspectRatio.height)))

        { topLeft } =
            selection

        newBottomRight =
            { x = topLeft.x + width
            , y = topLeft.y + height
            }
    in
        createAspectRatioSelection
            image
            aspectRatio
            DiagonalDirection
            topLeft
            newBottomRight


rectangleArea : Rectangle -> Int
rectangleArea rectangle =
    let
        size =
            rectangleSize rectangle
    in
        size.width * size.height



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        subscriptions =
            case model.action of
                Move _ ->
                    [ Mouse.moves MoveAt, Mouse.ups MoveEnd ]

                Resize _ ->
                    [ Mouse.moves ResizeAt, Mouse.ups ResizeEnd ]

                Select _ ->
                    [ Mouse.moves SelectAt, Mouse.ups SelectEnd ]

                _ ->
                    []
    in
        Sub.batch subscriptions



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
                    , onTouchStart MoveStart
                    , onTouchMove MoveAt
                    , onTouchEnd MoveEnd
                    ]
                    (borders ++ handles)
                , shadow
                    { topLeft =
                        { x = 0
                        , y = 0
                        }
                    , bottomRight =
                        { x = displaySelection.topLeft.x - 1 |> atLeast 0
                        , y = cropArea.height
                        }
                    }
                , shadow
                    { topLeft =
                        { x = displaySelection.bottomRight.x + 1 |> atMost cropArea.width
                        , y = 0
                        }
                    , bottomRight =
                        { x = cropArea.width
                        , y = cropArea.height
                        }
                    }
                , shadow
                    { topLeft =
                        { x = displaySelection.topLeft.x - 1 |> atLeast 0
                        , y = 0
                        }
                    , bottomRight =
                        { x = displaySelection.bottomRight.x + 1 |> atMost cropArea.width
                        , y = displaySelection.topLeft.y - 1 |> atLeast 0
                        }
                    }
                , shadow
                    { topLeft =
                        { x = displaySelection.topLeft.x - 1 |> atLeast 0
                        , y = displaySelection.bottomRight.y
                        }
                    , bottomRight =
                        { x = displaySelection.bottomRight.x + 1 |> atMost cropArea.width
                        , y = cropArea.height
                        }
                    }
                ]

        Nothing ->
            [ shadow
                { topLeft =
                    { x = 0
                    , y = 0
                    }
                , bottomRight =
                    { x = cropArea.width
                    , y = cropArea.height
                    }
                }
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

        -- Image taken from Jcrop: http://jcrop.org/css/Jcrop.gif
        backgroundImage =
            "data:image/gif;base64,R0lGODlhCAAIAJEAAKqqqv///wAAAAAAACH/C05FVFN"
                ++ "DQVBFMi4wAwEAAAAh+QQJCgAAACwAAAAACAAIAAACDZQFCadrzVRMB9FZ5"
                ++ "SwAIfkECQoAAAAsAAAAAAgACAAAAg+ELqCYaudeW9ChyOyltQAAIfkECQo"
                ++ "AAAAsAAAAAAgACAAAAg8EhGKXm+rQYtC0WGl9oAAAIfkECQoAAAAsAAAAA"
                ++ "AgACAAAAg+EhWKQernaYmjCWLF7qAAAIfkECQoAAAAsAAAAAAgACAAAAg2"
                ++ "EISmna81UTAfRWeUsACH5BAkKAAAALAAAAAAIAAgAAAIPFA6imGrnXlvQo"
                ++ "cjspbUAACH5BAkKAAAALAAAAAAIAAgAAAIPlIBgl5vq0GLQtFhpfaIAACH"
                ++ "5BAUKAAAALAAAAAAIAAgAAAIPlIFgknq52mJowlixe6gAADs="
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
                , ( "background", "#fff url(" ++ backgroundImage ++ ")" )
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
            , onTouchStart (ResizeStart orientation)
            , onTouchMove ResizeAt
            , onTouchEnd ResizeEnd
            , style
                [ ( "background-color", "rgba(49,28,28,0.58)" )
                , ( "border", "1px #eee solid" )
                , ( "width", "19px" )
                , ( "height", "19px" )
                , ( "position", "absolute" )
                , ( "opacity", "0.8" )
                , ( horizontalPosition, horizontalSpacing )
                , ( "margin-" ++ horizontalPosition, "-11px" )
                , ( verticalPosition, verticalSpacing )
                , ( "margin-" ++ verticalPosition, "-11px" )
                , ( "cursor", cursor ++ "-resize" )
                ]
            ]
            []


shadow : Rectangle -> Html Msg
shadow position =
    div
        [ onMouseDown SelectStart
        , onTouchStart SelectStart
        , onTouchMove SelectAt
        , onTouchEnd SelectEnd
        , style
            [ ( "background-color", "#000000" )
            , ( "opacity", "0.5" )
            , ( "position", "absolute" )
            , ( "left", px position.topLeft.x )
            , ( "top", px position.topLeft.y )
            , ( "width", px (position.bottomRight.x - position.topLeft.x) )
            , ( "height", px (position.bottomRight.y - position.topLeft.y) )
            ]
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


mouseEventDecoder : Json.Decoder MouseButtonEvent
mouseEventDecoder =
    Json.map3
        (\x y button -> MouseButtonEvent (Mouse.Position x y) button)
        (field "pageX" Json.int)
        (field "pageY" Json.int)
        (field "button" Json.int)


onTouchStart : (MouseButtonEvent -> Msg) -> Attribute Msg
onTouchStart msg =
    let
        decoder =
            touchDecoder (\x y -> MouseButtonEvent (Point x y) 0)
    in
        onWithOptions
            "touchstart"
            { stopPropagation = True
            , preventDefault = True
            }
            (Json.map msg decoder)


onTouchMove : (Point -> Msg) -> Attribute Msg
onTouchMove msg =
    onWithOptions
        "touchmove"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.map msg (touchDecoder Point))


onTouchEnd : (Point -> Msg) -> Attribute Msg
onTouchEnd msg =
    onWithOptions
        "touchend"
        { stopPropagation = True
        , preventDefault = True
        }
        (Json.map msg (touchDecoder Point))


touchDecoder : (Int -> Int -> a) -> Json.Decoder a
touchDecoder constructor =
    Json.map2
        constructor
        (Json.at [ "changedTouches", "0", "pageX" ] Json.int)
        (Json.at [ "changedTouches", "0", "pageY" ] Json.int)
