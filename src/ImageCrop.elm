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
        , changeAspectRatio
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
    , aspectRatio : Maybe Size
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


init : Size -> Int -> Point -> Maybe Rectangle -> Maybe Size -> Model
init image cropAreaWidth offset selection aspectRatio =
    { image = image
    , cropAreaWidth = cropAreaWidth
    , offset = offset
    , selection = selection
    , aspectRatio = aspectRatio
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
                            resizeSelection model resize xy
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


normalizeEdges : Size -> Point -> Point -> Rectangle
normalizeEdges canvas first second =
    { topLeft =
        { x = min first.x second.x |> atLeast 0
        , y = min first.y second.y |> atLeast 0
        }
    , bottomRight =
        { x = max first.x second.x |> atMost canvas.width
        , y = max first.y second.y |> atMost canvas.height
        }
    }


resizeSelection : Model -> Resize -> Mouse.Position -> Rectangle
resizeSelection model resize position =
    let
        { topLeft, bottomRight } =
            resize.originalSelection

        relativeCoordinates point =
            { x = point.x - model.offset.x
            , y = point.y - model.offset.y
            }

        factor =
            toFloat model.image.width / toFloat model.cropAreaWidth

        scale value =
            round (toFloat value * factor)

        scalePoint point =
            { x = scale point.x
            , y = scale point.y
            }

        normalizedPosition =
            scalePoint (relativeCoordinates position)
    in
        case model.aspectRatio of
            Just aspectRatio ->
                let
                    anchor =
                        calculateAnchor resize

                    mouseRectangle =
                        createRectangleFromMouse
                            model.image
                            resize
                            aspectRatio
                            anchor
                            normalizedPosition

                    edgeConstraints =
                        createMaxRectangles
                            resize
                            model.image
                            aspectRatio
                            anchor
                            normalizedPosition

                    smallestRectangle =
                        minBy
                            rectangleArea
                in
                   List.foldr
                       smallestRectangle
                       mouseRectangle
                       edgeConstraints

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

calculateAnchor : Resize -> Point
calculateAnchor { direction, originalSelection } =
    let
        { topLeft, bottomRight } = originalSelection

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


createRectangleFromMouse : Size -> Resize -> Size -> Point -> Point -> Rectangle
createRectangleFromMouse image { direction, originalSelection } aspectRatio anchor position =
    if List.member direction [ West, East ] then
        let
            x =
                position.x
                    |> atLeast 0
                    |> atMost image.width

            width =
                abs (x - anchor.x)

            height =
                round (toFloat width / toFloat aspectRatio.width * toFloat aspectRatio.height)

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
    else if List.member direction [ North, South ] then
        let
            y =
                position.y
                    |> atLeast 0
                    |> atMost image.height

            height =
                abs (y - anchor.y)

            width =
                round (toFloat height / toFloat aspectRatio.height * toFloat aspectRatio.width)

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
    else
        let
            horizontallyAlignedRectangle =
                let
                    width = abs (position.x - anchor.x)

                    factor = if position.y < anchor.y then -1 else 1

                    height = factor * round (toFloat width / toFloat aspectRatio.width * toFloat aspectRatio.height)

                    target =
                        { x = position.x
                        , y = anchor.y + height
                        }
                in
                    orderEdges anchor target

            verticallyAlignedRectangle =
                let
                    height = abs (position.y - anchor.y)

                    factor = if position.x < anchor.x then -1 else 1

                    width = factor * round (toFloat height / toFloat aspectRatio.height * toFloat aspectRatio.width)

                    target =
                        { x = anchor.x + width
                        , y = position.y
                        }
                in
                    orderEdges anchor target
        in
            maxBy -- You can use minBy here instead to alter the behaviour
                rectangleArea
                horizontallyAlignedRectangle
                verticallyAlignedRectangle



createMaxRectangles { direction } image aspectRatio anchor position =
    if List.member direction [ West, East ] then
        [ createMaxRectangleTop
            image.height aspectRatio anchor position
        , createMaxRectangleBottom
            image.height aspectRatio anchor position
        ]
    else if List.member direction [ North, South ] then
        [ createMaxRectangleLeft
            image.width aspectRatio anchor position
        , createMaxRectangleRight
            image.width aspectRatio anchor position
        ]
    else
        []


createMaxRectangleTop imageHeight aspectRatio anchor position =
    let
        height = anchor.y * 2

        factor = if position.x < anchor.x then -1 else 1

        width = factor * (round (toFloat height / toFloat aspectRatio.height * toFloat aspectRatio.width))

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

createMaxRectangleBottom imageHeight aspectRatio anchor position =
    let
        height = (imageHeight - anchor.y) * 2

        factor = if position.x < anchor.x then -1 else 1

        width = factor * (round (toFloat height / toFloat aspectRatio.height * toFloat aspectRatio.width))

        first =
           { x = anchor.x
           , y = imageHeight - height
           }

        second =
           { x = anchor.x + width
           , y = imageHeight
           }
    in
        orderEdges first second

createMaxRectangleLeft imageWidth aspectRatio anchor position =
    let
        width = anchor.x * 2

        factor = if position.y < anchor.y then -1 else 1

        height = factor * (round (toFloat width / toFloat aspectRatio.width * toFloat aspectRatio.height))

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

createMaxRectangleRight imageWidth aspectRatio anchor position =
    let
        width = (imageWidth - anchor.x) * 2

        factor = if position.y < anchor.y then -1 else 1

        height = factor * (round (toFloat width / toFloat aspectRatio.width * toFloat aspectRatio.height))

        first =
           { x = imageWidth - width
           , y = anchor.y
           }

        second =
           { x = imageWidth
           , y = anchor.y + height
           }
    in
        orderEdges first second


minBy : (a -> comparable) -> a -> a -> a
minBy fn a b =
    if fn b < fn a then
        b
    else
        a

maxBy : (a -> comparable) -> a -> a -> a
maxBy fn a b =
    if fn b > fn a then
        b
    else
        a

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

absClamp n =
    clamp -n n

rectangleSize : Rectangle -> Size
rectangleSize { topLeft, bottomRight } =
    { width = bottomRight.x - topLeft.x
    , height = bottomRight.y - topLeft.y
    }


createSelection : Select -> Model -> Mouse.Position -> Maybe Rectangle
createSelection select model position =
    if select.start == position then
        Nothing
    else
        let
            relativeCoordinates point =
                { x = point.x - model.offset.x
                , y = point.y - model.offset.y
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


changeAspectRatio : Maybe Size -> Model -> Model
changeAspectRatio maybeAspectRatio model =
    let
        selectionUpdated =
            case maybeAspectRatio of
                Just aspectRatio ->
                    case model.selection of
                        Just selection ->
                            let
                                newSelection =
                                    recalculateSelection aspectRatio selection
                            in
                               { model | selection = Just newSelection }

                        Nothing ->
                            model

                Nothing ->
                    model
    in
       { selectionUpdated | aspectRatio = maybeAspectRatio }

recalculateSelection : Size -> Rectangle -> Rectangle
recalculateSelection aspectRatio selection =
    let
        area =
            rectangleArea selection

        height =
            round (sqrt (toFloat area / (toFloat aspectRatio.width / toFloat aspectRatio.height)))

        width =
            round (sqrt (toFloat area * (toFloat aspectRatio.width / toFloat aspectRatio.height)))

        { topLeft } = selection

        newBottomRight =
            { x = topLeft.x + width
            , y = topLeft.y + height
            }
    in
        { selection | bottomRight = newBottomRight }



rectangleArea : Rectangle -> Int
rectangleArea rectangle =
    let
        size = rectangleSize rectangle
    in
        size.width * size.height

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
                    (borders ++ handles)
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
