module ImageCrop.Update
    exposing
        ( init
        , update
        , subscriptions
        , changeAspectRatio
        , receiveOffset
        )

import Mouse
import ImageCrop.Model exposing (Notification(..), Model)
import ImageCrop.Internal.Model exposing (..)
import ImageCrop.Internal.Update exposing (..)
import ImageCrop.Model.AspectRatio as AspectRatio exposing (AspectRatio)
import ImageCrop.Model.Movement as Movement exposing (Movement)
import ImageCrop.Model.Point as Point exposing (Point)
import ImageCrop.Model.Rectangle as Rectangle exposing (Rectangle)
import ImageCrop.Model.Size as Size exposing (Size)


init : Size -> Int -> Maybe Rectangle -> Maybe AspectRatio -> Model
init image cropAreaWidth selection aspectRatio =
    { image = image
    , cropAreaWidth = cropAreaWidth
    , selection = selection
    , aspectRatio = aspectRatio
    , action = NoAction
    }


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
        Rectangle.move movement move.originalSelection


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
            Point.move normalizedMovement startingPoint
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
                    Rectangle.normalize first second
                        |> Rectangle.clamp model.image


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
            (minBy Rectangle.area)
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
                Rectangle.normalize first second

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
                Rectangle.normalize first second

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
                Rectangle.normalize anchor target

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
                Rectangle.normalize anchor target
    in
        -- You can use minBy here instead to alter the behaviour
        maxBy
            Rectangle.area
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
                        Rectangle.normalize first second

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
                        Rectangle.normalize first second
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
                        Rectangle.normalize first second

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
                        Rectangle.normalize first second
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
                        Rectangle.normalize anchor target

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
                        Rectangle.normalize anchor target
            in
                [ horizontalBoundary, verticalBoundary ]


widthFromHeight aspectRatio height =
    round (toFloat height / aspectRatio.height * aspectRatio.width)


heightFromWidth aspectRatio width =
    round (toFloat width / aspectRatio.width * aspectRatio.height)


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
                        Rectangle.normalize normalizedStart normalizedPosition
                            |> Rectangle.clamp model.image
        in
            Just selection


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
            Rectangle.area selection

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
