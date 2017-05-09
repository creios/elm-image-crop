module ImageCrop.View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onWithOptions)
import Mouse
import Json.Decode as Json exposing (field)
import ImageCrop.Model exposing (Model)
import ImageCrop.Internal.Model exposing (..)
import ImageCrop.Internal.Update
    exposing
        ( atLeast
        , atMost
        , scaleRectangle
        , rectangleSize
        )


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
