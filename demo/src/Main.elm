port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, src, width, height, value, type_, id)
import Html.Events exposing (onInput)
import ImageCrop
import Platform.Cmd
import String exposing (toInt)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type Model
    = Initializing ImageCrop.Point ImageCrop.Size
    | Running ImageCrop.Model


type alias Viewport =
    { width : Int
    , offset : ImageCrop.Point
    }


type alias Size =
    { width : Int
    , height : Int
    }


init : ( Model, Cmd Msg )
init =
    ( Initializing { x = 20, y = 20 } { width = 1800, height = 1200 }, ready () )



-- Update


type Msg
    = ImageCropMsg ImageCrop.Msg
    | ViewportChanged Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageCropMsg msg ->
            case model of
                Initializing offset size ->
                    ( model, Cmd.none )

                Running model ->
                    let
                        ( newModel, newCmd ) =
                            ImageCrop.update msg model
                    in
                        ( Running newModel
                        , Platform.Cmd.map ImageCropMsg newCmd
                        )

        ViewportChanged width ->
            case model of
                Initializing offset size ->
                    ( Running <|
                        ImageCrop.init
                            size
                            width
                            offset
                            (Just
                                { topLeft =
                                    { x = 400
                                    , y = 200
                                    }
                                , bottomRight =
                                    { x = 610
                                    , y = 497
                                    }
                                }
                            )
                            Nothing
                    , Cmd.none
                    )

                Running model ->
                    ( Running { model | cropAreaWidth = width }, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        imageCropSubs =
            case model of
                Initializing offset size ->
                    Sub.none

                Running model ->
                    Sub.map ImageCropMsg <| ImageCrop.subscriptions model
    in
        Sub.batch
            [ viewportChanged ViewportChanged
            , imageCropSubs
            ]



-- View


view : Model -> Html Msg
view model =
    let
        imageCrop =
            case model of
                Initializing offset size ->
                    [ placeholdit size ]

                Running model ->
                    [ placeholdit model.image
                    , Html.map
                        ImageCropMsg
                        (ImageCrop.view model)
                    ]

        controls =
            case model of
                Initializing offset size ->
                    []

                Running model ->
                    [ rectangle model.selection ]

        imageCropContainer =
            [ div
                [ style
                    [ ( "position", "relative" )
                    ]
                ]
                imageCrop
            ]
    in
        div
            [ style
                [ ( "font-family", "monospace" )
                , ( "padding", "20px" )
                ]
            ]
            (imageCropContainer ++ controls)


placeholdit : ImageCrop.Size -> Html Msg
placeholdit size =
    img
        [ src ("https://placehold.it/" ++ toString size.width ++ "x" ++ toString size.height)
        , width size.width
        , height size.height
        , style
            [ ( "width", "100%" )
            , ( "max-width", "900px" )
            , ( "height", "auto" )
            ]
        , id "image"
        ]
        []


rectangle : Maybe ImageCrop.Rectangle -> Html Msg
rectangle selection =
    let
        output =
            case selection of
                Just selection ->
                    "(" ++ toString selection.topLeft.x ++ "|" ++ toString selection.topLeft.y ++ "), (" ++ toString selection.bottomRight.x ++ "|" ++ toString selection.bottomRight.y ++ ")"

                Nothing ->
                    "No selection"
    in
        p [] [ text output ]


port ready : () -> Cmd msg


port viewportChanged : (Int -> msg) -> Sub msg
