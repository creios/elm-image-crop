port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, src, width, height, value, type_, id, selected, attribute)
import Html.Events exposing (onInput, on)
import ImageCrop
import Platform.Cmd
import String exposing (toInt)
import Json.Decode as Json


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
    | AspectRatioChanged String


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

        AspectRatioChanged key ->
            case model of
                Initializing offset size ->
                    ( model, Cmd.none )

                Running model ->
                    let
                        aspectRatio =
                            if key == "free" then
                                Nothing
                            else if key == "square" then
                                Just { width = 1, height = 1 }
                            else if key == "din-landscape" then
                                Just { width = sqrt 2, height = 1 }
                            else if key == "din-portrait" then
                                Just { width = 1, height = sqrt 2 }
                            else
                                Nothing

                        newModel =
                            ImageCrop.changeAspectRatio aspectRatio model
                    in
                        ( Running newModel, Cmd.none )



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
                    [ demoImage size ]

                Running model ->
                    [ demoImage model.image
                    , Html.map
                        ImageCropMsg
                        (ImageCrop.view model)
                    ]

        controls =
            case model of
                Initializing offset size ->
                    []

                Running model ->
                    [ rectangle model.selection
                    , label
                        []
                        [ text "Aspect Ratio "
                        , select
                            [ onSelect AspectRatioChanged ]
                            [ option
                                [ value "free", selected True ]
                                [ text "Free" ]
                            , option
                                [ value "square" ]
                                [ text "Square" ]
                            , optgroup
                                [ labelAttribute "Landscape" ]
                                [ option
                                    [ value "din-landscape" ]
                                    [ text "DIN" ]
                                ]
                            , optgroup
                                [ labelAttribute "Potrait" ]
                                [ option
                                    [ value "din-portrait" ]
                                    [ text "DIN" ]
                                ]
                            ]
                        ]
                    ]

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


labelAttribute text =
    attribute "label" text


demoImage : ImageCrop.Size -> Html Msg
demoImage size =
    img
        [ src ("https://picload.org/image/raooacap/1k5qq4yqm0g-mark-basarab.jpg")
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


onSelect : (String -> msg) -> Html.Attribute msg
onSelect msg =
    on "change" (Json.map msg (Json.at [ "target", "value" ] Json.string))


port ready : () -> Cmd msg


port viewportChanged : (Int -> msg) -> Sub msg
