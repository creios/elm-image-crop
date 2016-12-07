port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style, src, width, height, value, type_, id)
import Html.Events exposing (onInput)
import ImageCrop
import Platform.Cmd
import String exposing (toInt)


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model

type alias Flags =
    { cropAreaWidth : Int
    , offset : ImageCrop.Point
    }


type alias Model =
    ImageCrop.Model


type alias Size =
    { width : Int
    , height : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( ImageCrop.init
        { width = 1800
        , height = 1200
        }
        flags.cropAreaWidth
        flags.offset
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



-- Update


type Msg
    = ImageCropMsg ImageCrop.Msg
    | ViewportChanged Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageCropMsg msg ->
            let
                ( newModel, newCmd ) =
                    ImageCrop.update msg model
            in
                ( newModel
                , Platform.Cmd.map ImageCropMsg newCmd
                )

        ViewportChanged width ->
            ( { model | cropAreaWidth = width }, Cmd.none )


updateSelectionValue : Model -> String -> (ImageCrop.Rectangle -> Int) -> (Int -> ImageCrop.Rectangle -> ImageCrop.Rectangle) -> ( Model, Cmd Msg )
updateSelectionValue model value default transform =
    let
        newModel =
            case model.selection of
                Just selection ->
                    let
                        number =
                            Result.withDefault (default selection) (toInt value)

                        newSelection =
                            transform number selection
                    in
                        { model | selection = Just newSelection }

                Nothing ->
                    model
    in
        ( newModel, Cmd.none )



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        imageCropSubs =
            Sub.map ImageCropMsg <| ImageCrop.subscriptions model
    in
        Sub.batch
            [ viewportChanged ViewportChanged
            , imageCropSubs
            ]



-- View


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "position", "relative" )
            ]
        ]
        [ placeholdit model.image model.cropAreaWidth
        , Html.map
            ImageCropMsg
            (ImageCrop.view model)
        , rectangle model.selection
        ]


placeholdit : ImageCrop.Size -> Int -> Html Msg
placeholdit size displayWidth =
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


port viewportChanged : (Int -> msg) -> Sub msg
