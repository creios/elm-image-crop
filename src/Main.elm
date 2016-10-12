module Main exposing (..)

import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (style, src, width, height, value, type')
import Html.Events exposing (onInput)
import ImageCrop
import Platform.Cmd
import String exposing (toInt)


main : Program Never
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    ImageCrop.Model


type alias Size =
    { width : Int
    , height : Int
    }


init : ( Model, Cmd Msg )
init =
    ( ImageCrop.init
        { width = 1800
        , height = 1200
        }
        900
        { x = 20
        , y = 20
        }
        (Just
            { topLeft =
                { x = 822
                , y = 1
                }
            , bottomRight =
                { x = 906
                , y = 421
                }
            }
        )
        (Just
            { width = 1
            , height = 5
            }
        )
    , Cmd.none
    )



-- Update


type Msg
    = ImageCropMsg ImageCrop.Msg
    | TopLeftX String
    | TopLeftY String
    | BottomRightX String
    | BottomRightY String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageCropMsg msg ->
            let
                ( model, cmd ) =
                    ImageCrop.update msg model
            in
                ( model
                , Platform.Cmd.map ImageCropMsg cmd
                )

        TopLeftX value ->
            let
                default selection =
                    selection.topLeft.x

                transform number selection =
                    let
                        { topLeft } =
                            selection
                    in
                        { selection | topLeft = { topLeft | x = number } }
            in
                updateSelectionValue model value default transform

        TopLeftY value ->
            let
                default selection =
                    selection.topLeft.y

                transform number selection =
                    let
                        { topLeft } =
                            selection
                    in
                        { selection | topLeft = { topLeft | y = number } }
            in
                updateSelectionValue model value default transform

        BottomRightX value ->
            let
                default selection =
                    selection.bottomRight.x

                transform number selection =
                    let
                        { bottomRight } =
                            selection
                    in
                        { selection | bottomRight = { bottomRight | x = number } }
            in
                updateSelectionValue model value default transform

        BottomRightY value ->
            let
                default selection =
                    selection.bottomRight.y

                transform number selection =
                    let
                        { bottomRight } =
                            selection
                    in
                        { selection | bottomRight = { bottomRight | y = number } }
            in
                updateSelectionValue model value default transform


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
    Sub.map ImageCropMsg <| ImageCrop.subscriptions model



-- View


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "position", "relative" )
            , ( "margin", "20px" )
            ]
        ]
        [ unsplashit model.image model.cropAreaWidth
        , Html.App.map
            ImageCropMsg
            (ImageCrop.view model)
        , debugForm model.selection
        ]


unsplashit : ImageCrop.Size -> Int -> Html Msg
unsplashit size displayWidth =
    img
        [ src ("https://unsplash.it/" ++ toString size.width ++ "/" ++ toString size.height ++ "?image=1067")
        , width size.width
        , height size.height
        , style
            [ ( "width", toString displayWidth ++ "px" )
            , ( "height", "auto" )
            ]
        ]
        []


debugForm : Maybe ImageCrop.Rectangle -> Html Msg
debugForm selection =
    case selection of
        Just selection ->
            let
                { topLeft, bottomRight } =
                    selection
            in
                Html.form
                    []
                    [ text "("
                    , coordinateInput TopLeftX topLeft.x
                    , text "|"
                    , coordinateInput TopLeftY topLeft.y
                    , text "), ("
                    , coordinateInput BottomRightX bottomRight.x
                    , text "|"
                    , coordinateInput BottomRightY bottomRight.y
                    , text ")"
                    ]

        Nothing ->
            p [] [ text "No selection" ]


coordinateInput : (String -> Msg) -> Int -> Html Msg
coordinateInput msg val =
    input
        [ type' "number"
        , value (toString val)
        , onInput msg
        ]
        []
