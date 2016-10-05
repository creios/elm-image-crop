port module Interop exposing (main, viewportChanged)

import Html exposing (Html)
import Html.App
import ImageCropper


type alias Flags =
    { image : ImageCropper.Size
    , cropAreaWidth : Int
    , selection : Maybe ImageCropper.Rectangle
    }


init : Flags -> ( ImageCropper.Model, Cmd Msg )
init { image, cropAreaWidth, selection } =
    ( ImageCropper.init image cropAreaWidth selection
    , Cmd.none
    )


type Msg
    = ViewportChanged Int
    | ImageCropperMsg ImageCropper.Msg


main : Program Flags
main =
    Html.App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> ImageCropper.Model -> ( ImageCropper.Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                ViewportChanged width ->
                    { model | cropAreaWidth = width }

                ImageCropperMsg msg ->
                    fst (ImageCropper.update msg model)
    in
        ( newModel, selectionChanged newModel.selection )


view : ImageCropper.Model -> Html Msg
view model =
    Html.App.map ImageCropperMsg (ImageCropper.view model)


subscriptions : ImageCropper.Model -> Sub Msg
subscriptions model =
    let
        imageCropperSubs =
            Sub.map ImageCropperMsg <| ImageCropper.subscriptions model
    in
        Sub.batch [ viewportChanged ViewportChanged, imageCropperSubs ]


port selectionChanged : Maybe ImageCropper.Rectangle -> Cmd msg


port viewportChanged : (Int -> msg) -> Sub msg
