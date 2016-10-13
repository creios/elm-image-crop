port module Interop exposing (main, viewportChanged)

import Html exposing (Html)
import Html.App
import ImageCrop


type alias Flags =
    { image : ImageCrop.Size
    , cropAreaWidth : Int
    , offset : ImageCrop.Point
    , selection : Maybe ImageCrop.Rectangle
    , aspectRatio : Maybe ImageCrop.Size
    }


init : Flags -> ( ImageCrop.Model, Cmd Msg )
init { image, cropAreaWidth, offset, selection, aspectRatio } =
    ( ImageCrop.init image cropAreaWidth offset selection aspectRatio
    , Cmd.none
    )


type Msg
    = ViewportChanged Int
    | ChangeAspectRatio (Maybe ImageCrop.Size)
    | ImageCropMsg ImageCrop.Msg


main : Program Flags
main =
    Html.App.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> ImageCrop.Model -> ( ImageCrop.Model, Cmd Msg )
update msg model =
    let
        newModel =
            case msg of
                ViewportChanged width ->
                    { model | cropAreaWidth = width }

                ChangeAspectRatio aspectRatio ->
                    ImageCrop.changeAspectRatio aspectRatio model

                ImageCropMsg msg ->
                    fst (ImageCrop.update msg model)
    in
        ( newModel, selectionChanged newModel.selection )


view : ImageCrop.Model -> Html Msg
view model =
    Html.App.map ImageCropMsg (ImageCrop.view model)


subscriptions : ImageCrop.Model -> Sub Msg
subscriptions model =
    let
        imageCropSubs =
            Sub.map ImageCropMsg <| ImageCrop.subscriptions model
    in
        Sub.batch
            [ viewportChanged ViewportChanged
            , changeAspectRatio ChangeAspectRatio
            , imageCropSubs
            ]


port selectionChanged : Maybe ImageCrop.Rectangle -> Cmd msg


port viewportChanged : (Int -> msg) -> Sub msg


port changeAspectRatio : (Maybe ImageCrop.Size -> msg) -> Sub msg
