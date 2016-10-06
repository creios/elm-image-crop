port module Interop exposing (main, viewportChanged)

import Html exposing (Html)
import Html.App
import ImageCrop


type alias Flags =
    { image : ImageCrop.Size
    , cropAreaWidth : Int
    , offset : ImageCrop.Point
    , selection : Maybe ImageCrop.Rectangle
    }


init : Flags -> ( ImageCrop.Model, Cmd Msg )
init { image, cropAreaWidth, offset, selection } =
    ( ImageCrop.init image cropAreaWidth offset selection
    , Cmd.none
    )


type Msg
    = ViewportChanged Int
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
        imageCroppSubs =
            Sub.map ImageCropMsg <| ImageCrop.subscriptions model
    in
        Sub.batch [ viewportChanged ViewportChanged, imageCroppSubs ]


port selectionChanged : Maybe ImageCrop.Rectangle -> Cmd msg


port viewportChanged : (Int -> msg) -> Sub msg
