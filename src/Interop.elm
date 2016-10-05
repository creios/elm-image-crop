port module Interop exposing (main)

import Html.App
import ImageCropper


type alias Flags =
    { image : ImageCropper.Size
    , selection : ImageCropper.Rectangle
    }


init : Flags -> ( ImageCropper.Model, Cmd ImageCropper.Msg )
init { image, selection } =
    ( ImageCropper.init image selection
    , Cmd.none
    )


main : Program Flags
main =
    Html.App.programWithFlags
        { init = init
        , view = ImageCropper.view
        , update = update
        , subscriptions = ImageCropper.subscriptions
        }


update : ImageCropper.Msg -> ImageCropper.Model -> ( ImageCropper.Model, Cmd ImageCropper.Msg )
update msg model =
    let
        ( newModel, _ ) =
            ImageCropper.update msg model
    in
        ( newModel, selectionChanged newModel.selection )


port selectionChanged : ImageCropper.Rectangle -> Cmd msg
