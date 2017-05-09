port module ImageCrop.Interop exposing (main, viewportChanged)

import Html exposing (Html)
import ImageCrop.Model
import ImageCrop.Update
import ImageCrop.View


type alias Flags =
    { image : ImageCrop.Model.Size
    , cropAreaWidth : Int
    , selection : Maybe ImageCrop.Model.Rectangle
    , aspectRatio : Maybe ImageCrop.Model.AspectRatio
    }


init : Flags -> ( ImageCrop.Model.Model, Cmd Msg )
init { image, cropAreaWidth, selection, aspectRatio } =
    ( ImageCrop.Update.init image cropAreaWidth selection aspectRatio
    , Cmd.none
    )


type Msg
    = ViewportChanged Int
    | ReceiveOffset ImageCrop.Model.Point
    | ChangeAspectRatio (Maybe ImageCrop.Model.AspectRatio)
    | ImageCropMsg ImageCrop.Model.Msg


main : Program Flags ImageCrop.Model.Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> ImageCrop.Model.Model -> ( ImageCrop.Model.Model, Cmd Msg )
update msg model =
    case msg of
        ViewportChanged width ->
            ( { model | cropAreaWidth = width }, Cmd.none )

        ReceiveOffset offset ->
            ( ImageCrop.Update.receiveOffset offset model, Cmd.none )

        ChangeAspectRatio aspectRatio ->
            ( ImageCrop.Update.changeAspectRatio aspectRatio model, Cmd.none )

        ImageCropMsg msg ->
            let
                ( newModel, newCmd, notification ) =
                    ImageCrop.Update.update msg model

                interopCmd =
                    case notification of
                        ImageCrop.Model.RequestOffset ->
                            requestOffset ()

                        ImageCrop.Model.SelectionChanged selection ->
                            selectionChanged selection

                        _ ->
                            Cmd.none
            in
                ( newModel
                , Cmd.batch [ Cmd.map ImageCropMsg newCmd, interopCmd ]
                )


view : ImageCrop.Model.Model -> Html Msg
view model =
    Html.map ImageCropMsg (ImageCrop.View.view model)


subscriptions : ImageCrop.Model.Model -> Sub Msg
subscriptions model =
    let
        imageCropSubs =
            Sub.map ImageCropMsg <| ImageCrop.Update.subscriptions model
    in
        Sub.batch
            [ viewportChanged ViewportChanged
            , receiveOffset ReceiveOffset
            , changeAspectRatio ChangeAspectRatio
            , imageCropSubs
            ]


port selectionChanged : Maybe ImageCrop.Model.Rectangle -> Cmd msg


port viewportChanged : (Int -> msg) -> Sub msg


port changeAspectRatio : (Maybe ImageCrop.Model.AspectRatio -> msg) -> Sub msg


port requestOffset : () -> Cmd msg


port receiveOffset : (ImageCrop.Model.Point -> msg) -> Sub msg
