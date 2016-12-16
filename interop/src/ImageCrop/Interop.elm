port module ImageCrop.Interop exposing (main, viewportChanged)

import Html exposing (Html)
import ImageCrop


type alias Flags =
    { image : ImageCrop.Size
    , cropAreaWidth : Int
    , selection : Maybe ImageCrop.Rectangle
    , aspectRatio : Maybe ImageCrop.AspectRatio
    }


init : Flags -> ( ImageCrop.Model, Cmd Msg )
init { image, cropAreaWidth, selection, aspectRatio } =
    ( ImageCrop.init image cropAreaWidth selection aspectRatio
    , Cmd.none
    )


type Msg
    = ViewportChanged Int
    | ReceiveOffset ImageCrop.Point
    | ChangeAspectRatio (Maybe ImageCrop.AspectRatio)
    | ImageCropMsg ImageCrop.Msg


main : Program Flags ImageCrop.Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


update : Msg -> ImageCrop.Model -> ( ImageCrop.Model, Cmd Msg )
update msg model =
    case msg of
        ViewportChanged width ->
            ( { model | cropAreaWidth = width }, Cmd.none )

        ReceiveOffset offset ->
            ( ImageCrop.receiveOffset offset model, Cmd.none )

        ChangeAspectRatio aspectRatio ->
            ( ImageCrop.changeAspectRatio aspectRatio model, Cmd.none )

        ImageCropMsg msg ->
            let
                ( newModel, newCmd, notification ) =
                    ImageCrop.update msg model

                interopCmd =
                    case notification of
                        ImageCrop.RequestOffset ->
                            requestOffset ()

                        ImageCrop.SelectionChanged selection ->
                            selectionChanged selection

                        _ ->
                            Cmd.none
            in
                ( newModel
                , Cmd.batch [ Cmd.map ImageCropMsg newCmd, interopCmd ]
                )


view : ImageCrop.Model -> Html Msg
view model =
    Html.map ImageCropMsg (ImageCrop.view model)


subscriptions : ImageCrop.Model -> Sub Msg
subscriptions model =
    let
        imageCropSubs =
            Sub.map ImageCropMsg <| ImageCrop.subscriptions model
    in
        Sub.batch
            [ viewportChanged ViewportChanged
            , receiveOffset ReceiveOffset
            , changeAspectRatio ChangeAspectRatio
            , imageCropSubs
            ]


port selectionChanged : Maybe ImageCrop.Rectangle -> Cmd msg


port viewportChanged : (Int -> msg) -> Sub msg


port changeAspectRatio : (Maybe ImageCrop.AspectRatio -> msg) -> Sub msg


port requestOffset : () -> Cmd msg


port receiveOffset : (ImageCrop.Point -> msg) -> Sub msg
