module ImageCrop.Model
    exposing
        ( Model
        , Point
        , Size
        , Rectangle
        , AspectRatio
        , Msg
        , Notification(..)
        )

import Mouse
import ImageCrop.Internal.Model exposing (..)


type alias Msg =
    ImageCrop.Internal.Model.Msg


type alias Point =
    ImageCrop.Internal.Model.Point


type alias Rectangle =
    ImageCrop.Internal.Model.Rectangle


type alias Size =
    ImageCrop.Internal.Model.Size


type alias AspectRatio =
    ImageCrop.Internal.Model.AspectRatio


type alias Model =
    { image : Size
    , cropAreaWidth : Int
    , selection : Maybe Rectangle
    , aspectRatio : Maybe AspectRatio
    , action : Action
    }


type Notification
    = NoNotification
    | SelectionChanged (Maybe Rectangle)
    | RequestOffset
