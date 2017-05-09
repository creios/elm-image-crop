module ImageCrop.Model
    exposing
        ( Model
        , Msg
        , Notification(..)
        )

import Mouse
import ImageCrop.Internal.Model exposing (..)
import ImageCrop.Model.AspectRatio exposing (AspectRatio)
import ImageCrop.Model.Point exposing (Point)
import ImageCrop.Model.Rectangle exposing (Rectangle)
import ImageCrop.Model.Size exposing (Size)


type alias Msg =
    ImageCrop.Internal.Model.Msg


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
