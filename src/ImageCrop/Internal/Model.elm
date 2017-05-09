module ImageCrop.Internal.Model exposing (..)

import Mouse
import ImageCrop.Model.Point exposing (Point)
import ImageCrop.Model.Rectangle exposing (Rectangle)
import ImageCrop.Model.Size exposing (Size)


type alias MouseButtonEvent =
    { position : Mouse.Position
    , button : Int
    }


type Direction
    = North
    | NorthEast
    | East
    | SouthEast
    | South
    | SouthWest
    | West
    | NorthWest


type Msg
    = MoveStart MouseButtonEvent
    | MoveAt Mouse.Position
    | MoveEnd Mouse.Position
    | ResizeStart Direction MouseButtonEvent
    | ResizeAt Mouse.Position
    | ResizeEnd Mouse.Position
    | SelectStart MouseButtonEvent
    | SelectAt Mouse.Position
    | SelectEnd Mouse.Position


type Action
    = NoAction
    | Move MoveData
    | Resize ResizeData
    | WaitingForOffset WaitingForOffsetData
    | Select SelectData


type alias MoveData =
    { start : Mouse.Position
    , originalSelection : Rectangle
    }


type alias ResizeData =
    { direction : Direction
    , start : Mouse.Position
    , originalSelection : Rectangle
    }


type alias WaitingForOffsetData =
    { start : Mouse.Position
    }


type alias SelectData =
    { start : Mouse.Position
    , offset : Point
    }


type Position
    = PositionTop
    | PositionRight
    | PositionBottom
    | PositionLeft


type Orientation
    = Horizontal
    | Vertical
