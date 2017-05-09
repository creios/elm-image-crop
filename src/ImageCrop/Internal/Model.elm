module ImageCrop.Internal.Model exposing (..)

import Mouse


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


type alias Rectangle =
    { topLeft : Point
    , bottomRight : Point
    }


type alias Point =
    { x : Int
    , y : Int
    }


type alias Movement =
    { horizontal : Int
    , vertical : Int
    }


type alias Size =
    { width : Int
    , height : Int
    }


type alias AspectRatio =
    { width : Float
    , height : Float
    }


type Position
    = PositionTop
    | PositionRight
    | PositionBottom
    | PositionLeft


type Orientation
    = Horizontal
    | Vertical
