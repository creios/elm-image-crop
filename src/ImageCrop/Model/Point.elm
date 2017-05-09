module ImageCrop.Model.Point exposing (Point, scale, move, clamp, map, map2)

import ImageCrop.Model.Movement as Movement exposing (Movement)
import ImageCrop.Model.Size as Size exposing (Size)
import Basics


type alias Point =
    { x : Int
    , y : Int
    }


scale : Float -> Point -> Point
scale factor point =
    map (toFloat >> ((*) factor) >> round) point


move : Movement -> Point -> Point
move movement point =
    { x = point.x + movement.horizontal
    , y = point.y + movement.vertical
    }


clamp : Size -> Point -> Point
clamp size point =
    { x = Basics.clamp 0 size.width point.x
    , y = Basics.clamp 0 size.height point.y
    }


map : (Int -> Int) -> Point -> Point
map fn point =
    { x = fn point.x
    , y = fn point.y
    }


map2 : (Int -> Int -> Int) -> Point -> Point -> Point
map2 fn point1 point2 =
    { x = fn point1.x point2.x
    , y = fn point1.y point2.y
    }
