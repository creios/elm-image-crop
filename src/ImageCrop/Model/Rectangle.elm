module ImageCrop.Model.Rectangle
    exposing
        ( Rectangle
        , size
        , scale
        , move
        , clamp
        , normalize
        , area
        , map
        )

import ImageCrop.Model.Movement as Movement exposing (Movement)
import ImageCrop.Model.Point as Point exposing (Point)
import ImageCrop.Model.Size as Size exposing (Size)


type alias Rectangle =
    { topLeft : Point
    , bottomRight : Point
    }


size : Rectangle -> Size
size { topLeft, bottomRight } =
    { width = bottomRight.x - topLeft.x
    , height = bottomRight.y - topLeft.y
    }


scale : Float -> Rectangle -> Rectangle
scale factor rectangle =
    map (Point.scale factor) rectangle


move : Movement -> Rectangle -> Rectangle
move movement rectangle =
    map (Point.move movement) rectangle


clamp : Size -> Rectangle -> Rectangle
clamp size rectangle =
    map (Point.clamp size) rectangle


normalize : Point -> Point -> Rectangle
normalize point1 point2 =
    { topLeft = Point.map2 min point1 point2
    , bottomRight = Point.map2 max point1 point2
    }


area : Rectangle -> Int
area rectangle =
    let
        { width, height } =
            size rectangle
    in
        width * height


map : (Point -> Point) -> Rectangle -> Rectangle
map fn rectangle =
    { topLeft = fn rectangle.topLeft
    , bottomRight = fn rectangle.bottomRight
    }
