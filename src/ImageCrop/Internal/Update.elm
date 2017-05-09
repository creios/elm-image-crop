module ImageCrop.Internal.Update exposing (..)

import ImageCrop.Model exposing (Rectangle, Point, Size)


minBy : (a -> comparable) -> a -> a -> a
minBy fn a b =
    if fn b < fn a then
        b
    else
        a


maxBy : (a -> comparable) -> a -> a -> a
maxBy fn a b =
    if fn b > fn a then
        b
    else
        a


atLeast : comparable -> comparable -> comparable
atLeast =
    max


atMost : comparable -> comparable -> comparable
atMost =
    min


rectangleSize : Rectangle -> Size
rectangleSize { topLeft, bottomRight } =
    { width = bottomRight.x - topLeft.x
    , height = bottomRight.y - topLeft.y
    }


scalePoint : Float -> Point -> Point
scalePoint factor point =
    { x = round (toFloat point.x * factor)
    , y = round (toFloat point.y * factor)
    }


scaleRectangle : Float -> Rectangle -> Rectangle
scaleRectangle factor rectangle =
    { topLeft = scalePoint factor rectangle.topLeft
    , bottomRight = scalePoint factor rectangle.bottomRight
    }
