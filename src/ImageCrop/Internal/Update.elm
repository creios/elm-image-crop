module ImageCrop.Internal.Update exposing (..)

import ImageCrop.Model.Point as Point exposing (Point)
import ImageCrop.Model.Rectangle as Rectangle exposing (Rectangle)
import ImageCrop.Model.Size as Size exposing (Size)


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
