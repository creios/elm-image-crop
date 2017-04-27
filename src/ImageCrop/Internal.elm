module ImageCrop.Internal exposing (..)


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
