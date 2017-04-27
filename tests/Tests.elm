module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import ImageCrop.Internal exposing (..)


all : Test
all =
    describe "ImageCrop test suite"
        [ describe "minBy"
            [ test "Simple example" <|
                \() ->
                    let
                        ( a, b ) =
                            twoRectangles
                    in
                        minBy .width a b
                            |> Expect.equal a
            , fuzz int "Simple example fuzzed" <|
                \i ->
                    let
                        ( a, b ) =
                            twoRectanglesWith i
                    in
                        minBy .width a b
                            |> Expect.equal a
            ]
        , describe "maxBy"
            [ test "Simple example" <|
                \() ->
                    let
                        ( a, b ) =
                            twoRectangles
                    in
                        maxBy .width a b
                            |> Expect.equal b
            , fuzz int "Simple example fuzzed" <|
                \i ->
                    let
                        ( a, b ) =
                            twoRectanglesWith i
                    in
                        maxBy .width a b
                            |> Expect.equal b
            ]
        ]


twoRectangles =
    ( { width = 10, height = 25 }
    , { width = 25, height = 10 }
    )


twoRectanglesWith i =
    ( { width = i, height = 25 }
    , { width = i + 1, height = 10 }
    )
