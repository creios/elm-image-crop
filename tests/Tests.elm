module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import ImageCrop.Internal.Update exposing (..)


all : Test
all =
    describe "ImageCrop test suite"
        [ describe "minBy"
            [ test "Simple example" <|
                \() ->
                    let
                        ( a, b ) =
                            ( { width = 10, height = 25 }
                            , { width = 25, height = 10 }
                            )
                    in
                        minBy .width a b
                            |> Expect.equal a
            ]
        , describe "maxBy"
            [ test "Simple example" <|
                \() ->
                    let
                        ( a, b ) =
                            ( { width = 10, height = 25 }
                            , { width = 25, height = 10 }
                            )
                    in
                        maxBy .width a b
                            |> Expect.equal b
            ]
        , describe "atLeast"
            [ test "Below threshold" <|
                \() ->
                    atLeast 0 -5 |> Expect.equal 0
            , test "At threshold" <|
                \() ->
                    atLeast 0 0 |> Expect.equal 0
            , test "Above threshold" <|
                \() ->
                    atLeast 0 5 |> Expect.equal 5
            ]
        , describe "atMost"
            [ test "Below threshold" <|
                \() ->
                    atMost 0 -5 |> Expect.equal -5
            , test "At threshold" <|
                \() ->
                    atMost 0 0 |> Expect.equal 0
            , test "Above threshold" <|
                \() ->
                    atMost 0 5 |> Expect.equal 0
            ]
        ]
