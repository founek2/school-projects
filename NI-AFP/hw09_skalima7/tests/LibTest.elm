module LibTest exposing
    ( testDoubleFirst1
    , testDoubleFirst2
    , testFilterTrianglesByType
    , testFilterValidTriangles
    , testMakeTriangles
    , testParseValues
    , testSortTrianglesByArea
    , testSumTrianglesArea
    )

import Dict
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Lib exposing (..)
import Test exposing (..)


testMakeTriangles : Test
testMakeTriangles =
    describe "makeTriangles"
        [ test "creates triangles" <|
            \_ ->
                let
                    triangles =
                        makeTriangles
                            [ 1, 2, 3 ]
                            [ 4, 5, 6 ]
                            [ 7, 8, 9 ]
                in
                Expect.equal triangles [ Triangle 1 4 7, Triangle 2 5 8, Triangle 3 6 9 ]
        ]


testFilterValidTriangles : Test
testFilterValidTriangles =
    describe "filterValidTriangles"
        [ test "filter triangles" <|
            \_ ->
                let
                    triangles =
                        filterValidTriangles <|
                            makeTriangles
                                [ 10, 45, 18, 0, -8 ]
                                [ 12, 10, 18, 6, -9 ]
                                [ 22, 45, 18, 9, -7 ]
                in
                Expect.equal triangles
                    [ Triangle 45 10 45, Triangle 18 18 18 ]
        ]


testFilterTrianglesByType : Test
testFilterTrianglesByType =
    describe "filterTrianglesByType"
        [ test "filter right-angled triangles" <|
            \_ ->
                let
                    triangles =
                        filterTrianglesByType RightAngled <|
                            makeTriangles
                                [ 4, 12, 12, 0, 7, 5 ]
                                [ 3, 56, 5, 6, 24, 6 ]
                                [ 5, -6, 13, 7, 25, 7 ]
                in
                Expect.equal triangles
                    [ Triangle 4 3 5, Triangle 12 5 13, Triangle 7 24 25 ]
        , test "filter isosceles triangles" <|
            \_ ->
                let
                    triangles =
                        filterTrianglesByType Isosceles <|
                            makeTriangles
                                [ 5, 323, 54, -8, 0, 6 ]
                                [ 23, 128, 54, 9, 2, 6 ]
                                [ 23, 462, 12, 9, 3, 6 ]
                in
                Expect.equal triangles
                    [ Triangle 5 23 23, Triangle 54 54 12, Triangle 6 6 6 ]
        , test "filter equiliteral triangles" <|
            \_ ->
                let
                    triangles =
                        filterTrianglesByType Equilateral <|
                            makeTriangles
                                [ 0, 7, -2, 3, 123, 121 ]
                                [ 0, 7, -2, 6, 5, 121 ]
                                [ 0, 7, -2, 9, 8, 121 ]
                in
                Expect.equal triangles
                    [ Triangle 7 7 7, Triangle 121 121 121 ]
        ]


testSortTrianglesByArea : Test
testSortTrianglesByArea =
    describe "sortTrianglesByArea"
        [ test "sorts" <|
            \_ ->
                let
                    triangles =
                        [ Triangle 2 4 3
                        , Triangle 10 12 21
                        , Triangle 5 6 7
                        , Triangle 6 7 4
                        ]

                    sorted =
                        [ Triangle 2 4 3
                        , Triangle 6 7 4
                        , Triangle 5 6 7
                        , Triangle 10 12 21
                        ]
                in
                Expect.equal sorted <| sortTrianglesByArea triangles
        ]


testSumTrianglesArea : Test
testSumTrianglesArea =
    describe "sumTriangleArea"
        [ test "sums the area" <|
            \_ ->
                let
                    triangles =
                        [ Triangle 2 4 3
                        , Triangle 10 12 21
                        , Triangle 5 6 7
                        , Triangle 6 7 4
                        ]
                in
                Expect.within (Absolute 0.001) 63.848 <| sumTrianglesArea triangles
        , test "sums empty list" <|
            \_ -> Expect.equal 0 <| sumTrianglesArea []
        ]


testDoubleFirst1 : Test
testDoubleFirst1 =
    testDoubleFirst "doubleFirst1" doubleFirst1


testDoubleFirst2 : Test
testDoubleFirst2 =
    testDoubleFirst "doubleFirst2" doubleFirst2


testDoubleFirst : String -> (Maybe (List Int) -> Int) -> Test
testDoubleFirst name doubleFirst =
    describe name
        [ parametrized
            [ ( 4, [ 2, 5, 1 ] )
            , ( -18, [ -9, 0 ] )
            , ( 0, [ 0, 14, 55, 31 ] )
            ]
            "doubles first number"
          <|
            \_ -> Expect.equal 4 <| doubleFirst <| Just [ 2, 5, 1 ]
        , test "returns 0 for empty list" <|
            \_ -> Expect.equal 0 <| doubleFirst <| Just []
        , test "returns 0 when list is Nothing" <|
            \_ -> Expect.equal 0 <| doubleFirst Nothing
        ]


testParseValues : Test
testParseValues =
    describe "parseValues"
        [ parametrized
            [ ( "a = 12.5\nb = 6.7\n\ntx = 134.76"
              , [ ( "a", 12.5 ), ( "b", 6.7 ), ( "tx", 134.76 ) ]
              )
            , ( "x=5", [ ( "x", 5 ) ] )
            , ( "y           =   78\n\n\n\n\n\n     z=\t\t65.3"
              , [ ( "y", 78 ), ( "z", 65.3 ) ]
              )
            , ( "", [] )
            ]
            "works for valid data"
          <|
            \( source, expectedData ) ->
                case parseValues source of
                    Err err ->
                        Expect.fail err

                    Ok parsedValues ->
                        Expect.equalDicts parsedValues <| Dict.fromList expectedData
        , test "returns an error for invalid string" <|
            \_ ->
                Expect.err <| parseValues "a = abc; c = 15"
        ]


testIndexOf : Test
testIndexOf =
    describe "indexof"
        [ test "returns correct index for ints" <|
            \_ -> Expect.equal (Just 1) <| indexOf 2 [ 1, 2, 3, 4, 5, 6 ]
        , test "returns nothing for ints" <|
            \_ -> Expect.equal Nothing <| indexOf 5 [ 1, 1, 1, 1 ]
        , test "returns correct index for strings" <|
            \_ -> Expect.equal (Just 2) <| indexOf "abc" [ "bca", "cbc", "abc", "acc" ]
        ]


parametrized : List a -> String -> (a -> Expectation) -> Test
parametrized fixtures desc testFunction =
    describe desc
        (List.indexedMap (\i f -> test (desc ++ " " ++ String.fromInt i) (\_ -> testFunction f)) fixtures)
