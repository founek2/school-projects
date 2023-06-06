module Lib exposing
    ( Triangle
    , TriangleType(..)
    , doubleFirst1
    , doubleFirst2
    , filterTrianglesByType
    , filterValidTriangles
    , indexOf
    , makeTriangles
    , parseValues
    , sortTrianglesByArea
    , sumTrianglesArea
    )

import Dict exposing (Dict)
import List.Extra
import Result.Extra


{-| Represents a triangle with sides a, b, c.
-}
type alias Triangle =
    { a : Float
    , b : Float
    , c : Float
    }


{-| Defines the type of a triangle
-}
type TriangleType
    = RightAngled
    | Isosceles
    | Equilateral


{-| Write a function that lists of sides (as, bs and cs) and creates a list of
triangles from those sides.
-}
isValidTriangle : Triangle -> Bool
isValidTriangle { a, b, c } =
    a
        > 0
        && b
        > 0
        && c
        > 0
        && (a + b > c && a + c > b && b + c > a)



-- makeTriangle : Float -> Float -> Float -> Maybe Triangle
-- makeTriangle a b c =
--     if isValidTriangle (Triangle a b c) then
--         Just (Triangle a b c)
--     else
--         Nothing
-- appendJust : Maybe a -> List a -> List a
-- appendJust maybe list =
--     maybe
--         |> Maybe.map List.singleton
--         |> Maybe.withDefault []
--         |> (\x -> List.append x list)


makeTriangles : List Float -> List Float -> List Float -> List Triangle
makeTriangles xs ys zs =
    case ( xs, ys, zs ) of
        ( a :: xs2, b :: ys2, c :: zs2 ) ->
            List.append [ Triangle a b c ] (makeTriangles xs2 ys2 zs2)

        _ ->
            []


{-| Write a function that takes a list of triangles and returns a list of only
valid triangles.
-}
filterValidTriangles : List Triangle -> List Triangle
filterValidTriangles triangles =
    List.filter isValidTriangle triangles


{-| Write a function that takes a triangle type and a list of triangles and
returns a list of triangles that conforms the given type.
-}
isEquilateral : Triangle -> Bool
isEquilateral { a, b, c } =
    a == b && b == c && a == c && a > 0 && b >= 0 && c >= 0


pythagoras : Float -> Float -> Float -> Bool
pythagoras a b c =
    a ^ 2 + b ^ 2 == c ^ 2


isRightAngled : Triangle -> Bool
isRightAngled { a, b, c } =
    pythagoras a b c || pythagoras c a b || pythagoras b c a


isIsosceles : Triangle -> Bool
isIsosceles { a, b, c } =
    (a == b || b == c || a == c) && a > 0 && b >= 0 && c >= 0


filterTrianglesByType : TriangleType -> List Triangle -> List Triangle
filterTrianglesByType triangleType =
    let
        cmp =
            case triangleType of
                Equilateral ->
                    isEquilateral

                RightAngled ->
                    isRightAngled

                Isosceles ->
                    isIsosceles
    in
    List.filter cmp


{-| Write a function that takes a list of triangles and returns a list of
triangles sorted by their area.
-}
sortTrianglesByArea : List Triangle -> List Triangle
sortTrianglesByArea =
    List.sortBy triangleArea


{-| Write a function that takes a list of triangles and returns their area
-}
triangleArea : Triangle -> Float
triangleArea { a, b, c } =
    let
        s =
            (a + b + c) / 2
    in
    sqrt (s * (s - a) * (s - b) * (s - c))


sumTrianglesArea : List Triangle -> Float
sumTrianglesArea triangles =
    triangles
        |> List.map triangleArea
        |> List.sum


{-| Write a function that has argument of type Maybe (List Int) and returns
the first number of the list mulitplied by 2.
If the list is Nothing or empty, it should return 0.

Do not use pattern matching or conditions, use helper functions from Maybe
package and |> operator to chain them together.

-}
doubleFirst1 : Maybe (List Int) -> Int
doubleFirst1 maybeList =
    maybeList
        |> Maybe.withDefault []
        |> List.head
        |> Maybe.withDefault 0
        |> (*) 2


{-| Write a function that works in the same way as doubleFirst1 but use point free
style and >> or << operators to chain needed functions together.
-}
doubleFirst2 : Maybe (List Int) -> Int
doubleFirst2 =
    (*) 2 << Maybe.withDefault 0 << List.head << Maybe.withDefault []


{-| Write a function that takes a string that contains property names and
float values e.g.:

""""
a = 12.5
b = 5.4

tx = 18
"""

It converts the string into a dict with string keys and float values. Empty
lines and whitespaces should be skipped. The return value should be wrapped in
result and it should return an error if it cannot be parsed.

-}
parseValues : String -> Result String (Dict String Float)
parseValues str =
    let
        lines =
            String.lines str
                |> List.filter (\x -> String.trim x /= "")
                |> List.map (\line -> String.words <| String.replace "=" " = " line)

        convert : List String -> Result String ( String, Float )
        convert list =
            case list of
                [ key, "=", value ] ->
                    value
                        |> String.toFloat
                        |> Result.fromMaybe "Error parsing float"
                        |> Result.map (\x -> ( key, x ))

                _ ->
                    Err "Invalid format"
    in
    lines
        |> List.map convert
        |> Result.Extra.combine
        |> Result.map (\list -> List.foldr (\( key, value ) store -> Dict.insert key value store) Dict.empty list)


{-| This function should return an index of the given element in the list. If
the element does not exist, it should return Nothing.

Do not implement this function by yourself. Find an Elm package that
already has the function and use it here.

-}
indexOf : a -> List a -> Maybe Int
indexOf =
    List.Extra.elemIndex
