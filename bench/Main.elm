module Main exposing (main)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmark exposing (Benchmark)
import Array.Hamt as Array exposing (Array)
import Array.JsArray as JsArray


main : BenchmarkProgram
main =
    program <| suite 100


suite : Int -> Benchmark
suite n =
    let
        ( ls, arr ) =
            setup n ( [], Array.empty )

        pred =
            findPredicate n
    in
        Benchmark.describe (toString n ++ " elements")
            [ Benchmark.compare "idealImpl"
                "List"
                (\_ -> listFind pred ls)
                "Array"
                (\_ -> arrayFind pred arr)
            , Benchmark.compare "arrayImpl"
                "Stoppable"
                (\_ -> arrayFind pred arr)
                "Foldl"
                (\_ -> arrayFindSimple pred arr)
            , Benchmark.compare "simpleImpl"
                "List"
                (\_ -> listFindSimple pred ls)
                "Array"
                (\_ -> arrayFindSimple pred arr)
            ]


type alias TestSubject =
    { name : String
    , recievesPlacebo : Bool
    }


subjectName : Int -> String
subjectName n =
    "subject#" ++ toString n


testSubject : Int -> TestSubject
testSubject n =
    { name = subjectName n
    , recievesPlacebo = n % 2 == 0
    }


setup : Int -> ( List TestSubject, Array TestSubject ) -> ( List TestSubject, Array TestSubject )
setup n ( lsAcc, arrAcc ) =
    ( List.range 0 (n - 1)
        |> List.map testSubject
    , Array.initialize n testSubject
    )


findPredicate : Int -> TestSubject -> Bool
findPredicate n =
    let
        half =
            n // 2

        name =
            subjectName half
    in
        (\ts -> not ts.recievesPlacebo && ts.name == name)


listFind : (a -> Bool) -> List a -> Maybe a
listFind pred ls =
    case ls of
        [] ->
            Nothing

        x :: xs ->
            if pred x then
                Just x
            else
                listFind pred xs


arrayFind : (a -> Bool) -> Array a -> Maybe a
arrayFind pred arr =
    Array.stoppableFoldl
        (\item acc ->
            if pred item then
                JsArray.Done (Just item)
            else
                JsArray.Continue acc
        )
        Nothing
        arr


listFindSimple : (a -> Bool) -> List a -> Maybe a
listFindSimple pred ls =
    List.foldl
        (\item acc ->
            if acc == Nothing && pred item then
                (Just item)
            else
                acc
        )
        Nothing
        ls


arrayFindSimple : (a -> Bool) -> Array a -> Maybe a
arrayFindSimple pred arr =
    Array.foldl
        (\item acc ->
            if acc == Nothing && pred item then
                (Just item)
            else
                acc
        )
        Nothing
        arr
