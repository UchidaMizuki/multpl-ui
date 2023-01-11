module Game exposing (Hand, Play, Played, generatorPlay)

import Dict exposing (Dict)
import List
import List.Cartesian as Cartesian
import List.Nonempty as Nonempty
import Random


type alias Hand =
    Dict Int Int


type alias Played =
    { first : Maybe Int
    , second : Maybe Int
    }


type alias Play =
    { first : Int
    , second : Int
    }

generatorPlay : Hand -> Int -> Hand -> Random.Generator (Maybe Play)
generatorPlay handOpponent fieldFirst handYour =
    case combination handOpponent fieldFirst handYour of
        head :: tail ->
            Nonempty.Nonempty head tail
                |> Nonempty.map Just
                |> Nonempty.sample

        [] ->
            Random.constant Nothing


combination : Hand -> Int -> Hand -> List Play
combination handOpponent fieldFirst handYour =
    let
        keys : List Int
        keys =
            Dict.keys handOpponent

        handYourList : List Int
        handYourList =
            handYour
                |> Dict.toList
                |> List.map
                    (\( rank, number ) ->
                        List.repeat number rank
                    )
                |> List.concat
    in
    Cartesian.map2
        (\first second ->
            { first = first
            , second = second
            }
        )
        keys
        keys
        |> List.filter
            (\play ->
                let
                    first =
                        play.first

                    second =
                        play.second
                in
                if first == second then
                    case Dict.get first handOpponent of
                        Just number ->
                            number >= 2

                        Nothing ->
                            False

                else
                    True
            )
        |> List.filter
            (\play ->
                modBy fieldFirst (play.first + play.second) == 0
            )
        |> List.filter
            (\play ->
                if List.length handYourList == 2 then
                    modBy (min play.first play.second) (List.sum handYourList) /= 0
                else
                    True
            )
