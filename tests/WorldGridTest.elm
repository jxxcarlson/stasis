module WorldGridTest exposing (suite)

import CellGrid exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)
import World exposing (Resource(..))
import WorldGrid exposing (..)


suite : Test
suite =
    describe "The String module"
        [ test "has no effect on a palindrome" <|
            \_ ->
                let
                    cg =
                        CellGrid.fromList 3
                            3
                            [ Unoccupied
                            , Occupied Nature
                            , Unoccupied
                            , Unoccupied
                            , Occupied Nature
                            , Unoccupied
                            , Unoccupied
                            , Unoccupied
                            , Unoccupied
                            ]
                            |> Maybe.withDefault CellGrid.empty

                    testIndices =
                        WorldGrid.indicesOfCellsOfResourceType Nature cg

                    goodIndices =
                        [ ( 1, Occupied Nature ), ( 4, Occupied Nature ) ]

                    --         WorldGrid.indicesOfCellsOfResourceType Nature cg
                in
                Expect.equal testIndices goodIndices
        , test "matrixIndicesOfSameResource" <|
            \_ ->
                let
                    cg =
                        CellGrid.fromList 3
                            3
                            [ Unoccupied
                            , Occupied Nature
                            , Unoccupied
                            , Unoccupied
                            , Occupied Nature
                            , Unoccupied
                            , Unoccupied
                            , Unoccupied
                            , Unoccupied
                            ]
                            |> Maybe.withDefault CellGrid.empty

                    testIndices =
                        WorldGrid.matrixIndicesOfSameResource Nature cg

                    goodIndices =
                        [ ( 0, 1 ), ( 1, 1 ) ]
                in
                Expect.equal testIndices goodIndices
        , test "neighborsOfSameResource" <|
            \_ ->
                let
                    cg =
                        CellGrid.fromList 3
                            3
                            [ Unoccupied
                            , Occupied Nature
                            , Unoccupied
                            , Unoccupied
                            , Occupied Nature
                            , Unoccupied
                            , Unoccupied
                            , Unoccupied
                            , Unoccupied
                            ]
                            |> Maybe.withDefault CellGrid.empty

                    testIndices =
                        WorldGrid.neighborsOfSameResource Nature cg

                    goodIndices =
                        [ ( 2, 2 ), ( 2, 1 ), ( 2, 0 ), ( 1, 2 ), ( 1, 1 ), ( 1, 0 ), ( 0, 2 ), ( 0, 1 ), ( 0, 0 ) ]
                in
                Expect.equal testIndices goodIndices
        , test "unoccupied neighborsOfSameResource" <|
            \_ ->
                let
                    cg =
                        CellGrid.fromList 3
                            3
                            [ Unoccupied
                            , Occupied Nature
                            , Occupied Crop
                            , Unoccupied
                            , Occupied Nature
                            , Occupied Crop
                            , Occupied Crop
                            , Occupied City
                            , Unoccupied
                            ]
                            |> Maybe.withDefault CellGrid.empty

                    testIndices =
                        WorldGrid.neighborsOfSameResource Nature cg
                            |> WorldGrid.filterVacant cg

                    goodIndices =
                        [ ( 2, 2 ), ( 1, 0 ), ( 0, 0 ) ]
                in
                Expect.equal testIndices goodIndices

        --
        ]
