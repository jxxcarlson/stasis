module WorldGridTest exposing (suite)

import CellGrid exposing (..)
import Expect exposing (Expectation)
import Test exposing (..)
import World exposing (Resource(..))
import WorldGrid exposing (..)


suite : Test
suite =
    describe "The WorldGrid module"
        [ test "indicesOfCellsOfGivenState" <|
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
                        WorldGrid.indicesOfCellsOfGivenState (Occupied Nature) cg

                    goodIndices =
                        [ ( 1, Occupied Nature ), ( 4, Occupied Nature ) ]

                    --         WorldGrid.indicesOfCellsOfResourceType Nature cg
                in
                Expect.equal testIndices goodIndices
        , test "matrixIndicesOfGivenState" <|
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
                        WorldGrid.matrixIndicesOfGivenState (Occupied Nature) cg

                    goodIndices =
                        [ ( 0, 1 ), ( 1, 1 ) ]
                in
                Expect.equal testIndices goodIndices
        , test "neighborsOfGivenState" <|
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
                        WorldGrid.neighborsOfGivenState (Occupied Nature) cg

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
                        WorldGrid.neighborsOfGivenState (Occupied Nature) cg
                            |> WorldGrid.filterVacant cg

                    goodIndices =
                        [ ( 2, 2 ), ( 1, 0 ), ( 0, 0 ) ]
                in
                Expect.equal testIndices goodIndices
        , test "changeFractionOfGivenState" <|
            \_ ->
                let
                    cg =
                        CellGrid.fromList 3
                            3
                            [ Unoccupied
                            , Occupied Nature
                            , Occupied Crop
                            , Unoccupied
                            , Occupied Crop
                            , Occupied Crop
                            , Occupied Crop
                            , Occupied City
                            , Unoccupied
                            ]
                            |> Maybe.withDefault CellGrid.empty

                    cg2 =
                        CellGrid.fromList 3
                            3
                            [ Unoccupied
                            , Occupied Nature
                            , Unoccupied
                            , Unoccupied
                            , Unoccupied
                            , Occupied Crop
                            , Occupied Crop
                            , Occupied City
                            , Unoccupied
                            ]
                            |> Maybe.withDefault CellGrid.empty

                    result =
                        WorldGrid.changeFractionOfGivenState 0.2 0.5 (Occupied Crop) Unoccupied cg
                            |> Tuple.second
                in
                Expect.equal result cg2

        --
        ]
