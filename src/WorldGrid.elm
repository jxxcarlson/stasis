module WorldGrid exposing
    ( State(..)
    , emptyGrid
    , indicesOfCellsOfResourceType
    , indicesOfVacantCells
    , matrixIndicesOfSameResource
    , neighborsOfSameResource
    , numberOccupied
    , toggleState
    , unique
    , updateCells
    )

import Array exposing (Array)
import CellGrid exposing (CellGrid(..), CellType(..), cellAtMatrixIndex)
import Maybe.Extra
import Random
import World exposing (Resource)


type State
    = Occupied Resource
    | Unoccupied


emptyGrid : Int -> Int -> CellGrid State
emptyGrid rows cols =
    CellGrid.fromList rows cols (List.repeat (rows * cols) Unoccupied)
        |> Maybe.withDefault CellGrid.empty


updateCells : CellGrid State -> CellGrid State
updateCells cellGrid =
    cellGrid


toggleState : State -> ( Int, Int ) -> CellGrid State -> CellGrid State
toggleState newState ( i, j ) cg =
    case CellGrid.cellAtMatrixIndex ( i, j ) cg of
        Nothing ->
            cg

        Just state ->
            if state == newState then
                CellGrid.setValue cg ( i, j ) Unoccupied

            else
                CellGrid.setValue cg ( i, j ) newState


gen : Int -> ( Float, Float ) -> Random.Generator (List Float)
gen n ( a, b ) =
    Random.list n (Random.float a b)


makeSeed : Int -> Random.Seed
makeSeed k =
    Random.initialSeed k


cellSequence_ : Int -> Random.Seed -> ( Float, Float ) -> ( List Float, Random.Seed )
cellSequence_ n seed ( a, b ) =
    Random.step (gen n ( a, b )) seed


numberVacant : CellGrid State -> Int
numberVacant (CellGrid ( _, _ ) cells) =
    cells
        |> Array.filter (\state -> state == Unoccupied)
        |> Array.length


numberOccupied : CellGrid State -> Int
numberOccupied (CellGrid ( _, _ ) cells) =
    cells
        |> Array.filter (\state -> state /= Unoccupied)
        |> Array.length


indicesOfVacantCells : CellGrid State -> Array ( Int, Int )
indicesOfVacantCells (CellGrid ( nRows, nCols ) cells) =
    cells
        |> Array.indexedMap (\k state -> ( k, state ))
        |> Array.filter (\( k, state ) -> state == Unoccupied)
        |> Array.map (\( k, state ) -> k)
        |> Array.map (\k -> CellGrid.matrixIndex ( nRows, nCols ) k)


indicesOfCellsOfResourceType : Resource -> CellGrid.CellGrid State -> List ( Int, State )
indicesOfCellsOfResourceType resource (CellGrid ( nRows, nCols ) cells) =
    cells
        |> Array.indexedMap (\k state -> ( k, state ))
        |> Array.filter (\( k, state ) -> state == Occupied resource)
        |> Array.toList


matrixIndicesOfSameResource : Resource -> CellGrid.CellGrid State -> List ( Int, Int )
matrixIndicesOfSameResource resource ((CellGrid ( nRows, nCols ) cells) as cg) =
    let
        indices =
            indicesOfCellsOfResourceType resource cg
                |> List.map (Tuple.first >> CellGrid.matrixIndex ( nRows, nCols ))
    in
    indices


neighborsOfSameResource : Resource -> CellGrid.CellGrid State -> List ( Int, Int )
neighborsOfSameResource resource ((CellGrid ( nRows, nCols ) cells) as cg) =
    let
        indices =
            matrixIndicesOfSameResource resource cg
    in
    indices
        |> List.map neighborIndices
        |> List.concat
        |> List.filter (\( x, y ) -> x >= 0 && y >= 0)
        |> unique


unique : List comparable -> List comparable
unique list =
    list
        |> List.sort
        |> List.foldl
            (\item acc ->
                if Just item /= List.head acc then
                    item :: acc

                else
                    acc
            )
            []


neighborIndices : ( Int, Int ) -> List ( Int, Int )
neighborIndices ( x, y ) =
    [ ( x, y + 1 )
    , ( x - 1, y + 1 )
    , ( x - 1, y )
    , ( x - 1, y - 1 )
    , ( x, y - 1 )
    , ( x + 1, y - 1 )
    , ( x + 1, y )
    , ( x + 1, y + 1 )
    ]



-- |> List.map (\(k, state) -> ((CellGrid.matrixIndex ( nRows, nCols ) k), state)
