module WorldGrid exposing
    ( State(..)
    , emptyGrid
    , indicesOfVacantCells
    , numberOccupied
    , toggleState
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
