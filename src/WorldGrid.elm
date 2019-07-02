module WorldGrid exposing
    ( State(..)
    , changeFractionOfResources
    , emptyGrid
    , filterVacant
    , indicesOfCellsOfResourceType
    , indicesOfVacantCells
    , matrixIndicesOfSameResource
    , neighborsOfSameResource
    , numberOccupied
    , setRandomCell
    , toggleState
    , updateCells
    )

import Array exposing (Array)
import CellGrid exposing (CellGrid(..), CellType(..), cellAtMatrixIndex)
import Maybe.Extra
import Random
import Utility exposing (uniquefyList)
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


changeFractionOfResources : Float -> Float -> Resource -> State -> CellGrid State -> ( Int, CellGrid State )
changeFractionOfResources seed p sourceResource targetState ((CellGrid ( nRows, nCols ) cells) as grid) =
    let
        matrixIndices =
            indicesOfCellsOfResourceType sourceResource grid
                |> List.map Tuple.first
                |> List.map (CellGrid.matrixIndex ( nRows, nCols ))

        chosenMatrixIndices =
            Utility.chooseRandomSubList seed p matrixIndices

        nChosen =
            List.length chosenMatrixIndices

        isChosenMatrixIndex : ( Int, Int ) -> Bool
        isChosenMatrixIndex ( i, j ) =
            List.member ( i, j ) chosenMatrixIndices

        mapper : ( Int, Int ) -> State -> State
        mapper ( i, j ) state =
            if isChosenMatrixIndex ( i, j ) then
                targetState

            else
                state
    in
    ( nChosen, CellGrid.mapWithIndex mapper grid )


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
        |> Utility.uniquefyList


filterVacant : CellGrid State -> List ( Int, Int ) -> List ( Int, Int )
filterVacant ((CellGrid ( nRows, nCols ) cells) as cg) tupleList =
    tupleList
        |> List.filter (\( i, j ) -> CellGrid.cellAtMatrixIndex ( i, j ) cg == Just Unoccupied)


setRandomCell : Float -> Resource -> CellGrid State -> CellGrid State
setRandomCell p resource cellGrid =
    let
        freeIndexTuples =
            neighborsOfSameResource resource cellGrid
                |> filterVacant cellGrid

        n =
            freeIndexTuples
                |> List.length
                |> toFloat

        k =
            (p * n)
                |> round
    in
    case Utility.getListElement k freeIndexTuples of
        Nothing ->
            setRandomCell1 p resource cellGrid

        Just ( i, j ) ->
            CellGrid.setValue cellGrid ( i, j ) (Occupied resource)


setRandomCell1 : Float -> Resource -> CellGrid State -> CellGrid State
setRandomCell1 p resource cellGrid =
    let
        freeIndices =
            indicesOfVacantCells cellGrid

        n =
            freeIndices
                |> Array.length
                |> toFloat

        k =
            (p * n)
                |> round
    in
    case Array.get k freeIndices of
        Nothing ->
            cellGrid

        Just ( i, j ) ->
            CellGrid.setValue cellGrid ( i, j ) (Occupied resource)


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
