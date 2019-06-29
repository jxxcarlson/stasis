module Main exposing (main)

import Browser
import CellGrid exposing (CellGrid)
import CellGrid.Render exposing (CellRenderer)
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)
import World exposing (Resource(..), World, WorldChange)
import WorldGrid exposing (State(..))


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = viewDocument
        }


type alias Model =
    { stagedWorldChange : WorldChange
    , world : World
    , cellGrid : CellGrid State
    , selectedState : State
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { stagedWorldChange =
            { cities = 0
            , crops = 0
            , nature = 0
            }
      , world =
            World.init
      , cellGrid = CellGrid.empty
      , selectedState = Unoccupied
      }
    , Cmd.none
    )


type Msg
    = ChangeTheWorld
    | StageResource World.Resource
    | CellGrid CellGrid.Render.Msg
    | ChooseCity
    | ChooseCrop
    | ChooseNature
    | ChooseUnoccupied


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTheWorld ->
            ( stageWorldChange model
            , Cmd.none
            )

        StageResource newResource ->
            let
                stagedResource =
                    model.stagedWorldChange
            in
            case newResource of
                World.Nature ->
                    ( { model
                        | stagedWorldChange =
                            { stagedResource | nature = stagedResource.nature + 1 }
                      }
                    , Cmd.none
                    )

                World.Crop ->
                    ( { model
                        | stagedWorldChange =
                            { stagedResource | crops = stagedResource.crops + 1 }
                      }
                    , Cmd.none
                    )

                World.City ->
                    ( { model
                        | stagedWorldChange =
                            { stagedResource | cities = stagedResource.cities + 1 }
                      }
                    , Cmd.none
                    )

        CellGrid msg_ ->
            case msg_ of
                CellGrid.Render.MouseClick ( i, j ) ( x, y ) ->
                    let
                        message =
                            "(i,j) = (" ++ String.fromInt i ++ ", " ++ String.fromInt j ++ ")"

                        maybeSelectedCell =
                            CellGrid.cellAtMatrixIndex ( i, j ) model.cellGrid
                    in
                    ( { model
                        | stagedWorldChange = updateWordChange maybeSelectedCell model.selectedState model.stagedWorldChange
                        , cellGrid = WorldGrid.toggleState model.selectedState ( i, j ) model.cellGrid
                      }
                    , Cmd.none
                    )

        ChooseCity ->
            ( { model | selectedState = Occupied City }, Cmd.none )

        ChooseCrop ->
            ( { model | selectedState = Occupied Crop }, Cmd.none )

        ChooseNature ->
            ( { model | selectedState = Occupied Nature }, Cmd.none )

        ChooseUnoccupied ->
            ( { model | selectedState = Unoccupied }, Cmd.none )


stageWorldChange : Model -> Model
stageWorldChange model =
    { model
        | stagedWorldChange =
            { cities = 0
            , crops = 0
            , nature = 0
            }
        , world =
            model.world ++ [ model.stagedWorldChange ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


viewDocument : Model -> Browser.Document Msg
viewDocument model =
    { title = "Stasis", body = [ view model ] }


view : Model -> Html Msg
view model =
    div
        [ Html.Attributes.style "font-size" "50px"
        ]
        [ turnView model.world
        , model.world
            |> World.view model.stagedWorldChange
            |> Html.map StageResource
        , Html.button
            [ Html.Events.onClick ChangeTheWorld
            , Html.Attributes.style "font-size" "40px"
            ]
            [ text "Change the World! 🌏 🙌" ]
        ]


turnView : World -> Html msg
turnView world =
    text ("Turn number: " ++ String.fromInt (List.length world))



--
-- Added by JC
--


updateWordChange : Maybe State -> State -> WorldChange -> WorldChange
updateWordChange maybeSelectedCell chosenState worldChange =
    if maybeSelectedCell /= Just chosenState then
        case chosenState of
            Occupied Crop ->
                { worldChange | crops = worldChange.crops + 1 }

            Occupied City ->
                { worldChange | cities = worldChange.cities + 1 }

            Occupied Nature ->
                { worldChange | nature = worldChange.nature + 1 }

            Unoccupied ->
                worldChange

    else
        case chosenState of
            Occupied Crop ->
                { worldChange | crops = worldChange.crops - 1 }

            Occupied City ->
                { worldChange | cities = worldChange.cities - 1 }

            Occupied Nature ->
                { worldChange | nature = worldChange.nature - 1 }

            Unoccupied ->
                worldChange
