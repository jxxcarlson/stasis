module Main exposing (main)

import Array
import Browser
import CellGrid exposing (CellGrid)
import CellGrid.Render exposing (CellRenderer)
import Color
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)
import Random
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
    , randomFloat : Float
    }


gridDisplayWidth =
    550.0


gridWidth =
    16


init : () -> ( Model, Cmd Msg )
init _ =
    ( { stagedWorldChange =
            { cities = 0
            , crops = 0
            , nature = 0
            }
      , world =
            World.init
      , cellGrid = WorldGrid.emptyGrid gridWidth gridWidth
      , selectedState = Occupied Crop
      , randomFloat = 0.0
      }
    , Random.generate NewRandomFloat (Random.float 0 1)
    )


type Msg
    = ChangeTheWorld
    | StageResource World.Resource
    | CellGrid CellGrid.Render.Msg
    | ChooseCity
    | ChooseCrop
    | ChooseNature
    | ChooseUnoccupied
    | NewRandomFloat Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTheWorld ->
            ( stageWorldChange model
            , Random.generate NewRandomFloat (Random.float 0 1)
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
                        , cellGrid = WorldGrid.setRandomCell model.randomFloat (Occupied Nature) model.cellGrid
                      }
                    , Random.generate NewRandomFloat (Random.float 0 1)
                    )

                World.Crop ->
                    ( { model
                        | stagedWorldChange =
                            { stagedResource | crops = stagedResource.crops + 1 }
                        , cellGrid = WorldGrid.setRandomCell model.randomFloat (Occupied Crop) model.cellGrid
                      }
                    , Random.generate NewRandomFloat (Random.float 0 1)
                    )

                World.City ->
                    ( { model
                        | stagedWorldChange =
                            { stagedResource | cities = stagedResource.cities + 1 }
                        , cellGrid = WorldGrid.setRandomCell model.randomFloat (Occupied City) model.cellGrid
                      }
                    , Random.generate NewRandomFloat (Random.float 0 1)
                    )

        CellGrid msg_ ->
            case msg_ of
                CellGrid.Render.MouseClick ( i, j ) ( x, y ) ->
                    let
                        message =
                            "(i,j) = (" ++ String.fromInt i ++ ", " ++ String.fromInt j ++ ")"

                        maybeSelectedCell =
                            CellGrid.cellAtMatrixIndex ( i, j ) model.cellGrid

                        ( newStagedWorldChange, newCellGrid ) =
                            case model.selectedState of
                                Unoccupied ->
                                    ( model.stagedWorldChange, model.cellGrid )

                                Occupied resource ->
                                    case World.resourceAvailable resource model.stagedWorldChange model.world of
                                        True ->
                                            ( updateWordChange maybeSelectedCell model.selectedState model.stagedWorldChange
                                            , WorldGrid.toggleState model.selectedState ( i, j ) model.cellGrid
                                            )

                                        False ->
                                            ( model.stagedWorldChange, model.cellGrid )
                    in
                    ( { model
                        | stagedWorldChange = newStagedWorldChange
                        , cellGrid = newCellGrid
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

        NewRandomFloat p ->
            ( { model | randomFloat = p }, Cmd.none )


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
        [ div [ Html.Attributes.style "float" "left" ]
            [ renderGrid model
            , div [ Html.Attributes.style "float" "left" ] [ palette model ]
            ]
        , div [ Html.Attributes.style "float" "left", Html.Attributes.style "margin-left" "20px" ]
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
        ]


turnView : World -> Html msg
turnView world =
    text ("Turn number: " ++ String.fromInt (List.length world))



--
-- Added by JC
--


renderGrid : Model -> Html Msg
renderGrid model =
    let
        _ =
            Debug.log "VACANT" (WorldGrid.indicesOfVacantCells model.cellGrid)
    in
    CellGrid.Render.asHtml
        gridDisplayWidth
        gridDisplayWidth
        cellrenderer
        model.cellGrid
        |> Html.map CellGrid


palette : Model -> Html Msg
palette model =
    div []
        [ div st [ paletteButton model City, text <| String.fromInt model.stagedWorldChange.cities ]
        , div st [ paletteButton model Crop, text <| String.fromInt model.stagedWorldChange.crops ]
        , div st [ paletteButton model Nature, text <| String.fromInt model.stagedWorldChange.nature ]
        ]


paletteButton : Model -> Resource -> Html Msg
paletteButton model resource =
    let
        dimensions =
            if model.selectedState == Occupied resource then
                "70px"

            else
                "60px"
    in
    Html.button
        [ Html.Attributes.style "width" dimensions
        , Html.Attributes.style "height" dimensions
        , Html.Attributes.style "font-color" "white"
        , Html.Attributes.style "background-color" (colorOfResource resource)
        , Html.Attributes.style "margin-right" "10px"
        , Html.Attributes.style "font-size" "30px"
        , Html.Events.onClick (handlerOfResource resource)
        , Html.Attributes.disabled (not (World.resourceAvailable resource model.stagedWorldChange model.world))
        ]
        [ text <| labelForResource resource ]


st =
    [ Html.Attributes.style "margin-right" "10px" ]


handlerOfResource : Resource -> Msg
handlerOfResource resource =
    case resource of
        City ->
            ChooseCity

        Crop ->
            ChooseCrop

        Nature ->
            ChooseNature


colorOfResource : Resource -> String
colorOfResource resource =
    case resource of
        City ->
            "blue"

        Crop ->
            "#ee5"

        Nature ->
            "#5f5"


labelForResource : Resource -> String
labelForResource resource =
    case resource of
        City ->
            World.emojiFromResource City

        Crop ->
            World.emojiFromResource Crop

        Nature ->
            World.emojiFromResource Nature


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


cellrenderer =
    { cellSize = gridDisplayWidth / toFloat gridWidth
    , cellColorizer =
        \state ->
            case state of
                Occupied Crop ->
                    Color.rgb 1 1 0

                Occupied City ->
                    Color.rgb 0 0 1

                Occupied Nature ->
                    Color.rgb 0 1 0

                Unoccupied ->
                    Color.rgb 0 0 0
    , defaultColor = Color.rgb 0 0 0
    , gridLineWidth = 0.5
    , gridLineColor = Color.rgb 0 0 1
    }
