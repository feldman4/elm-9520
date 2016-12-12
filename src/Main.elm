module ComponentTemplate exposing (Model, Msg(..), init, update, view)

import Html exposing (text, div, program, ul)
import Html.Attributes exposing (id, class, style)
import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { challenge : ( String, String )
    , input : List String
    , score : Float
    , choices : ( List String, List String )
    , appState : AppState
    }


init : ( Model, Cmd Msg )
init =
    let
        adjectives =
            [ "unctuous", "unwashed", "gutless", "defrocked", "preposterous" ]

        nouns =
            [ "weakling", "backstabber", "wimp", "criminal", "squatter" ]
    in
        { challenge = ( "Rancid", "Felon" )
        , input = []
        , choices = ( adjectives, nouns )
        , score = 0
        , appState = Splash
        }
            ! []


type AppState
    = Splash
    | Prompt
    | Map



-- UPDATE


type Msg
    = Click String
    | ToState AppState
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click choice ->
            let
                appState =
                    if ((List.length model.input) % 2) == 1 then
                        Map
                    else
                        model.appState
            in
                { model
                    | input = model.input ++ [ choice ]
                    , appState = appState
                }
                    ! []

        ToState appState ->
            { model | appState = appState } ! []

        NoOp ->
            model ! []



-- VIEW


view : Model -> Html.Html Msg
view model =
    case model.appState of
        Splash ->
            viewSplash model

        Map ->
            viewMap model

        Prompt ->
            viewPrompt model


viewPrompt : Model -> Html.Html Msg
viewPrompt model =
    let
        choices =
            if ((List.length model.input) % 2) == 0 then
                Tuple.first model.choices
            else
                Tuple.second model.choices

        choicesList =
            choices
                |> List.map (\s -> ul [ class "word", onClick (Click s) ] [ text s ])
    in
        div [ id "prompt" ] choicesList


viewSplash : Model -> Html.Html Msg
viewSplash model =
    let
        introString1 =
            "Meet Dave."

        daveStyle =
            style [ ( "backgroundColor", "red" ) ]

        daveDiv =
            div [ id "intro-dave", daveStyle ] [ text "D" ]

        introString2 =
            "Dave is an impostor. He's really a"

        impostorId =
            (Tuple.first model.challenge) ++ " " ++ (Tuple.second model.challenge)

        impostorDiv =
            div [ id "intro-dave", (style [ ( "color", "green" ) ]) ] [ text impostorId ]

        introString3 =
            "Reveal him in 3 tries."
    in
        div [ id "splash", onClick (ToState Map) ]
            [ div [ id "intro-1" ] [ text introString1 ]
            , daveDiv
            , div [ id "intro-2" ] [ text introString2 ]
            , impostorDiv
            , div [ id "intro-3" ] [ text introString3 ]
            ]


viewMap : Model -> Html.Html Msg
viewMap model =
    let
        scoreString =
            "score: " ++ (toString model.score)

        challengeString =
            toString model.challenge

        inputString =
            "History: " ++ (String.join ", " model.input)
    in
        div [ id "main", onClick (ToState Prompt) ]
            ([ div [ id "challenge" ] [ text challengeString ]
             , div [ id "score" ] [ text scoreString ]
             , div [ id "input" ] [ text inputString ]
             ]
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    [] |> Sub.batch



--     [ AnimationFrame.diffs Animate
--     , Keyboard.downs (keyChange True)
--     , Keyboard.ups (keyChange False)
--     , Window.resizes Resize
--     , Drag.subscriptions DragMsg model.dragModel
--     , Sub.map SpeechMsg (Speech.subscriptions model.speechModel)
--     ]
--         |> Sub.batch
