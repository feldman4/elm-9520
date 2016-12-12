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
    | Game



-- UPDATE


type Msg
    = Click String
    | ToGame
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click choice ->
            { model | input = model.input ++ [ choice ] }
                ! []

        ToGame ->
            { model | appState = Game } ! []

        NoOp ->
            model ! []



-- VIEW


view : Model -> Html.Html Msg
view model =
    case model.appState of
        Splash ->
            viewSplash model

        Game ->
            viewGame model


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
        div [ id "splash", onClick ToGame ]
            [ div [ id "intro-1" ] [ text introString1 ]
            , daveDiv
            , div [ id "intro-2" ] [ text introString2 ]
            , impostorDiv
            , div [ id "intro-3" ] [ text introString3 ]
            ]


viewGame : Model -> Html.Html Msg
viewGame model =
    let
        scoreString =
            "score: " ++ (toString model.score)

        challengeString =
            toString model.challenge

        choices =
            if ((List.length model.input) % 2) == 0 then
                Tuple.first model.choices
            else
                Tuple.second model.choices

        choicesList =
            choices
                |> List.map (\s -> ul [ class "word", onClick (Click s) ] [ text s ])

        inputString =
            "History: " ++ (String.join ", " model.input)
    in
        div [ id "main" ]
            ([ div [ id "challenge" ] [ text challengeString ]
             , div [ id "score" ] [ text scoreString ]
             ]
                ++ choicesList
                ++ [ div [ id "input" ] [ text inputString ] ]
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
