module ComponentTemplate exposing (Model, Msg(..), init, update, view)

import Html exposing (text, div, program, ul)
import Html.Attributes exposing (id, class, style)
import Html.Events exposing (onClick)
import Json.Decode as JD
import Json.Encode as JE
import Random.Pcg as Random
import Time
import Debug
import Task
import WebSocket


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
    { challenge : List String
    , input : List String
    , score : List ( Float, Float )
    , choices : ( List String, List String )
    , appState : AppState
    , seed : Random.Seed
    }


init : ( Model, Cmd Msg )
init =
    let
        a =
            Debug.log "word counts" ( List.length adjectives, List.length nouns )
    in
        { challenge = [ "Rancid", "Felon" ]
        , input = []
        , choices =
            ( [], [] )
            -- get filled in randomly with time
        , score = []
        , appState = Splash
        , seed =
            Random.initialSeed 0
            -- replaced by time
        }
            ! [ Task.perform NewTime Time.now ]


initVocab : Int -> List String -> Random.Generator (List String)
initVocab n vocab =
    let
        vocabSize =
            List.length vocab
    in
        Random.list vocabSize (Random.float 0 1)
            |> Random.map argSort
            |> Random.map (indexList vocab)
            |> Random.map (List.take n)


initChallenge : ( Random.Generator String, Random.Generator String )
initChallenge =
    ( Random.sample adjectives |> Random.map (Maybe.withDefault "random-adjective")
    , Random.sample nouns |> Random.map (Maybe.withDefault "random-noun")
    )


indexList : List a -> List Int -> List a
indexList data index =
    let
        f i =
            case List.drop i data |> List.head of
                Just x ->
                    [ x ]

                Nothing ->
                    []
    in
        index |> List.map f |> List.concat


argSort : List comparable -> List Int
argSort input =
    input
        |> List.indexedMap (\a b -> ( b, a ))
        |> List.sort
        |> List.map Tuple.second


type AppState
    = Splash
    | Prompt
    | Map



-- UPDATE


type Msg
    = Click String
    | ToState AppState
    | NewTime Time.Time
    | Score String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click choice ->
            let
                newInput =
                    model.input ++ [ choice ]

                ( appState, newCmd ) =
                    if ((List.length model.input) % 2) == 1 then
                        ( Map, (requestScore model.challenge newInput model.choices) )
                    else
                        model.appState ! []
            in
                { model
                    | input = newInput
                    , appState = appState
                }
                    ! [ newCmd ]

        ToState appState ->
            case appState of
                Prompt ->
                    if (List.length model.input) == 6 then
                        init
                    else
                        { model | appState = appState } ! []

                _ ->
                    { model | appState = appState } ! []

        NewTime time ->
            let
                seed0 =
                    Random.initialSeed (round time)

                ( gA, gB ) =
                    initChallenge

                ( wordA, seed2 ) =
                    seed0 |> Random.step gA

                ( wordB, seed3 ) =
                    seed2 |> Random.step gB

                newModel =
                    { model
                        | seed = seed3
                        , challenge = [ wordA, wordB ]
                    }

                ( adj, nou, finalSeed ) =
                    getAdjNoun newModel
            in
                { newModel | choices = ( adj, nou ), seed = finalSeed }
                    ! []

        Score s ->
            let
                a =
                    Debug.log "incoming" s

                ( scoreA, scoreB ) =
                    decodeScores s

                ( adj, nou, seed ) =
                    getAdjNoun model
            in
                { model
                    | score = model.score ++ [ ( scoreA, scoreB ) ]
                    , choices = ( adj, nou )
                    , seed = seed
                }
                    ! []


getAdjNoun : Model -> ( List String, List String, Random.Seed )
getAdjNoun model =
    let
        f x =
            not (List.member x (model.input ++ model.challenge))

        adjectivesFilt =
            List.filter f adjectives

        nounsFilt =
            List.filter f nouns

        ( adj, seed1 ) =
            model.seed |> Random.step (initVocab 5 adjectivesFilt)

        ( nou, seed2 ) =
            seed1 |> Random.step (initVocab 5 nounsFilt)
    in
        ( adj, nou, seed2 )



-- REQUEST
-- we are going to ask the server for a score based on the user's choice


type alias Choices =
    ( List String, List String )


decodeScores : String -> ( Float, Float )
decodeScores json =
    let
        decoder x =
            case x of
                Ok y ->
                    y

                Err z ->
                    let
                        a =
                            Debug.log "err" z
                    in
                        -1

        choiceA =
            JD.decodeString (JD.field "scoreA" JD.float) json |> decoder

        choiceB =
            JD.decodeString (JD.field "scoreB" JD.float) json |> decoder
    in
        ( choiceA, choiceB )


requestScore : List String -> List String -> Choices -> Cmd Msg
requestScore challenge input choices =
    let
        listString x =
            List.map JE.string x |> JE.list

        request =
            JE.object
                [ ( "challenge", listString challenge )
                , ( "input", listString input )
                , ( "choicesA", listString (Tuple.first choices) )
                , ( "choicesB", listString (Tuple.second choices) )
                ]
                |> JE.encode 4

        a =
            Debug.log "sent" request
    in
        WebSocket.send "ws://localhost:5000/score" request



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
            String.join " " model.challenge

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
    [ WebSocket.listen "ws://localhost:5000/score" Score ] |> Sub.batch



--     [ AnimationFrame.diffs Animate
--     , Keyboard.downs (keyChange True)
--     , Keyboard.ups (keyChange False)
--     , Window.resizes Resize
--     , Drag.subscriptions DragMsg model.dragModel
--     , Sub.map SpeechMsg (Speech.subscriptions model.speechModel)
--     ]
--         |> Sub.batch
-- DATA


adjectives : List String
adjectives =
    [ "apprentice"
    , "craven"
    , "defenceless"
    , "orthodox"
    , "alcoholic"
    , "antediluvian"
    , "armchair"
    , "asthmatic"
    , "atrophied"
    , "avaricious"
    , "backwater"
    , "backwoods"
    , "bankrupt"
    , "bearded"
    , "bellicose"
    , "belligerent"
    , "bigoted"
    , "botched"
    , "breastfed"
    , "bucktoothed"
    , "cantankerous"
    , "careless"
    , "celebrated"
    , "clamorous"
    , "closet"
    , "clumsy"
    , "common"
    , "constipated"
    , "crass"
    , "daft"
    , "deceased"
    , "defrocked"
    , "deranged"
    , "despicable"
    , "dimwitted"
    , "dirty"
    , "diseased"
    , "disgraced"
    , "disguised"
    , "dishonest"
    , "disingenuous"
    , "disorganized"
    , "drugstore"
    , "embarrassing"
    , "floundering"
    , "forsaken"
    , "foul"
    , "fraudulent"
    , "garish"
    , "gilded"
    , "gutless"
    , "hairless"
    , "humorless"
    , "ignorant"
    , "impotent"
    , "impoverished"
    , "incontinent"
    , "indiscreet"
    , "ineffectual"
    , "insignificant"
    , "insincere"
    , "insufferable"
    , "intellectual"
    , "inveterate"
    , "irrelevant"
    , "jackbooted"
    , "jaded"
    , "jilted"
    , "kowtowing"
    , "licentious"
    , "listless"
    , "litigious"
    , "loathsome"
    , "lonely"
    , "luckless"
    , "malignant"
    , "mean"
    , "meddlesome"
    , "medicated"
    , "melancholy"
    , "misinformed"
    , "mistaken"
    , "monied"
    , "morose"
    , "nauseating"
    , "obsequious"
    , "odious"
    , "officious"
    , "overfunded"
    , "overpaid"
    , "pallid"
    , "poisonous"
    , "political"
    , "pompous"
    , "powdered"
    , "precocious"
    , "predatory"
    , "prejudiced"
    , "preposterous"
    , "reckless"
    , "renegade"
    , "rented"
    , "retired"
    , "scheming"
    , "simpering"
    , "sinister"
    , "sniveling"
    , "spineless"
    , "subservient"
    , "sweaty"
    , "synthetic"
    , "talentless"
    , "tendentious"
    , "treacherous"
    , "tufted"
    , "unctuous"
    , "underhanded"
    , "underqualified"
    , "unelected"
    , "unemployed"
    , "unprincipled"
    , "unregenerate"
    , "unsuccessful"
    , "unwashed"
    , "unwitting"
    , "variegated"
    , "vindictive"
    , "worthless"
    , "wretched"
    ]


nouns : List String
nouns =
    [ "carrion"
    , "fop"
    , "fakir"
    , "halfwit"
    , "highwayman"
    , "judas"
    , "marxist"
    , "monk"
    , "moocher"
    , "mystic"
    , "poltergeist"
    , "regurgitator"
    , "trotskyite"
    , "undertaker"
    , "yahoo"
    , "abuser"
    , "academic"
    , "accomplice"
    , "amateur"
    , "apostate"
    , "aristocrat"
    , "arsonist"
    , "assassin"
    , "atheist"
    , "backstabber"
    , "bastard"
    , "betrayer"
    , "bigmouth"
    , "boob"
    , "caveman"
    , "centipede"
    , "cheater"
    , "clerk"
    , "collaborator"
    , "communist"
    , "concubine"
    , "coward"
    , "crackpot"
    , "creature"
    , "cretin"
    , "criminal"
    , "cuckold"
    , "cultist"
    , "demagogue"
    , "dignitary"
    , "drunk"
    , "dwarf"
    , "enigma"
    , "eunuch"
    , "executioner"
    , "experiment"
    , "failure"
    , "fanatic"
    , "fascist"
    , "fetishist"
    , "filth"
    , "fool"
    , "functionary"
    , "garbageman"
    , "gelding"
    , "goon"
    , "gravedigger"
    , "hack"
    , "hitman"
    , "huckster"
    , "hypocrite"
    , "idealist"
    , "individualist"
    , "insect"
    , "invertebrate"
    , "jingoist"
    , "kidnapper"
    , "latecomer"
    , "lawbreaker"
    , "liar"
    , "looter"
    , "loser"
    , "lunatic"
    , "masturbator"
    , "mercenary"
    , "miscreant"
    , "moron"
    , "mortician"
    , "murderer"
    , "narcissist"
    , "neophyte"
    , "nincompoop"
    , "parasite"
    , "peasant"
    , "pederast"
    , "philanthropist"
    , "philistine"
    , "philosopher"
    , "pilferer"
    , "pimp"
    , "plagiarizer"
    , "poltroon"
    , "pornographer"
    , "priest"
    , "pushover"
    , "pygmy"
    , "racist"
    , "rapist"
    , "rapist"
    , "reactionary"
    , "redneck"
    , "reptile"
    , "riffraff"
    , "scoundrel"
    , "scum"
    , "sinner"
    , "soothsayer"
    , "sponge"
    , "spook"
    , "squatter"
    , "stenographer"
    , "swindler"
    , "swineherd"
    , "sycophant"
    , "thug"
    , "traitor"
    , "transient"
    , "trapper"
    , "trash"
    , "turd"
    , "turncoat"
    , "tyrant"
    , "upstart"
    , "vandal"
    , "villain"
    , "warlord"
    , "weakling"
    , "weasel"
    , "wimp"
    , "witchdoctor"
    , "zealot"
    ]
