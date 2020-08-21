module FGORankUpQuestCount exposing (..)

import Browser
import Dict
import Html exposing (Attribute, Html, div, input, text)
import Html.Attributes exposing (checked, style, type_)
import Html.Events exposing (onClick)
import List exposing (foldl)

-- MODEL
type alias Servant
    = String

type alias RankUpQuest =
    { number : Int
    , hasServant : Bool
    }

type alias Model =
    { servants : Dict.Dict Servant RankUpQuest
    , totalQuest : Int
    }

type Msg
    = ToggleServant String

-- VIEW
view : Model -> Html Msg
view model =
    div [style "text-align" "center"]
        [ div []
            [ text ("Number of Quests: " ++ String.fromInt model.totalQuest) ]
        , viewServants model.servants
        ]

viewServants : Dict.Dict Servant RankUpQuest -> Html Msg
viewServants servants =
    let
        viewServant : Servant -> Html Msg
        viewServant servant =
            let
                rankUpQuest : Maybe RankUpQuest
                rankUpQuest =
                    Dict.get servant servants

                hasServant : Bool
                hasServant =
                    case rankUpQuest of
                        Just r ->
                            r.hasServant
                        Nothing ->
                            False
            in
            div []
                [ text (servant ++ " ")
                , input
                    [ type_ "checkbox"
                    , checked hasServant
                    , onClick (ToggleServant servant)
                    ]
                    []
                ]
    in
    div []
        (List.map viewServant (Dict.keys servants))

-- UPDATE
update : Msg -> Model -> Model
update msg model =
    case msg of
        ToggleServant servantToToggle ->
            { servants = toggle servantToToggle model.servants
            , totalQuest = updateTotalQuest servantToToggle model
            }

updateTotalQuest : Servant -> Model -> Int
updateTotalQuest servantToToggle model =
    foldl (+) 0
    <| List.map
        ( \key ->
            if key == servantToToggle then
            case Dict.get key model.servants of
                Just r ->
                    -- since this will be toggled to does not have servant
                    if r.hasServant then
                        0
                    else
                        r.number
                Nothing ->
                    0
            else
            case Dict.get key model.servants of
                Just r ->
                    if r.hasServant then
                        r.number
                    else
                        0
                Nothing ->
                    0
        )
        (Dict.keys model.servants)


toggle : Servant -> Dict.Dict Servant RankUpQuest -> Dict.Dict Servant RankUpQuest
toggle servantToToggle dict =
    Dict.update servantToToggle
        (\maybeValue ->
            case maybeValue of
                Just r ->
                    Just ({r | hasServant = not r.hasServant})
                Nothing ->
                    Nothing
        )
        dict

-- INIT
servantNames : List String
servantNames =
    ["amakusa","arash","artoria","asterios","astolfo","atalante","babbage","benkei","billy","caesar","caligula"
    ,"castergilles","cu","cuproto","dantes","deon","edison","emiyalter","enkidu","euryale","fionnmcCunt","hassanarm"
    ,"hassanserenity","karna","kiara","kidalexander","kiyohime","leonidas","lilyartoria","lubu","marie","matahairi"
    ,"medb","medea","melt","mephisto","mhx","mhxalter","mozart","nightingale","paracleushoenheim","robinhood","romulus"
    ,"sabergilles","sheherezade","tamacat","tesla","xanzhang","zerkerlot","blackbeard","boudica","carmilla","castercu"
    ,"eli","emiya","ericbloodaxe","jingke","medusa","sanson","sasaki","shakespeare","siegfried","stheno","ushi","martha"
    ,"spartacus"]

servantQuestCount : List Int
servantQuestCount =
    [ 1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2
    ,2,2,2,2,2,2,2,3,3]


initServant : String -> Int -> (Servant, RankUpQuest)
initServant name numOfQuest =
    ( name
    , { number = numOfQuest, hasServant = False}
    )

initServants : Dict.Dict Servant RankUpQuest
initServants =
    Dict.fromList
    <| List.map2
        initServant
        servantNames
        servantQuestCount

initModel : Model
initModel =
    { servants = initServants
    , totalQuest = 0
    }

-- MAIN
main : Program () Model Msg
main =
    Browser.sandbox
        { init = initModel
        , view = view
        , update = update
        }
