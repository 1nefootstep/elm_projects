module RandomNumber exposing (Model)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random

type Model =
    Model Int Int

type Msg
    = GenerateRandomNumber
    | NewRandomNumbers TwoInt

initialModel : Model
initialModel =
    Model 0 0

init : () -> ( Model, Cmd Msg )
init _ =
    (initialModel, Cmd.none)

view : Model -> Html Msg
view model =
    case model of
        Model n1 n2 ->
            div []
                [ button [ onClick GenerateRandomNumber ]
                    [ text "Generate Random Number" ]
                , text ((String.fromInt n1) ++ " " ++ (String.fromInt n2))
                ]

type alias TwoInt =
    { first : Int
    , second : Int
    }

twoIntGenerator : Random.Generator TwoInt
twoIntGenerator =
    Random.map2 TwoInt (Random.int 0 100) (Random.int 0 100)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateRandomNumber ->
            ( model, Random.generate NewRandomNumbers twoIntGenerator )
        NewRandomNumbers t ->
            ( Model t.first t.second, Cmd.none)
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }