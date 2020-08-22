module HttpExamples exposing (Model)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (Decoder, Error(..), decodeString, list, string)

type alias Model =
    { names : List String
    , errorMessage : Maybe String
    }

type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error (List String))

url : String
url =
    "http://localhost:5019/nicknames"

nicknamesDecoder : Decoder (List String)
nicknamesDecoder =
    list string

getNames : Cmd Msg
getNames =
    Http.get
        { url = url
        , expect = Http.expectJson DataReceived nicknamesDecoder
        }

view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ]
            [ text "Get Data from server" ]
        , viewNicknameOrError model
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, getNames )
        DataReceived (Ok nicknames) ->
            ( { model | names = nicknames }, Cmd.none )
        DataReceived (Err httpError) ->
            ( { model | errorMessage = Just (buildErrorMessage httpError) }
            , Cmd.none
            )

buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
         Http.BadUrl str ->
             str
         Http.Timeout ->
             "Server is taking too long to respond. Please try again later."
         Http.NetworkError ->
             "Unable to reach server."
         Http.BadStatus statusNum ->
             "Request failed with status code: " ++ String.fromInt statusNum
         Http.BadBody str ->
             str

viewNickname : String -> Html Msg
viewNickname name =
    li [] [ text name ]

viewNicknames : List String -> Html Msg
viewNicknames lsNames =
    div []
        [ h3 [] [ text "Old School Main Characters" ]
        , ul [] (List.map viewNickname lsNames)
        ]

viewError : String -> Html Msg
viewError error =
    let
        errorHeading = "Couldn't fetch nicknames at this time."
    in
        div []
            [ h3 [] [ text errorHeading ]
            , text ("Error: " ++ error)
            ]

viewNicknameOrError : Model -> Html Msg
viewNicknameOrError model =
    case model.errorMessage of
        Nothing ->
            viewNicknames model.names
        Just message ->
            viewError message

init : () -> ( Model, Cmd Msg )
init _ =
    ( { names = []
      , errorMessage = Nothing
      }
    , Cmd.none
    )

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }