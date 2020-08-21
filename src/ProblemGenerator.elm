module ProblemGenerator exposing (..)

import Browser
import Html exposing (Attribute, Html, button, div, input, span, text)
import Html.Attributes exposing (id, type_, value)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode as Json
import Random

type Operator
    = Plus
    | Minus
    | Times
    | Divide

operatorGenerator : Random.Generator Operator
operatorGenerator =
    Random.uniform Plus [ Minus, Times, Divide ]

type alias Question =
    { operand1 : Int
    , operand2 : Int
    , operator : Operator
    , answer : Answer
    }

type alias QuestionParts =
    { num1 : Int
    , num2 : Int
    , operator : Operator
    }

type alias Score =
    { correct : Int
    , attempts : Int
    }

type Answer
    = Answer Int
    | NoAnswer

type alias Model =
    { question : Question
    , score : Score
    , answer : Answer
    }

type Msg
    = NewQuestionParts QuestionParts
    | SaveAnswer String
    | KeyDown Int
    | SubmitPressed

minNum = 1

maxNum = 12

generateRandomQuestionParts : Random.Generator QuestionParts
generateRandomQuestionParts =
    Random.map3 QuestionParts
        (Random.int minNum maxNum)
        (Random.int minNum maxNum)
        operatorGenerator

view : Model -> Html Msg
view model =
    let
        op1 = model.question.operand1
        op2 = model.question.operand2
        opr = model.question.operator
    in
    div []
        [ div []
            [ questionView op1 op2 opr
            , viewAnswer model.answer
            ]
        , viewScore model.score
        ]


operatorToString : Operator -> String
operatorToString op =
    case op of
        Plus -> "+"
        Minus -> "-"
        Times -> "x"
        Divide -> "/"

questionView : Int -> Int -> Operator -> Html Msg
questionView n1 n2 opr =
    text
        ((String.fromInt n1)
        ++ " "
        ++ (operatorToString opr)
        ++ " "
        ++ (String.fromInt n2)
        ++ " = "
        )

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Json.map tagger keyCode)

viewAnswer : Answer -> Html Msg
viewAnswer userAnswer =
    case userAnswer of
        Answer num ->
            span []
                [ input
                    [ id "answer"
                    , type_ "answer"
                    , onInput SaveAnswer
                    , onKeyDown KeyDown
                    , value (String.fromInt num)
                    ]
                    []
                , button
                     [ type_ "submit"
                     , onClick SubmitPressed
                     ]
                     [ text "Submit Answer" ]
                ]
        NoAnswer ->
            span []
             [ input
                 [ id "answer"
                 , type_ "answer"
                 , onInput SaveAnswer
                 , onKeyDown KeyDown
                 ]
                 []
             , button
                  [ type_ "submit"
                  , onClick SubmitPressed
                  ]
                  [ text "Submit Answer" ]
             ]



viewScore : Score -> Html Msg
viewScore score =
    div []
        [ text (
            "Score: "
            ++ String.fromInt score.correct
            ++ "/"
            ++ String.fromInt score.attempts
            )
        ]

generateRandomQuestion : Model -> ( Model, Cmd Msg )
generateRandomQuestion model =
    ( model, Random.generate NewQuestionParts generateRandomQuestionParts )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --GenerateRandomQuestionParts ->
        --    ( model, Random.generate NewQuestionParts generateRandomQuestionParts )

        NewQuestionParts questionParts ->
            ( Model (makeQuestion questionParts) model.score NoAnswer, Cmd.none)

        SaveAnswer ans ->
            ( { model | answer = stringToAnswer ans}, Cmd.none)

        KeyDown key ->
            if key == 13 then
                checkAnswer model
            else
                ( model, Cmd.none )

        SubmitPressed ->
            checkAnswer model

checkAnswer : Model -> ( Model, Cmd Msg )
checkAnswer model =
    let
        userAnswer = model.answer
        questionAns = model.question.answer

        rightAnswer =
            ( { model | score = updateScore model.score 1 1}
            , Random.generate NewQuestionParts generateRandomQuestionParts
            )

        wrongAnswer =
            ( { model | score = updateScore model.score 0 1}
            , Cmd.none
            )
    in
    case (questionAns, userAnswer) of
        (Answer a, Answer b) ->
            if a == b then
                rightAnswer
            else
                wrongAnswer
        (Answer a, NoAnswer) ->
            wrongAnswer
        (NoAnswer, _) ->
            generateRandomQuestion model

updateScore : Score -> Int -> Int -> Score
updateScore s addToCorrect addToAttempts =
    { s
    | correct = s.correct + addToCorrect
    , attempts = s.attempts + addToAttempts
    }

makeQuestion : QuestionParts -> Question
makeQuestion questionParts =
    let
        n1 = questionParts.num1
        n2 = questionParts.num2

        makeMinusQuestion a b =
            if a > b then
                Question
                    n1
                    n2
                    questionParts.operator
                    (Answer (n1 - n2))
            else
                Question
                    n2
                    n1
                    questionParts.operator
                    (Answer (n2 - n1))
    in
    case questionParts.operator of
        Plus ->
            Question
                n1
                n2
                questionParts.operator
                (Answer (n1 + n2))
        Minus ->
            makeMinusQuestion n1 n2
        Times ->
            Question
                n1
                n2
                questionParts.operator
                (Answer (n1 * n2))
        Divide ->
            Question
                (n1 * n2)
                n1
                questionParts.operator
                (Answer n2)

stringToAnswer : String -> Answer
stringToAnswer s =
    let
        i = String.toInt s
    in
    case i of
        Just a ->
            Answer a
        Nothing ->
            NoAnswer


initialQuestion : Question
initialQuestion =
    Question 0 0 Plus NoAnswer

initialScore : Score
initialScore =
    Score 0 0

initialModel : Model
initialModel =
    Model initialQuestion initialScore NoAnswer

init : () -> ( Model, Cmd Msg )
init _ =
    generateRandomQuestion initialModel

main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }