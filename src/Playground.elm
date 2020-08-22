module Playground exposing
    ( main
    , signUp
    , escapeVelocity
    , descending
    , MyList(..)
    )
import Html
import Regex

type MyList a
    = Empty
    | Node a (MyList a)


escapeVelocity currentVelocity currentSpeed =
    if currentVelocity > 11.186 then
        "Godspeed"
    else if currentSpeed == 7.67 then
        "Stay in orbit"
    else
        "Come back"

descending a b =
    compare b a

test: Int -> Int -> Int
test a b =
    a + b

signUp : String -> String -> Result String String
signUp email ageStr =
    case String.toInt ageStr of
        Nothing ->
            Err "Age must be an integer."

        Just age ->
            let
                emailPattern =
                    "\\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\b"
                regex =
                    Maybe.withDefault Regex.never <|
                        Regex.fromString emailPattern
                isValidEmail =
                    Regex.contains regex email
            in
            if age < 13 then
                Err "You need to be at least 13 years old to sign up."
            else if isValidEmail then
                Ok "Your account has been created successfully!"
            else
                Err "You entered an invalid email."

main =
    [ 316, 320, 312, 370, 337, 318, 314 ]
        |> List.sortWith descending
        |> Debug.toString
        |> Html.text