module RippleCarryAdder exposing
    ( andGate
    , digits
    , fullAdder
    , halfAdder
    , inverter
    , orGate
    , rippleCarryAdder
    )

import Array
import Bitwise


andGate a b =
    Bitwise.and a b


orGate a b =
    Bitwise.or a b


inverter a =
    case a of
        0 ->
            1

        1 ->
            0

        _ ->
            -1


halfAdder a b =
    let
        d =
            orGate a b

        e =
            andGate a b
                |> inverter

        sumDigit =
            andGate d e

        carryOut =
            andGate a b
    in
    { carry = carryOut
    , sum = sumDigit
    }


fullAdder a b carryIn =
    let
        haOne =
            halfAdder b carryIn

        haTwo =
            halfAdder a haOne.sum

        carryOut =
            orGate haOne.carry haTwo.carry
    in
    { carry = carryOut
    , sum = haTwo.sum
    }


type alias Binary =
    { d0 : Int
    , d1 : Int
    , d2 : Int
    , d3 : Int
    }


rippleCarryAdder a b carryIn =
    let
        firstSignal =
            extractDigits a

        secondSignal =
            extractDigits b

        faOne =
            fullAdder firstSignal.d3 secondSignal.d3 carryIn

        faTwo =
            fullAdder firstSignal.d2 secondSignal.d2 faOne.carry

        faThree =
            fullAdder firstSignal.d1 secondSignal.d1 faTwo.carry

        faFour =
            fullAdder firstSignal.d0 secondSignal.d0 faThree.carry
    in
    [ faFour, faThree, faTwo, faOne ]
        |> List.map .sum
        |> (::) faFour.carry
        |> numberFromDigits


numberFromDigits : List Int -> Int
numberFromDigits digitLs =
    List.foldl (\digit number -> number * 10 + digit) 0 digitLs


extractDigits number =
    digits number
        |> padZeroes 4
        |> Array.fromList
        |> arrayToRecord



--String.fromInt number
--    |> String.split ""
--    |> List.map stringToInt
--    |> Array.fromList
--    |> arrayToRecord


padZeroes total list =
    let
        numZeroes =
            total - List.length list
    in
    List.repeat numZeroes 0 ++ list


stringToInt : String -> Int
stringToInt string =
    String.toInt string
        |> Maybe.withDefault -1


arrayToRecord arr =
    let
        first =
            Array.get 0 arr
                |> Maybe.withDefault -1

        second =
            Array.get 1 arr
                |> Maybe.withDefault -1

        third =
            Array.get 2 arr
                |> Maybe.withDefault -1

        fourth =
            Array.get 3 arr
                |> Maybe.withDefault -1
    in
    { d0 = first
    , d1 = second
    , d2 = third
    , d3 = fourth
    }


digits number =
    let
        helper currNum ls =
            if currNum == 0 then
                ls

            else
                helper (currNum // 10) (remainderBy 10 currNum :: ls)
    in
    helper number []
