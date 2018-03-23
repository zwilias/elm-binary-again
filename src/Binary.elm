module Binary exposing (Byte, Tape, byte, fromHexString, fromNumbers, unbyte)

import Binary.Internal as Internal


type alias Tape =
    Internal.Tape


type alias Byte =
    Internal.Byte


byte : Int -> Maybe Byte
byte x =
    if x >= 0 && x < 256 then
        Just (Internal.Byte x)
    else
        Nothing


unbyte : Byte -> Int
unbyte (Internal.Byte x) =
    x


toNumbers : Tape -> List Int
toNumbers =
    List.map unbyte


fromNumbers : List Int -> Maybe Tape
fromNumbers xs =
    let
        helper : List Int -> Tape -> Maybe Tape
        helper xs tape =
            case xs of
                [] ->
                    Just (List.reverse tape)

                x :: rest ->
                    case byte x of
                        Nothing ->
                            Nothing

                        Just b ->
                            helper rest (b :: tape)
    in
    helper xs []


fromHexString : String -> Maybe Tape
fromHexString string =
    toBytes (String.toList <| String.toLower string) []


toHexString : Tape -> String
toHexString =
    List.map toHex >> String.concat


toHex : Byte -> String
toHex (Internal.Byte b) =
    String.fromList
        [ toHexChar <| b // 16
        , toHexChar <| b % 16
        ]


toHexChar : Int -> Char
toHexChar c =
    case c of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        10 ->
            'a'

        11 ->
            'b'

        12 ->
            'c'

        13 ->
            'd'

        14 ->
            'e'

        _ ->
            'f'


toBytes : List Char -> Tape -> Maybe Tape
toBytes chars tape =
    case chars of
        [] ->
            Just <| List.reverse tape

        [ _ ] ->
            Nothing

        x :: y :: rest ->
            case ( unHexChar x, unHexChar y ) of
                ( Just a, Just b ) ->
                    toBytes rest (Internal.Byte (16 * a + b) :: tape)

                _ ->
                    Nothing


unHexChar : Char -> Maybe Int
unHexChar c =
    case c of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        'a' ->
            Just 10

        'b' ->
            Just 11

        'c' ->
            Just 12

        'd' ->
            Just 13

        'e' ->
            Just 14

        'f' ->
            Just 15

        _ ->
            Nothing
