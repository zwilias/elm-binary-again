module Example exposing (..)

import Binary.Decoder as BD exposing (Decoder)
import Binary.Internal as I exposing (Byte(..), Tape)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import String.UTF8
import Test exposing (..)


byte : Test
byte =
    test "Reading a byte from a tape returns that byte" <|
        \_ ->
            let
                tape : Tape
                tape =
                    [ Byte 123 ]

                decoder : Decoder Byte
                decoder =
                    BD.byte
            in
            BD.run decoder tape
                |> Expect.equal (Ok (Byte 123))


stringX : Test
stringX =
    fuzz Fuzz.string "Decoding a String" <|
        \string ->
            let
                tape : Tape
                tape =
                    String.UTF8.toBytes string
                        |> List.map Byte

                decoder : Decoder String
                decoder =
                    BD.stringx (List.length tape)
            in
            BD.run decoder tape
                |> Expect.equal (Ok string)


combo : Test
combo =
    test "Reading some ints returns the correct order" <|
        \_ ->
            let
                tape : Tape
                tape =
                    [ Byte 10, Byte 11, Byte 12, Byte 13 ]

                decoder : Decoder ( Int, Int, Int, Int )
                decoder =
                    BD.succeed (,,,)
                        |> BD.andMap BD.int1
                        |> BD.andMap BD.int1
                        |> BD.andMap BD.int1
                        |> BD.andMap BD.int1
            in
            BD.run decoder tape
                |> Expect.equal (Ok ( 10, 11, 12, 13 ))


map4 : Test
map4 =
    test "Reading some ints with map4 returns the correct order" <|
        \_ ->
            let
                tape : Tape
                tape =
                    [ Byte 10, Byte 11, Byte 12, Byte 13 ]

                decoder : Decoder ( Int, Int, Int, Int )
                decoder =
                    BD.map4 (,,,)
                        BD.int1
                        BD.int1
                        BD.int1
                        BD.int1
            in
            BD.run decoder tape
                |> Expect.equal (Ok ( 10, 11, 12, 13 ))
