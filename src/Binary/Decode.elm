module Binary.Decode
    exposing
        ( Decoder
        , andMap
        , andThen
        , byte
        , combine
        , end
        , fail
        , int1
        , int2
        , int3
        , int4
        , map
        , map2
        , map3
        , map4
        , oneOf
        , repeat
        , run
        , stringx
        , succeed
        , tape
        )

import Binary as B
import Binary.Internal as Internal exposing (Byte(..))
import String.UTF8


{-| A `Decoder` is a recipe for how to go from a tape to some datastructure.

You'll likely use the combinators in this library to bundle them up into a
single, powerful decoder and run that using `run`.

-}
type Decoder a
    = Decoder
        (Internal.Tape
         ->
            Result String
                { tape : Internal.Tape
                , result : a
                }
        )


undecoder :
    Decoder a
    -> (Internal.Tape -> Result String { tape : Internal.Tape, result : a })
undecoder (Decoder decoderFn) =
    decoderFn


{-| Run a decoder on a tape.

    import Binary
    import Binary.Decode as Decode


    Decode.run Decode.byte []
    --> Err "Read past end of tape"


    Binary.fromNumbers [ 1, 2, 3 ]
        |> Result.fromMaybe "no tape"
        |> Result.andThen
            (Decode.run
                (Decode.map3 (,,)
                    Decode.int1
                    Decode.int1
                    Decode.int1
                )
            )
    --> Ok (1, 2, 3)

-}
run : Decoder a -> B.Tape -> Result String a
run (Decoder decoderFn) tape =
    decoderFn tape |> Result.map .result


{-| Reads a single `Byte` of the start of the `Tape`. This consumes a single cell
of the tape.

If the tape is empty, this results in failure.

-}
byte : Decoder B.Byte
byte =
    Decoder <|
        \tape ->
            case tape of
                x :: xs ->
                    Ok { tape = xs, result = x }

                [] ->
                    Err "Read past end of tape"


{-| Repeat a decoder `n` times.

    import Binary
    import Binary.Decode as Decode


    Binary.fromNumbers [ 1, 2, 3 ]
        |> Result.fromMaybe "no tape"
        |> Result.andThen
            (Decode.run <| Decode.repeat 3 Decode.int1)
    --> Ok [ 1, 2, 3 ]

-}
repeat : Int -> Decoder a -> Decoder (List a)
repeat times decoder =
    List.repeat times decoder |> combine


combine : List (Decoder a) -> Decoder (List a)
combine decoders =
    List.foldr (map2 (::)) (succeed []) decoders


{-| Create a decoder that succeeds with a fixed value, without consuming any part
of the tape.

This can be useful when using `andMap` to do "pipeline-style" decoding, in other
words using an applicative style. It's also very useful when working with
`andThen` to chain decoders based on the outcome of a previous decoder.

-}
succeed : a -> Decoder a
succeed v =
    Decoder <|
        \tape ->
            Ok { tape = tape, result = v }


{-| Create a decoder that always fails with a certain message, ignoring the tape.
-}
fail : String -> Decoder a
fail msg =
    Decoder <| \tape -> Err msg


{-| Decode the rest of the tape, consuming all of it.
-}
tape : Decoder B.Tape
tape =
    Decoder <| \tape -> Ok { tape = [], result = tape }


{-| `map` a function over the value that will eventually be produced by the
`Decoder`.
-}
map : (a -> b) -> Decoder a -> Decoder b
map f (Decoder decoderFn) =
    Decoder <| decoderFn >> Result.map (\res -> { res | result = f res.result })


{-| Chain decoders.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen f (Decoder decoderFn) =
    Decoder <| decoderFn >> Result.andThen (\res -> (undecoder <| f res.result) res.tape)


{-| Map a function over 2 decoders. Note: not meant to be used pipeline-style.

The decoders are executed in the order they are provided, so `map2 f decoderOne
decoderTwo` will _first_ execute `decoderOne` and only then execute `decoderTwo`.

-}
map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 f (Decoder aDecoder) (Decoder bDecoder) =
    Decoder <|
        \tape ->
            Result.andThen
                (\resA ->
                    Result.map
                        (\resB ->
                            { resB | result = f resA.result resB.result }
                        )
                        (bDecoder resA.tape)
                )
                (aDecoder tape)


map3 : (a -> b -> c -> d) -> Decoder a -> Decoder b -> Decoder c -> Decoder d
map3 f decA decB decC =
    map2 f decA decB
        |> andMap decC


map4 : (a -> b -> c -> d -> e) -> Decoder a -> Decoder b -> Decoder c -> Decoder d -> Decoder e
map4 f decA decB decC decD =
    map3 f decA decB decC
        |> andMap decD


{-| Provide a value produced by a decoder to a decoder for a function. Note:
meant to be used in a pipeline.

The decoders are executed "in reverse". `andMap decoderVal decoderFn` will
_first_ execute `decoderFn` and only _then_ execute `decoderVal`.

This means that `succeed f |> andMap valDecoder` will execute in the "natural"
order of reading it.

-}
andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    flip (map2 (<|))


{-| Decode a single byte to an `Int`. In other words, this interprets the input
as an 8-bit unsigned `Int`.
-}
int1 : Decoder Int
int1 =
    intx 1


{-| Decode two bytes to an `Int`. This consumes two bytes from the tape and
interprets it as a 16-bit unsigned `Int`.
-}
int2 : Decoder Int
int2 =
    intx 2


{-| Decode three bytes to an `Int`. This consumes three bytes from the tape and
interprets it as a 24-bit unsigned `Int`.
-}
int3 : Decoder Int
int3 =
    intx 3


{-| Decode 4 bytes to an `Int`. This consumes 4 bytes from the tape and
interprets it as a 32-bit unsigned `Int`.
-}
int4 : Decoder Int
int4 =
    intx 4


intx : Int -> Decoder Int
intx cnt =
    map unbytes (repeat cnt byte)


{-| Read `x` bytes and convert it to an Elm string. Input is assumed to be
encoded using UTF-8, so each byte should represent a single UTF-8 codepoint.
-}
stringx : Int -> Decoder String
stringx cnt =
    repeat cnt int1
        |> andTry String.UTF8.toString


{-| Decode something, then try running a function that results in a `Result
String b` on it.

For reference, `stringx` is implemented as follows:

    stringx : Int -> Decoder String
    stringx cnt =
        repeat cnt int1 |> andTry String.UTF8.toString

-}
andTry : (a -> Result String b) -> Decoder a -> Decoder b
andTry f =
    andThen (fromResult << f)


{-| Create a decoder that always succeed or always fails from a `Result String x`
-}
fromResult : Result String a -> Decoder a
fromResult r =
    case r of
        Ok v ->
            succeed v

        Err e ->
            fail e


unbytes : List B.Byte -> Int
unbytes =
    List.foldl (\b acc -> acc * 256 + B.unbyte b) 0


{-| Try a couple of decoders. Note that this includes backtracking: if the first
decoder consumes some bytes but eventually fails, the tape is rolled back to its
original position before trying the second decoder.
-}
oneOf : List (Decoder a) -> Decoder a
oneOf decoders =
    Decoder <| \tape -> oneOfHelp tape decoders


oneOfHelp : B.Tape -> List (Decoder a) -> Result String { tape : B.Tape, result : a }
oneOfHelp tape decoders =
    case decoders of
        [] ->
            Err "Tried all options, no success"

        (Decoder x) :: xs ->
            case x tape of
                Ok res ->
                    Ok res

                Err e ->
                    oneOfHelp tape xs


{-| Verify that the end of the tape is reached and all of the bytes are read.
-}
end : Decoder a -> Decoder a
end (Decoder decoderFn) =
    Decoder <|
        \tape ->
            case tape of
                [] ->
                    decoderFn tape

                _ ->
                    Err "Tape hasn't reached end yet"
