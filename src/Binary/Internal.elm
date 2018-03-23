module Binary.Internal exposing (..)


type Byte
    = Byte Int


type alias Tape =
    List Byte
