module SongToJson exposing (FirebaseSong, getPulse, parseSong, songToJson)

import Json.Decode exposing (Decoder, Value, decodeString, decodeValue, int, map2, string)
import Json.Decode.Pipeline exposing (decode, required)
import Json.Encode as Encode
import Songs exposing (..)


songToJson : Song -> Encode.Value
songToJson song =
    Encode.object <|
        [ ( "name", Encode.string song.name )
        , ( "key", Encode.string song.key )
        , ( "pulse", Encode.string <| stringify song.pulse )
        , ( "chords", Encode.string song.chords )
        , ( "comment", Encode.string song.comment )
        , ( "id", Encode.string song.id )
        ]


stringify : ( Int, Int ) -> String
stringify ( a, b ) =
    toString a ++ "/" ++ toString b


getPulse : String -> Result String ( Int, Int )
getPulse str =
    case String.split "/" str of
        [ x, y ] ->
            case String.toInt x of
                Ok x ->
                    case String.toInt y of
                        Ok y ->
                            Ok ( x, y )

                        Err err ->
                            Err err

                Err err ->
                    Err err

        _ ->
            Err <| "Incorrectly formatted pulse: " ++ str


type alias FirebaseSong =
    { name : String
    , chords : String
    , key : String
    , pulse : String
    , id : String
    , comment : String
    }


parseSong : Decoder FirebaseSong
parseSong =
    decode FirebaseSong
        |> required "name" string
        |> required "chords" string
        |> required "key" string
        |> required "pulse" string
        |> required "id" string
        |> required "comment" string
