module Songs exposing (..)

import String.Extra exposing (..)


type alias Song =
    { name : String
    , pulse : ( Int, Int )
    , chords : String
    , key : String
    , id : String
    , comment : String
    }


emptySong : Song
emptySong =
    { name = ""
    , key = "A"
    , pulse = ( 3, 4 )
    , chords = "|: A B, A C D :|, A B"
    , id = ""
    , comment = ""
    }


exampleSong : Song
exampleSong =
    { name = "Blinka lilla stjärna"
    , key = "A"
    , pulse = ( 4, 4 )
    , chords = "(A, D A, D A, E A):, A D, A E, A D, A E, A, D A, D A, E A"
    , id = "testID"
    , comment = "här kan man skriva en kommentar"
    }


transposeSongRight : Song -> Song
transposeSongRight song =
    { song
        | key = transposeRight song.key
        , chords = transposeRight song.chords
    }


transposeSongLeft : Song -> Song
transposeSongLeft song =
    { song
        | key = transposeLeft song.key
        , chords = transposeLeft song.chords
    }


transposeRight : String -> String
transposeRight input =
    String.map transposeLetterRight input |> fixFs


transposeLeft : String -> String
transposeLeft input =
    String.map transposeLetterLeft input |> fixBs


fixFs : String -> String
fixFs input =
    replace "F" "F#" input |> replace "F##" "G" |> replace "F#b" "F"


fixBs : String -> String
fixBs input =
    replace "B" "Bb" input |> replace "Bbb" "A" |> replace "Bb#" "B"


transposeLetterLeft : Char -> Char
transposeLetterLeft note =
    case note of
        'C' ->
            'F'

        'F' ->
            'B'

        'B' ->
            'E'

        'E' ->
            'A'

        'A' ->
            'D'

        'D' ->
            'G'

        'G' ->
            'C'

        anyOther ->
            anyOther


transposeLetterRight : Char -> Char
transposeLetterRight note =
    case note of
        'C' ->
            'G'

        'G' ->
            'D'

        'D' ->
            'A'

        'A' ->
            'E'

        'E' ->
            'B'

        'B' ->
            'F'

        'F' ->
            'C'

        anyOther ->
            anyOther
