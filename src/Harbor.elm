port module Harbor exposing (..)

import Json.Decode as Decode
import Json.Encode exposing (Value)


--sends things out to javascript


port check : String -> Cmd msg


port signIn : String -> Cmd msg


port newSong : Value -> Cmd msg


port updateSong : Value -> Cmd msg


port removeSong : String -> Cmd msg


port amILoggedIn : () -> Cmd msg



-- port for listening for suggestions from JavaScript


port suggestions : (List String -> msg) -> Sub msg


port recieveNewSong : (Decode.Value -> msg) -> Sub msg


port recieveUpdatedSong : (Decode.Value -> msg) -> Sub msg


port recieveRemoveQuery : (String -> msg) -> Sub msg


port errorMsg : (String -> msg) -> Sub msg


port signedIn : (Bool -> msg) -> Sub msg


port setId : (String -> msg) -> Sub msg
