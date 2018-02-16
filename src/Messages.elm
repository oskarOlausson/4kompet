module Messages exposing (..)

import Json.Decode exposing (Value)
import Songs exposing (Song)
import Time exposing (Time)


type FirebaseSongEvent
    = Add
    | Update


type Msg
    = SetAsCurrentSong Song
    | Search String
    | TransposeSong Bool
    | TransposeBack
    | SetAsNewKey Song
    | ParseJson FirebaseSongEvent Value
    | RemoveSong String
    | RequestToRemoveSong Song
    | SignIn
    | DisplayError String
    | UpdatePassword String
    | IsLoggedIn Bool
    | UpdateChords Song String
    | ChangeSongName Song String
    | ChangePulse Song ( Int, Int )
    | NewSong
    | SetEditMode Bool
    | UploadToFirebase Song
    | UpdateSongAtFirebase Song
    | ChangeKey Song String
    | ChangeComment Song String
    | ToggleMenuSize
    | SetIdOnDisplaySong String
    | ClearErrors
    | RetrieveLostSong
    | Tick Time
