module Main exposing (..)

import Harbor exposing (..)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, cols, id, placeholder, required, rows, src, style, type_, value)
import Html.Events exposing (..)
import Json.Decode as Decode
import List.Extra exposing (replaceIf)
import Messages exposing (..)
import SongToJson exposing (..)
import Songs exposing (..)
import String.Extra exposing (replace, toSentenceCase)


---- MODEL ----


type alias EditMode =
    { loggedIn : Bool
    , editing : Bool
    }


type alias Model =
    { displaySong : Maybe Song
    , currentSong : Maybe Song
    , unpublished : Maybe Song
    , transposing : Int
    , songs : List Song
    , filter : String
    , password : String
    , editMode : EditMode
    , menuMinimized : Bool
    , errors : List String
    }


modelInit : Model
modelInit =
    { displaySong = Nothing
    , currentSong = Nothing
    , unpublished = Nothing
    , transposing = 0
    , songs = []
    , filter = ""
    , password = ""
    , editMode = EditMode False False
    , menuMinimized = False
    , errors = []
    }


init : ( Model, Cmd Msg )
init =
    ( modelInit, amILoggedIn () )



---- UPDATE ----


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search str ->
            ( { model | filter = str }, Cmd.none )

        SetEditMode bool ->
            let
                editMode =
                    EditMode model.editMode.loggedIn bool

                msg =
                    if bool then
                        amILoggedIn ()
                    else
                        Cmd.none
            in
            ( { model | editMode = editMode }, msg )

        SetAsNewKey song ->
            let
                newSong =
                    { song
                        | key = transpose song.key model.transposing
                        , chords = transpose song.chords model.transposing
                    }
            in
            ( { model | displaySong = Just newSong }, Cmd.none )

        SetAsCurrentSong song ->
            let
                unpub =
                    case model.displaySong of
                        Nothing ->
                            model.unpublished

                        Just dSong ->
                            if isNewSong model.songs dSong then
                                model.displaySong
                            else
                                model.unpublished
            in
            ( { model
                | currentSong = Just song
                , displaySong = Just song
                , unpublished = unpub
              }
            , Cmd.none
            )

        IsLoggedIn signedIn ->
            ( { model | editMode = EditMode signedIn model.editMode.editing }, Cmd.none )

        TransposeBack ->
            ( { model | transposing = 0 }, Cmd.none )

        TransposeSong ifRight ->
            let
                delta =
                    if ifRight then
                        1
                    else
                        -1

                magic int =
                    if int > 6 then
                        -5
                    else if int < -6 then
                        5
                    else
                        int

                sum =
                    magic (model.transposing + delta)
            in
            ( { model | transposing = sum }, Cmd.none )

        NewSong ->
            ( { model
                | displaySong = Just emptySong
                , currentSong = Just emptySong
                , editMode = EditMode True True
              }
            , Cmd.none
            )

        RequestToRemoveSong song ->
            let
                removed =
                    Just { song | id = "" }
            in
            ( { model | displaySong = removed, currentSong = removed }, removeSong song.id )

        UpdatePassword str ->
            ( { model | password = str }, Cmd.none )

        SignIn ->
            ( model, signIn model.password )

        DisplayError str ->
            ( { model | errors = model.errors ++ [ str ] }, Cmd.none )

        RetrieveLostSong ->
            ( { model
                | displaySong = model.unpublished
                , currentSong = model.unpublished
                , unpublished = Nothing
              }
            , Cmd.none
            )

        UpdateChords song newChords ->
            let
                newSong =
                    { song | chords = newChords }
            in
            ( { model | displaySong = Just newSong, transposing = 0 }, Cmd.none )

        ChangeComment song comment ->
            let
                newSong =
                    { song | comment = comment }
            in
            ( { model | displaySong = Just newSong }, Cmd.none )

        ChangeSongName song newName ->
            let
                newSong =
                    { song | name = newName }
            in
            ( { model | displaySong = Just newSong }, Cmd.none )

        ChangeKey song newKey ->
            let
                newSong =
                    { song | key = newKey }
            in
            ( { model | displaySong = Just newSong, transposing = 0 }, Cmd.none )

        ChangePulse song newPulse ->
            let
                newSong =
                    { song | pulse = newPulse }
            in
            ( { model | displaySong = Just newSong }, Cmd.none )

        ParseJson eventType a ->
            let
                s =
                    Decode.decodeValue parseSong a

                song =
                    case s of
                        Ok s ->
                            case getPulse s.pulse of
                                Err e ->
                                    Err e

                                Ok pulse ->
                                    Ok
                                        { name = s.name
                                        , chords = s.chords
                                        , pulse = pulse
                                        , key = s.key
                                        , id = s.id
                                        , comment = s.comment
                                        }

                        Err errorMsg ->
                            Err errorMsg
            in
            case song of
                Err errorMsg ->
                    ( { model | errors = model.errors ++ [ errorMsg ] }, Cmd.none )

                Ok song ->
                    case eventType of
                        Add ->
                            ( { model
                                | songs = model.songs ++ [ song ]
                              }
                            , Cmd.none
                            )

                        Update ->
                            ( { model | songs = findAndReplace song model.songs }, Cmd.none )

        RemoveSong songId ->
            ( { model
                | songs = List.filter (\sl -> sl.id /= songId) model.songs
              }
            , Cmd.none
            )

        UploadToFirebase song ->
            if String.isEmpty song.name then
                ( { model | errors = model.errors ++ [ "Du måste ha ett namn på låten" ] }, Cmd.none )
            else
                ( model, newSong <| songToJson song )

        UpdateSongAtFirebase song ->
            if String.isEmpty song.name then
                ( { model | errors = model.errors ++ [ "Du måste ha ett namn på låten" ] }, Cmd.none )
            else
                ( model, updateSong <| songToJson song )

        ToggleMenuSize ->
            ( { model | menuMinimized = not model.menuMinimized }, Cmd.none )

        SetIdOnDisplaySong newId ->
            case model.displaySong of
                Nothing ->
                    ( model, Cmd.none )

                Just song ->
                    let
                        alteredSong =
                            if String.isEmpty song.id then
                                { song | id = newId }
                            else
                                song
                    in
                    ( { model | displaySong = Just alteredSong, currentSong = Just alteredSong }, Cmd.none )

        ClearErrors ->
            ( { model | errors = [] }, Cmd.none )


findAndReplace : Song -> List Song -> List Song
findAndReplace updatedSong list =
    replaceIf (\song -> song.id == updatedSong.id) updatedSong list


isNewSong : List Song -> Song -> Bool
isNewSong songs s =
    List.isEmpty <| List.filter (\x -> x.id == s.id) songs


transpose : String -> Int -> String
transpose chords amount =
    if amount == 0 then
        chords
    else if amount > 0 then
        transpose (transposeRight chords) (amount - 1)
    else
        transpose (transposeLeft chords) (amount + 1)



---- VIEW ----


view : Model -> Html Msg
view model =
    let
        errors =
            if not (List.isEmpty model.errors) then
                div [ id "errors" ] <|
                    List.map (\e -> div [ class "errorMessage" ] [ text e ]) model.errors
                        ++ [ button [ onClick ClearErrors ] [ text "Stäng" ] ]
            else
                text ""
    in
    div [ class "all" ]
        [ header []
            [ div [ class "title" ]
                [ h1 [] [ text "Fårkompet" ]
                , img [ src "logo.svg" ] []
                ]
            , editModeStuff model
            ]
        , main_ []
            [ errors
            , article [] <|
                case model.displaySong of
                    Nothing ->
                        []

                    Just song ->
                        [ if model.editMode.editing then
                            editDock model song
                          else
                            text ""
                        , displaySong model.editMode.editing model.transposing song
                        ]
            , displayMenu model
            ]
        ]


editDock : Model -> Song -> Html Msg
editDock model song =
    let
        new =
            isNewSong model.songs song
    in
    div [ id "entireDock" ]
        [ div [ id "editDock" ]
            [ displaySettings song
            , div []
                [ div [ class "hints" ] <|
                    List.map
                        (\s -> p [] [ text s ])
                        [ ", = taktseparerare", "|: = start på repris", ":| = slut på repris" ]
                , textarea [ id "chordInput", rows 5, onInput <| UpdateChords song, value song.chords ] []
                ]
            ]
        , div [ class "buttonRow" ]
            [ if new then
                button [ onClick <| UploadToFirebase song ]
                    [ text <|
                        "Ladda upp"
                    ]
              else
                button [ onClick <| UpdateSongAtFirebase song ]
                    [ text <|
                        "Uppdatera"
                    ]
            , if new then
                button [ class "deleteButton, inactive" ] [ text "Opublicerad" ]
              else
                button [ class "deleteButton", onClick <| RequestToRemoveSong song ] [ text "Ta bort" ]
            ]
        ]


editModeStuff : Model -> Html Msg
editModeStuff model =
    div [ id "topButtons" ]
        [ case model.unpublished of
            Just s ->
                button [ id "helpButton", onClick RetrieveLostSong ] [ text "Hjälp min låt försvann!" ]

            Nothing ->
                text ""
        , if model.editMode.editing then
            if model.editMode.loggedIn then
                div []
                    [ button [ id "addButton", onClick NewSong ] [ text "ny låt" ]
                    , button [ onClick <| SetEditMode False ] [ text "Sluta redigera" ]
                    ]
            else
                div [ id "loginForm" ]
                    [ input [ type_ "password", onInput UpdatePassword, onSubmit SignIn, placeholder "Lösenord" ] []
                    , button [ onClick SignIn ] [ text "Logga in" ]
                    ]
          else
            button [ onClick <| SetEditMode True ] [ text "Redigera" ]
        ]


displayMenu : Model -> Html Msg
displayMenu model =
    let
        btnText =
            if model.menuMinimized then
                "<"
            else
                ">"
    in
    div
        [ class "menu" ]
    <|
        [ button [ id "menuToggleButton", onClick ToggleMenuSize ] [ text btnText ] ]
            ++ (if model.menuMinimized then
                    []
                else
                    [ div [ class "actualMenu" ]
                        [ div [ id "menuHeader" ]
                            [ div [ class "menuDescriptor" ] [ text "Låtar" ]
                            , inputBoxExt "searchBox"
                                (input
                                    [ onInput Search
                                    , placeholder "Sök"
                                    ]
                                    [ text model.filter
                                    ]
                                )
                                Nothing
                            ]
                        , div [ id "songItems" ]
                            (List.map songItem <|
                                filterList model.filter <|
                                    model.songs
                            )
                        ]
                    ]
               )


filterList : String -> List Song -> List Song
filterList filter list =
    let
        filter_ =
            String.toLower filter
    in
    List.filter (\x -> String.startsWith filter_ (String.toLower x.name)) list


songItem : Song -> Html Msg
songItem song =
    div [ class "songItem", onClick <| SetAsCurrentSong song ]
        [ text song.name
        ]


displaySettings : Song -> Html Msg
displaySettings song =
    let
        pulse =
            [ ( 2, 4 ), ( 3, 4 ), ( 4, 4 ), ( 5, 8 ), ( 6, 8 ), ( 7, 8 ), ( 9, 16 ), ( 11, 16 ) ]
    in
    div [ id "settings" ]
        [ inputBox "Namn" song.name (ChangeSongName song)
        , inputBox "Tonart" song.key (ChangeKey song)
        , div [ class "pulseInputParent" ] <|
            List.map
                (pulseButton song)
                pulse
        ]


inputBox : String -> String -> (String -> Msg) -> Html Msg
inputBox hint currentValue msg =
    inputBoxExt ""
        (input
            [ onInput msg
            , required True
            , value currentValue
            ]
            []
        )
        (Just hint)


inputBoxExt : String -> Html Msg -> Maybe String -> Html Msg
inputBoxExt c myInput hint =
    let
        hinting =
            case hint of
                Nothing ->
                    []

                Just hint ->
                    [ if String.isEmpty hint then
                        text ""
                      else
                        label [] [ text hint ]
                    ]
    in
    div [ class "group", class c ] <|
        [ myInput
        , span [ class "highlight" ] []
        , span [ class "inputBar" ] []
        ]
            ++ hinting


pulseButton : Song -> ( Int, Int ) -> Html Msg
pulseButton song ( a, b ) =
    div
        [ class <|
            if ( a, b ) == song.pulse then
                "current"
            else
                ""
        , onClick <| ChangePulse song ( a, b )
        ]
        [ text <| toString a ++ "/" ++ toString b ]


displaySong : Bool -> Int -> Song -> Html Msg
displaySong editMode amount song =
    let
        ( enumerator, denominator ) =
            song.pulse

        ifNotEdit =
            if not editMode then
                div [ id "description" ]
                    [ div [ class "songDescription" ] [ text <| "namn: " ++ toSentenceCase song.name ]
                    , div [] [ text <| "taktart: " ++ toString enumerator ++ "/" ++ toString denominator ]
                    , div [] [ text <| "tonart: " ++ transpose song.key amount ]
                    ]
            else
                text ""
    in
    div [] <|
        [ ifNotEdit
        , chordPart song.chords amount
        , p [] [ text "Transponera" ]
        , div []
            [ button [ class "transposeButton", onClick (TransposeSong False) ] [ text <| transpose song.key (amount - 1) ]
            , if editMode then
                button [ class "transposeButton", onClick (SetAsNewKey song) ] [ text <| "Sätt som originalTonart " ]
              else
                button [ class "transposeButton", onClick TransposeBack ] [ text <| "OriginalTonart " ]
            , button [ class "transposeButton", onClick (TransposeSong True) ] [ text <| transpose song.key (amount + 1) ]
            ]
        ]


chordPart : String -> Int -> Html Msg
chordPart chords amount =
    transpose chords amount
        |> String.split ","
        |> List.map (\bar -> makeBar bar)
        |> div [ id "chords" ]


makeBar : String -> Html Msg
makeBar bar =
    let
        isStartRepetition =
            String.contains "|:" bar

        isEndRepetition =
            String.contains ":|" bar

        cl =
            if isStartRepetition then
                class "startRepetition"
            else if isEndRepetition then
                class "endRepetition"
            else
                class "not-repetition"

        formatted =
            String.trim bar
                |> replace ":|" ""
                |> replace "|:" ""
    in
    String.split
        " "
        (String.trim formatted)
        |> List.map (\chord -> div [ class "chord" ] [ text chord ])
        |> div [ cl, class "bar" ]



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ recieveNewSong (ParseJson Add)
        , recieveUpdatedSong (ParseJson Update)
        , recieveRemoveQuery RemoveSong
        , errorMsg DisplayError
        , signedIn IsLoggedIn
        , setId SetIdOnDisplaySong
        ]


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
