module Main exposing (..)

import Browser
import Csv
import Csv.Decode
import Html exposing (Html, pre, text)
import Http



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type Model
    = Failure
    | Loading
    | Success (List String)

 
type NewModel
    = Loading2
    | List Data   
    
type Data
    = Failure2
    | Success2 String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , liste
        |> List.map
            (\datensatz ->
                Http.get
                    { url = "https://raw.githubusercontent.com/RicBre/Elm-Projekt-WineInformation/main/" ++ datensatz
                    , expect = Http.expectString GotText
                    }
            )
        |> Cmd.batch
    )



--|> Cmd.batch


liste : List String
liste =
    [ "cleansingWine.csv" ]


csvString_to_data : String -> List ( String, Maybe Float )
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeStockDay
        |> Result.toMaybe
        |> Maybe.withDefault []


decodeStockDay : Csv.Decode.Decoder (( String, Maybe Float ) -> a) a
decodeStockDay =
    Csv.Decode.map (\a b -> ( a, Just b ))
        (Csv.Decode.field "Date" Ok
            |> Csv.Decode.andMap
                (Csv.Decode.field "Open"
                    (String.toFloat >> Result.fromMaybe "error parsing string")
                )
        )


umwandeln : List ( String, Maybe Float ) -> List ( String, String )
umwandeln ganzerText =
    List.map (\( a, b ) -> ( a, b |> Maybe.map String.fromFloat |> Maybe.withDefault "Kein Wert vorhanden" )) ganzerText


umwandeln2 : List ( String, Maybe Float ) -> String
umwandeln2 ganzerText =
    List.map Tuple.first ganzerText
        |> String.concat



-- UPDATE


renderList : List ( String, String ) -> Html msg
renderList lst =
    Html.ul []
        (List.map (\( a, b ) -> Html.li [] [ text <| a ++ ", " ++ b ]) lst)


type Msg
    = GotText (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentList =
            case model of
                Success l ->
                    l

                Failure ->
                    []
                
                Loading ->
                    []
    in
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| currentList ++ [ fullText ], Cmd.none )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        Failure ->
            text "I was unable to load your book."

        Loading ->
            text "Loading..."

        Success l ->
            Html.div [] <|
                List.map (\fulltext -> pre [] [ renderList <| umwandeln <| csvString_to_data fulltext ]) l