module Entwicklung.ElmParalleleKoordinaten exposing (..)

import Axis
import Browser
import Color
import Csv
import Csv.Decode
import Html exposing (Html, a, li, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Http
import List.Extra
import Path
import Scale exposing (ContinuousScale)
import Shape
import Statistics
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, fontSize, stroke, strokeWidth, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))


--MAIN
main =
  Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

--MODEL
type Model
  = Failure
  | Loading
  | Success 
    { data : List Weine
    , ersteFunktion : Weine -> Float
    , zweiteFunktion : Weine -> Float
    , dritteFunktion : Weine -> Float
    , vierteFunktion : Weine -> Float
    , ersterName : String
    , zweiterName : String
    , dritterName : String
    , vierterName : String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , holenVonCsv GotText
    )

holenVonCsv : (Result Http.Error String -> Msg) -> Cmd Msg
holenVonCsv x = 
    liste
        |> List.map
            (\datensatz ->
                Http.get
                    { url = "https://raw.githubusercontent.com/RicBre/Elm-Projekt-WineInformation/main/Daten/Aufbereitete%20Daten/" ++ datensatz
                    , expect = Http.expectString x
                    }
            )
        |> Cmd.batch

liste : List String
liste =
    [ "WineInformationExcelAufbereitetKlein.csv"]

csvString_to_data : String -> List Weine
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeWeine
        |> Result.toMaybe
        |> Maybe.withDefault []

type alias Weine =
    { name : String
    , alc : Float
    , temperatur : Float
    , suesse : Float
    , saeurengehalt : Float
    , koerper : Float
    , gerbstoff : Float
    , preis : Float
    , jahr : Float
    , ml : Float
    }

decodeWeine : Csv.Decode.Decoder (Weine -> a) a
decodeWeine =
    Csv.Decode.map Weine
        (Csv.Decode.field "name" Ok
            |> Csv.Decode.andMap (Csv.Decode.field "alc"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "temperatur"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "suesse"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "saeurengehalt"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "koerper"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "gerbstoff"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "preis"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "jahr"(String.toFloat >> Result.fromMaybe "error parsing string"))
            |> Csv.Decode.andMap (Csv.Decode.field "ml"(String.toFloat >> Result.fromMaybe "error parsing string"))
        )


-- UPDATE
type Msg
    = GotText (Result Http.Error String)
    | Ändere1 (Weine -> Float, String)
    | Ändere2 (Weine -> Float, String)
    | Ändere3 (Weine -> Float, String)
    | Ändere4 (Weine -> Float, String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = weineListe [ fullText ], ersteFunktion = .alc, zweiteFunktion = .temperatur, dritteFunktion = .suesse, vierteFunktion = .saeurengehalt , ersterName = "Alkohol", zweiterName = "Temperatur", dritterName = "Süße", vierterName = "Säuregehalt"}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        Ändere1 (x, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, ersteFunktion = x, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = m.vierteFunktion , ersterName = a, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = m.vierterName}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        Ändere2 (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = y, dritteFunktion = m.dritteFunktion, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = a, dritterName = m.dritterName, vierterName = m.vierterName}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        Ändere3 (z, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = z, vierteFunktion = m.vierteFunktion , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = a, vierterName = m.vierterName}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        Ändere4 (c, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, ersteFunktion = m.ersteFunktion, zweiteFunktion = m.zweiteFunktion, dritteFunktion = m.dritteFunktion, vierteFunktion = c , ersterName = m.ersterName, zweiterName = m.zweiterName, dritterName = m.dritterName, vierterName = a}, Cmd.none )

                _ ->
                    ( model, Cmd.none )
weineListe :List String -> List Weine
weineListe liste1 =
    List.map(\t -> csvString_to_data t) liste1
        |> List.concat
        
-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount_ : Int
tickCount_ =
    8


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


wideExtent : List Float -> ( Float, Float )
wideExtent values =
    let
        closeExtent =
            Statistics.extent values
                |> Maybe.withDefault defaultExtent

        extension =
            (Tuple.second closeExtent - Tuple.first closeExtent) / toFloat (2 * tickCount_)
    in
    ( Tuple.first closeExtent - extension |> max 0
    , Tuple.second closeExtent + extension
    )

type alias MultiDimPoint =
    { pointName : String, value : List Float }


type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }



parallelCoodinatesPlot : Float -> Float -> MultiDimData -> Svg msg
parallelCoodinatesPlot w ar model =
    let
        h : Float
        h =
            w / ar

        listTransformieren : List (List Float)
        listTransformieren =
            model.data
                |> List.concat
                |> List.map .value
                |> List.Extra.transpose

        listWideExtent : List ( Float, Float )
        listWideExtent =
            listTransformieren |> List.map wideExtent

        listScale =
            List.map (Scale.linear ( h, 0 )) listWideExtent

        listAxis =
            List.map (Axis.left [ Axis.tickCount tickCount_ ]) listScale

        xScale =
            Scale.linear ( 0, w ) ( 1, List.length model.dimDescription |> toFloat )
    in
    svg
        [ viewBox 0 0 (w + 2 * padding) (h + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 90
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 90
        ]
    <|
        [ TypedSvg.style []
            []
        , g [ TypedSvg.Attributes.class [ "parallelAxis" ] ]
            [ g [ transform [ Translate (padding - 1) padding ] ] <|
                List.indexedMap
                    (\i axis ->
                        g
                            [ transform
                                [ Translate (Scale.convert xScale (toFloat i + 1)) 0
                                ]
                            ]
                            [ axis ]
                    )
                    listAxis
            , g [ transform [ Translate (padding - 1) 0 ] ] <|
                List.indexedMap
                    (\i desc ->
                        text_
                            [ fontFamily [ "sans-serif" ]
                            , fontSize (Px 10)
                            , x <| Scale.convert xScale (toFloat i + 1)
                            , y <| padding * 7 / 8
                            , textAnchor AnchorMiddle
                            ]
                            [ TypedSvg.Core.text desc ]
                    )
                    model.dimDescription
            ]
        ]
            ++ (let
                    drawPoint p =
                        let
                            linePath : Path.Path
                            linePath =
                                List.map3
                                    (\desc s px ->
                                        Just
                                            ( Scale.convert xScale <| toFloat desc
                                            , Scale.convert s px
                                            )
                                    )
                                    (List.range 1 (List.length model.dimDescription))
                                    listScale
                                    p
                                    |> Shape.line Shape.linearCurve
                        in
                        Path.element linePath
                            [ stroke <| Paint <| Color.rgba 0 0 0 0.8
                            , strokeWidth <| Px 0.5
                            , fill PaintNone
                            ]
                in
                model.data
                    |> List.map
                        (\dataset ->
                            g [ transform [ Translate (padding - 1) padding ] ]
                                (List.map (.value >> drawPoint) dataset)
                        )
               )

view : Model -> Html Msg
view model =
    case model of
        Failure ->
            Html.text "Ich konnte Ihre Weine nicht öffnen."

        Loading ->
            Html.text "Weine werden geöffnet..."

        Success l ->
                    let
                        multiDimDaten : List Weine -> (Weine -> Float) -> (Weine -> Float) -> (Weine -> Float) -> (Weine -> Float) -> (Weine -> String) -> String -> String -> String -> String-> MultiDimData
                        multiDimDaten listeWeine a b c d e f g h i=
                         MultiDimData [f, g, h, i]
                            [ List.map
                                (\x ->
                                    [(a x), (b x), (c x), (d x)]
                                        |> MultiDimPoint (e x)
                                )
                                listeWeine
                            ]

                        plotDaten = 
                            multiDimDaten l.data l.ersteFunktion l.zweiteFunktion l.dritteFunktion l.vierteFunktion .name l.ersterName l.zweiterName l.dritterName l.vierterName       
                    in
                    Html.div []
                        [
                            ul[][
                                li[][
                                    Html.text <| "Suchen eine Eigenschaft für die erste Spalte aus"
                                    , Html.button [onClick (Ändere1 (.alc, "Durchschnittlicher Alkoholgehalt"))][Html.text "Alkoholgehalt"]
                                    , Html.button [onClick (Ändere1 (.temperatur, "Durchschnittliche Trinktemperatur"))][Html.text "Trinktemperatur"]
                                    , Html.button [onClick (Ändere1 (.suesse, "Süße"))][Html.text "Süße"]
                                    , Html.button [onClick (Ändere1 (.saeurengehalt, "Säuregehalt"))][Html.text "Säuregehalt"]
                                    , Html.button [onClick (Ändere1 (.koerper, "Körper"))][Html.text "Körper"]
                                    , Html.button [onClick (Ändere1 (.gerbstoff, "Gerbstoffe"))][Html.text "Gerbstoffe"]
                                    , Html.button [onClick (Ändere1 (.preis, "Preis"))][Html.text "Preis"]
                                    , Html.button [onClick (Ändere1 (.jahr, "Jahr"))][Html.text "Jahr"]
                                    , Html.button [onClick (Ändere1 (.ml, "Mililiter"))][Html.text "Mililiter"]
                                ]
                            ]
                            , ul[][
                                li[][
                                    Html.text <| "Suchen eine Eigenschaft für die zweite Spalte aus"
                                    , Html.button [onClick (Ändere2 (.alc, "Durchschnittlicher Alkoholgehalt"))][Html.text "Alkoholgehalt"]
                                    , Html.button [onClick (Ändere2 (.temperatur, "Durchschnittliche Trinktemperatur"))][Html.text "Trinktemperatur"]
                                    , Html.button [onClick (Ändere2 (.suesse, "Süße"))][Html.text "Süße"]
                                    , Html.button [onClick (Ändere2 (.saeurengehalt, "Säuregehalt"))][Html.text "Säuregehalt"]
                                    , Html.button [onClick (Ändere2 (.koerper, "Körper"))][Html.text "Körper"]
                                    , Html.button [onClick (Ändere2 (.gerbstoff, "Gerbstoffe"))][Html.text "Gerbstoffe"]
                                    , Html.button [onClick (Ändere2 (.preis, "Preis"))][Html.text "Preis"]
                                    , Html.button [onClick (Ändere2 (.jahr, "Jahr"))][Html.text "Jahr"]
                                    , Html.button [onClick (Ändere2 (.ml, "Mililiter"))][Html.text "Mililiter"]
                                ]
                            ]
                            , ul[][
                                li[][
                                    Html.text <| "Suchen eine Eigenschaft für die dritte Spalte aus"
                                    , Html.button [onClick (Ändere3 (.alc, "Durchschnittlicher Alkoholgehalt"))][Html.text "Alkoholgehalt"]
                                    , Html.button [onClick (Ändere3 (.temperatur, "Durchschnittliche Trinktemperatur"))][Html.text "Trinktemperatur"]
                                    , Html.button [onClick (Ändere3 (.suesse, "Süße"))][Html.text "Süße"]
                                    , Html.button [onClick (Ändere3 (.saeurengehalt, "Säuregehalt"))][Html.text "Säuregehalt"]
                                    , Html.button [onClick (Ändere3 (.koerper, "Körper"))][Html.text "Körper"]
                                    , Html.button [onClick (Ändere3 (.gerbstoff, "Gerbstoffe"))][Html.text "Gerbstoffe"]
                                    , Html.button [onClick (Ändere3 (.preis, "Preis"))][Html.text "Preis"]
                                    , Html.button [onClick (Ändere3 (.jahr, "Jahr"))][Html.text "Jahr"]
                                    , Html.button [onClick (Ändere3 (.ml, "Mililiter"))][Html.text "Mililiter"]
                                ]
                            ]
                            , ul[][
                                li[][
                                    Html.text <| "Suchen eine Eigenschaft für die vierte Spalte aus"
                                    , Html.button [onClick (Ändere4 (.alc, "Durchschnittlicher Alkoholgehalt"))][Html.text "Alkoholgehalt"]
                                    , Html.button [onClick (Ändere4 (.temperatur, "Durchschnittliche Trinktemperatur"))][Html.text "Trinktemperatur"]
                                    , Html.button [onClick (Ändere4 (.suesse, "Süße"))][Html.text "Süße"]
                                    , Html.button [onClick (Ändere4 (.saeurengehalt, "Säuregehalt"))][Html.text "Säuregehalt"]
                                    , Html.button [onClick (Ändere4 (.koerper, "Körper"))][Html.text "Körper"]
                                    , Html.button [onClick (Ändere4 (.gerbstoff, "Gerbstoffe"))][Html.text "Gerbstoffe"]
                                    , Html.button [onClick (Ändere4 (.preis, "Preis"))][Html.text "Preis"]
                                    , Html.button [onClick (Ändere4 (.jahr, "Jahr"))][Html.text "Jahr"]
                                    , Html.button [onClick (Ändere4 (.ml, "Mililiter"))][Html.text "Mililiter"]
                                ]
                             ]
                                ,parallelCoodinatesPlot 600 2 plotDaten
                        ]