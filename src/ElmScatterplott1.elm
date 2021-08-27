module ElmScatterplott1 exposing (..)

import Axis
import Html exposing (Html)
import Http
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, r, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Length(..), Transform(..), px)
import Csv
import Csv.Decode
import Browser
import Html exposing (ul)
import Html exposing (li)
import Html.Events exposing (onClick)



type Model
  = Fehlschlag
  | Laden
  | Erfolg 
    { daten : List Weine
    , xAAFunktion : Weine -> Float
    , yAAFunktion : Weine -> Float
    , xName : String
    , yName : String
    }

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

type Msg
    = ErhalteText (Result Http.Error String)
    | ÄndereX (Weine -> Float, String)
    | ÄndereY (Weine -> Float, String)

type alias Punkt =
    { punktName : String, x : Float, y : Float }

type alias XYDaten =
    { xBeschreibung : String
    , yBeschreibung : String
    , daten : List Punkt
    }



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

csvStringZuDaten : String -> List Weine
csvStringZuDaten csvRoh =
    Csv.parse csvRoh
        |> Csv.Decode.decodeCsv dekodierenWeine
        |> Result.toMaybe
        |> Maybe.withDefault []



dekodierenWeine : Csv.Decode.Decoder (Weine -> a) a
dekodierenWeine =
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

weineListe :List String -> List Weine
weineListe liste1 =
    List.map(\t -> csvStringZuDaten t) liste1
        |> List.concat
        
filtertReduzierteWeine : List Weine -> (Weine -> String) -> (Weine -> Float) -> (Weine -> Float) -> String -> String -> XYDaten
filtertReduzierteWeine weinliste a b c x y =
    XYDaten x y (List.map (\n -> punktName n a b c x y) weinliste)



w : Float
w =
    900


h : Float
h =
    450


abstand : Float
abstand =
    60


radius : Float
radius =
    5.0


einteilungAchseZahl : Int
einteilungAchseZahl =
    5

xAchse : List Float -> Svg msg
xAchse werte =
    Axis.bottom [ Axis.tickCount einteilungAchseZahl ] (xSkala werte)


yAchse : List Float -> Svg msg
yAchse werte =
    Axis.left [ Axis.tickCount einteilungAchseZahl ] (ySkala werte)

xSkala : List Float -> ContinuousScale Float
xSkala werte =
    Scale.linear ( 0, w - 2 * abstand ) ( weiteErweiterung werte )

ySkala : List Float -> ContinuousScale Float
ySkala werte =
    Scale.linear ( h - 2 * abstand, 0 ) ( weiteErweiterung werte )

standartErweiterung : ( number, number1 )
standartErweiterung =
    ( 0, 100 )

addieren : (Float, Float) -> Float-> (Float, Float) 
addieren (min, max) x =
    if min <= 0 then
        ( 0, max + x)
    else 
        (min - x, max + x)

weiteErweiterung : List Float -> ( Float, Float )
weiteErweiterung werte = 
    let
        ergebnis = 
            Maybe.withDefault (0, 0)
            (Statistics.extent werte)
        
        max =          
            Maybe.withDefault (0)
            (List.maximum werte)
            
        ergebnis1 = 
            addieren ergebnis (toFloat(einteilungAchseZahl)*max/50)
        
        ergebnis2 = 
            addieren ergebnis1 (0.0)       
    in
        ergebnis2

punktName : Weine -> (Weine -> String) -> (Weine -> Float) -> (Weine -> Float) -> String -> String -> Punkt
punktName weine u v x y z =
    Punkt (u weine ++ ", " ++ y ++ ": " ++ String.fromFloat (v weine) ++ "," ++ z ++ ": " ++ String.fromFloat (x weine)) (v weine) (x weine)

punkt : ContinuousScale Float -> ContinuousScale Float -> Punkt -> Svg msg
punkt skalaX skalaY yxPunkt =
    g
        [
            class["point"]
            ,fontSize <| Px 12.0
            ,fontFamily ["calibri"]
            ,transform
                [
                    Translate
                    (Scale.convert skalaX yxPunkt.x)
                    (Scale.convert skalaY yxPunkt.y)
                ]
        ]

        [
            circle [cx 0, cy 0, r 5] []
            , text_ [x 10, y -20, textAnchor AnchorMiddle] [Html.text yxPunkt.punktName]
        ]

scatterplot : XYDaten -> Svg msg
scatterplot model =
    let
        xWerte : List Float
        xWerte =
            List.map .x model.daten

        yWerte : List Float
        yWerte =
            List.map .y model.daten

        xSkalaLokal : ContinuousScale Float
        xSkalaLokal =
            xSkala xWerte

        ySkalaLokal : ContinuousScale Float
        ySkalaLokal =
            ySkala yWerte

        halb : ( Float, Float ) -> Float
        halb t =
            (Tuple.second t - Tuple.first t) / 2

        labelPosition : { x : Float, y : Float }
        labelPosition =
            { x = weiteErweiterung xWerte |> halb
            , y = weiteErweiterung yWerte |> Tuple.second
            }
    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g [ transform [ Translate 60 390 ] ]
            [ xAchse xWerte
            , text_
                [ x (Scale.convert xSkalaLokal labelPosition.x)
                , y 35
                 , fontFamily [ "calibri" ]
                , fontSize (px 20)
                --, fontWeight FontWeightBold
                ]
                [ TypedSvg.Core.text model.xBeschreibung ]
            ]
        , g [ transform [ Translate 60 60 ] ]
            [ yAchse yWerte
            , text_
                [ x -30
                , y -30
                , fontFamily [ "calibri" ]
                , fontSize (px 20)
                --, fontWeight FontWeightBold
                ]
                [ TypedSvg.Core.text model.yBeschreibung ]
            ]
        , g [ transform [ Translate abstand abstand ] ]
            (List.map (punkt xSkalaLokal ySkalaLokal) model.daten)
        ]
        


main =
  Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Laden
    , holenVonCsv ErhalteText
    )

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> Html Msg
view model =
    case model of
        Fehlschlag ->
            Html.text "Ich konnte Ihre Weine nicht öffnen."

        Laden ->
            Html.text "Weine werden geöffnet..."

        Erfolg l ->
            let
                weine =
                    filtertReduzierteWeine l.daten .name l.xAAFunktion l.yAAFunktion l.xName l.yName

            in
            Html.div []
                [
                    ul[][
                        li[][
                            Html.text <| "Suchen eine Eigenschaft für die X-Achse aus"
                            , Html.button [onClick (ÄndereX (.alc, "Durchschnittlicher Alkoholgehalt"))][Html.text "Alkoholgehalt"]
                            , Html.button [onClick (ÄndereX (.temperatur, "Durchschnittliche Trinktemperatur"))][Html.text "Trinktemperatur"]
                            , Html.button [onClick (ÄndereX (.suesse, "Süße"))][Html.text "Süße"]
                            , Html.button [onClick (ÄndereX (.saeurengehalt, "Säuregehalt"))][Html.text "Säuregehalt"]
                            , Html.button [onClick (ÄndereX (.koerper, "Körper"))][Html.text "Körper"]
                            , Html.button [onClick (ÄndereX (.gerbstoff, "Gerbstoffe"))][Html.text "Gerbstoffe"]
                            , Html.button [onClick (ÄndereX (.preis, "Preis"))][Html.text "Preis"]
                            , Html.button [onClick (ÄndereX (.jahr, "Jahr"))][Html.text "Jahr"]
                            , Html.button [onClick (ÄndereX (.ml, "Mililiter"))][Html.text "Mililiter"]
                        ]
                    ]
                    , ul[][
                        li[][
                            Html.text <| "Suchen eine Eigenschaft für die Y-Achse aus"
                            , Html.button [onClick (ÄndereY (.alc, "Durchschnittlicher Alkoholgehalt"))][Html.text "Alkoholgehalt"]
                            , Html.button [onClick (ÄndereY (.temperatur, "Durchschnittliche Trinktemperatur"))][Html.text "Trinktemperatur"]
                            , Html.button [onClick (ÄndereY (.suesse, "Süße"))][Html.text "Süße"]
                            , Html.button [onClick (ÄndereY (.saeurengehalt, "Säuregehalt"))][Html.text "Säuregehalt"]
                            , Html.button [onClick (ÄndereY (.koerper, "Körper"))][Html.text "Körper"]
                            , Html.button [onClick (ÄndereY (.gerbstoff, "Gerbstoffe"))][Html.text "Gerbstoffe"]
                            , Html.button [onClick (ÄndereY (.preis, "Preis"))][Html.text "Preis"]
                            , Html.button [onClick (ÄndereY (.jahr, "Jahr"))][Html.text "Jahr"]
                            , Html.button [onClick (ÄndereY (.ml, "Mililiter"))][Html.text "Mililiter"]
                        ]
                    ] 
                    ,   scatterplot weine
                ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ErhalteText result ->
            case result of
                Ok fullText ->
                    ( Erfolg <| { daten = weineListe [ fullText ], xAAFunktion = .ml, yAAFunktion = .preis , xName = "Mililiter", yName = "Preis"}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ÄndereX (x, a) ->
            case model of
                Erfolg m ->
                    ( Erfolg <| { daten = m.daten, xAAFunktion = x, yAAFunktion = m.yAAFunktion, xName = a, yName = m.yName }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ÄndereY (y, a) ->
            case model of
                Erfolg m ->
                    ( Erfolg <| { daten = m.daten, xAAFunktion = m.xAAFunktion, yAAFunktion = y, xName = m.xName, yName = a }, Cmd.none )

                _ ->
                    ( model, Cmd.none )