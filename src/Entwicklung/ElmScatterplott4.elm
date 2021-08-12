module Entwicklung.ElmScatterplott4 exposing (..)

import Axis
import Html exposing (Html,text, pre)
import Http
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Length(..), Transform(..), px)
import Csv
import Csv.Decode
import Browser
import TypedSvg.Attributes exposing (name)
import Html exposing (ul)
import Html exposing (li)
import Html.Events exposing (onClick)

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
    , xAAFunktion : Weine -> Float
    , yAAFunktion : Weine -> Float
    , xName : String
    , yName : String
    }

init : () -> ( Model, Cmd Msg )
init _ =
    ( Loading
    , liste
        |> List.map
            (\datensatz ->
                Http.get
                    { url = "https://raw.githubusercontent.com/RicBre/Elm-Projekt-WineInformation/main/Daten/Aufbereitete%20Daten/" ++ datensatz
                    , expect = Http.expectString GotText
                    }
            )
        |> Cmd.batch
    )

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
    | ÄndereX (Weine -> Float, String)
    | ÄndereY (Weine -> Float, String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotText result ->
            case result of
                Ok fullText ->
                    ( Success <| { data = weineListe [ fullText ], xAAFunktion = .ml, yAAFunktion = .preis , xName = "Mililiter", yName = "Preis"}, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )
        ÄndereX (x, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunktion = x, yAAFunktion = m.yAAFunktion, xName = a, yName = m.yName }, Cmd.none )

                _ ->
                    ( model, Cmd.none )
        ÄndereY (y, a) ->
            case model of
                Success m ->
                    ( Success <| { data = m.data, xAAFunktion = m.xAAFunktion, yAAFunktion = y, xName = m.xName, yName = a }, Cmd.none )

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

w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )


scatterplot : XyData -> Svg msg
scatterplot model =
    let

        xValues : List Float
        xValues =
            List.map .x model.data

        yValues : List Float
        yValues =
            List.map .y model.data

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) / 2

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
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
            [ xAxis xValues
            , text_
                [ x (Scale.convert xScaleLocal labelPositions.x)
                , y 35

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)

                --, fontWeight FontWeightBold
                ]
                [ TypedSvg.Core.text model.xDescription ]
            ]
        , g [ transform [ Translate 60 60 ] ]
            [ yAxis yValues
            , text_
                [ x -30
                , y -30

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)

                --, fontWeight FontWeightBold
                ]
                [ TypedSvg.Core.text model.yDescription ]
            ]
        , g [ transform [ Translate padding padding ] ]
            (List.map (point xScaleLocal yScaleLocal) model.data)
        ]
        

point : ContinuousScale Float -> ContinuousScale Float -> Point -> Svg msg
point scaleX scaleY xyPoint =
    g
        [
            class["point"]
            ,fontSize <| Px 15.0
            ,fontFamily ["serif"]
            ,transform
                [
                    Translate
                    (Scale.convert scaleX xyPoint.x)
                    (Scale.convert scaleY xyPoint.y)
                ]
        ]

        [
            circle [cx 0, cy 0, r 5] []
            , text_ [x 10, y -20, textAnchor AnchorMiddle] [Html.text xyPoint.pointName]
        ]


type alias Point =
    { pointName : String, x : Float, y : Float }


type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }

xScale : List Float -> ContinuousScale Float
xScale values =
    Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )


yScale : List Float -> ContinuousScale Float
yScale values =
    Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )
 
addieren : (Float, Float) -> Float-> (Float, Float) 
addieren (min, max) shift =
    if min <= 0 then
        ( 0, max + shift)
    else 
        (min - shift, max + shift)
    
 
wideExtent : List Float -> ( Float, Float )
wideExtent values = 
    let
        result = 
            Maybe.withDefault (0, 0)
            (Statistics.extent values)
        
        max =          
            Maybe.withDefault (0)
            (List.maximum values)
            
        result1 = 
            addieren result (toFloat(tickCount)*max/50)
        
        result2 = 
            addieren result1 (0.0)
        
          
    in
     result2
    
     
     
 
 
xAxis : List Float -> Svg msg
xAxis values =
    Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


yAxis : List Float -> Svg msg
yAxis values =
    Axis.left [ Axis.tickCount tickCount ] (yScale values)

filterAndReduceWines : List Weine -> (Weine -> String) -> (Weine -> Float) -> (Weine -> Float) -> String -> String -> XyData
filterAndReduceWines weinliste a b c x y =
    XyData x y (List.map (\n -> pointName n a b c x y) weinliste)


pointName : Weine -> (Weine -> String) -> (Weine -> Float) -> (Weine -> Float) -> String -> String -> Point
pointName weine u v x y z =
    Point (u weine ++ ", " ++ y ++ ": " ++ String.fromFloat (v weine) ++ "," ++ z ++ ": " ++ String.fromFloat (x weine)) (v weine) (x weine)


-- VIEW
view : Model -> Html Msg
view model =
    case model of
        Failure ->
            Html.text "Ich konnte Ihre Weine nicht öffnen."

        Loading ->
            Html.text "Weine werden geöffnet..."

        Success l ->
            let
                weine =
                    filterAndReduceWines l.data .name l.xAAFunktion l.yAAFunktion l.xName l.yName

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
