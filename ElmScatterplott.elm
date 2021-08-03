-- Für Ellie


module Main exposing (main)

-- Für lokale Elm-Installation: Modul-Name muss gleich dem Dateinamen.elm sein
--module Scatterplot1_1_public exposing (main)

import Axis
import Html exposing (Html)
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg, text)
import TypedSvg.Types exposing (AnchorAlignment(..), FontWeight(..), Length(..), Transform(..), px)
import Csv
import Csv.Decode


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
        filteredCarsHead =
            filterAndReduceCars cars

        kreisbeschriftung : String
        kreisbeschriftung =
            Maybe.withDefault "Keine Beschriftung"
                (Maybe.map .pointName (List.head filteredCarsHead.data))

        xValues : List Float
        xValues =
            List.map .x model.data

        yValues : List Float
        yValues =
            --[0, 200000]
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
        
         , g[ transform [ Translate (60) (390)]]
            [
                xAxis xValues
                , text_
                [ x (Scale.convert xScaleLocal labelPositions.x)
                , y 35

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)

                --, fontWeight FontWeightBold
                ]
                [ text filteredCarsHead.xDescription ]
                ]
                
         ,g[transform [Translate(60) (60)]]
         [
             yAxis yValues
             , text_
                [ x -30
                , y -30

                -- , fontFamily [ "Helvetica", "sans-serif" ]
                , fontSize (px 20)

                --, fontWeight FontWeightBold
                ]
                [ text filteredCarsHead.yDescription ]
             ]
             
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

filterAndReduceCars : List Car -> XyData
filterAndReduceCars my_cars =
    XyData "city MPG" "retail Price" (List.filterMap maybePoint my_cars)


pointName : String -> Int -> Int -> Point
pointName x y z =
    Point (x ++ ", " ++ String.fromInt y ++ ", " ++ String.fromInt z) (toFloat y) (toFloat z)


maybePoint : Car -> Maybe Point
maybePoint i =
    Maybe.map3 pointName (Just i.vehicleName) i.cityMPG i.retailPrice


main : Html msg
main =
    let
        filteredCars =
            filterAndReduceCars cars

        numberCars =
            List.length cars

        numberFilterCars =
            List.length filteredCars.data
    in
    Html.div []
        [ Html.p []
            [ Html.text "Original Car list:"
            , Html.text <| String.fromInt numberCars
            , Html.text ", reduced Car list:"
            , Html.text <| String.fromInt numberFilterCars

            --,Html.text <| String.fromList firstElement
            ]
        , scatterplot filteredCars
        ]

-- Importierung der Daten aus CSV
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

liste : List String
liste =
    [ "cleansingWine.csv" ]


csvString_to_data : String -> List ( String, Maybe Float, Maybe Float )
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeStockDay
        |> Result.toMaybe
        |> Maybe.withDefault []


decodeStockDay : Csv.Decode.Decoder (( String, Maybe Float, Maybe Float ) -> a) a
decodeStockDay =
    Csv.Decode.map (\a b c -> ( a, Just b, Just c ))
        (Csv.Decode.field "name" Ok
            |> Csv.Decode.andMap
                (Csv.Decode.field "price"
                    (String.toFloat >> Result.fromMaybe "error parsing string")
                    |> Csv.Decode.andMap
                        (Csv.Decode.field "year"
                            (String.toFloat >> Result.fromMaybe "error parsing string")
                        )
                )
        )


umwandeln : List ( String, Maybe Float, Maybe Float ) -> List ( String, String, String )
umwandeln ganzerText =
    List.map (\( a, b, c ) -> ( a, b |> Maybe.map String.fromFloat |> Maybe.withDefault "Kein Wert vorhanden", c |> Maybe.map String.fromFloat |> Maybe.withDefault "Kein Wert vorhanden")) ganzerText


umwandeln2 : List ( String, Maybe Float ) -> String
umwandeln2 ganzerText =
    List.map Tuple.first ganzerText
        |> String.concat





type CarType
    = Small_Sporty_Compact_Large_Sedan
    | Sports_Car
    | SUV
    | Wagon
    | Minivan
    | Pickup


type WheelDrive
    = All_Wheel_Drive
    | Rear_Wheel_Drive
    | Front_Wheel_Drive


type alias Car =
    { vehicleName : String
    , carType : CarType
    , wheelDrive : WheelDrive
    , retailPrice : Maybe Int
    , dealerCost : Maybe Int
    , engineSize : Maybe Float
    , cyl : Maybe Float
    , hp : Maybe Int
    , cityMPG : Maybe Int
    , hwyMPG : Maybe Int
    , weight : Maybe Int
    , wheelBase : Maybe Int
    , carLen : Maybe Int
    , carWidth : Maybe Int
    }


cars : List Car
cars =
    [ Car "Acura 3.5 RL 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 43755) (Just 39014) (Just 3.5) (Just 6) (Just 225) (Just 18) (Just 24) (Just 3880) (Just 115) (Just 197) (Just 72)
    , Car "Acura 3.5 RL w/Navigation 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 46100) (Just 41100) (Just 3.5) (Just 6) (Just 225) (Just 18) (Just 24) (Just 3893) (Just 115) (Just 197) (Just 72)
    , Car "Volvo XC90 T6" SUV All_Wheel_Drive (Just 41250) (Just 38851) (Just 2.9) (Just 6) (Just 268) (Just 15) (Just 20) (Just 4638) (Just 113) (Just 189) (Just 75)
    ]
