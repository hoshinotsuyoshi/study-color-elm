module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, input, text, label, p, h1, h2, h3, span)
import Html.Attributes exposing (style, type_, min, max, step, value)
import Html.Events exposing (onInput)
import Oklch exposing (oklchToSrgbRaw, oklchToSrgbMapped, oklchToP3Raw, oklchToP3Mapped)
import Url
import Url.Parser exposing (Parser, (<?>))
import Url.Parser.Query as Query


-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , l : Float
    , c : Float
    , h : Float
    , result : ResultColor
    }


type alias ResultColor =
    { srgbRaw : ( Float, Float, Float )
    , srgbMapped : ( Float, Float, Float )
    , p3Raw : ( Float, Float, Float )
    , p3Mapped : ( Float, Float, Float )
    , mappedCSrgb : Float
    , mappedCP3 : Float
    , inGamutSrgb : Bool
    , inGamutP3 : Bool
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        params =
            parseUrlParams url

        initialL =
            Maybe.withDefault 0.7 params.l

        initialC =
            Maybe.withDefault 0.15 params.c

        initialH =
            Maybe.withDefault 150 params.h
    in
    ( { key = key
      , url = url
      , l = initialL
      , c = initialC
      , h = initialH
      , result = computeResult initialL initialC initialH
      }
    , Cmd.none
    )


type alias UrlParams =
    { l : Maybe Float
    , c : Maybe Float
    , h : Maybe Float
    }


parseUrlParams : Url.Url -> UrlParams
parseUrlParams url =
    let
        query =
            url.query |> Maybe.withDefault ""

        parser =
            Query.map3 UrlParams
                (Query.custom "l" parseFloatParam)
                (Query.custom "c" parseFloatParam)
                (Query.custom "h" parseFloatParam)

        parseFloatParam : List String -> Maybe Float
        parseFloatParam values =
            values
                |> List.head
                |> Maybe.andThen String.toFloat
    in
    case Url.Parser.parse (Url.Parser.top <?> parser) { url | path = "/" } of
        Just params ->
            params

        Nothing ->
            { l = Nothing, c = Nothing, h = Nothing }



-- UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | ChangeL String
    | ChangeC String
    | ChangeH String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                params =
                    parseUrlParams url

                newL =
                    Maybe.withDefault model.l params.l

                newC =
                    Maybe.withDefault model.c params.c

                newH =
                    Maybe.withDefault model.h params.h
            in
            ( { model
                | url = url
                , l = newL
                , c = newC
                , h = newH
                , result = computeResult newL newC newH
              }
            , Cmd.none
            )

        ChangeL str ->
            let
                newL =
                    String.toFloat str |> Maybe.withDefault model.l

                newUrl =
                    buildUrl model.url newL model.c model.h
            in
            ( { model
                | l = newL
                , result = computeResult newL model.c model.h
              }
            , Nav.pushUrl model.key newUrl
            )

        ChangeC str ->
            let
                newC =
                    String.toFloat str |> Maybe.withDefault model.c

                newUrl =
                    buildUrl model.url model.l newC model.h
            in
            ( { model
                | c = newC
                , result = computeResult model.l newC model.h
              }
            , Nav.pushUrl model.key newUrl
            )

        ChangeH str ->
            let
                newH =
                    String.toFloat str |> Maybe.withDefault model.h

                newUrl =
                    buildUrl model.url model.l model.c newH
            in
            ( { model
                | h = newH
                , result = computeResult model.l model.c newH
              }
            , Nav.pushUrl model.key newUrl
            )


buildUrl : Url.Url -> Float -> Float -> Float -> String
buildUrl url l c h =
    let
        base =
            Url.toString { url | query = Nothing, fragment = Nothing }

        query =
            "?l=" ++ String.fromFloat l ++ "&c=" ++ String.fromFloat c ++ "&h=" ++ String.fromFloat h
    in
    base ++ query


computeResult : Float -> Float -> Float -> ResultColor
computeResult l c h =
    let
        srgbRaw =
            oklchToSrgbRaw l c h

        srgbMapped =
            oklchToSrgbMapped l c h

        p3Raw =
            oklchToP3Raw l c h

        p3Mapped =
            oklchToP3Mapped l c h
    in
    { srgbRaw = srgbRaw
    , srgbMapped = ( srgbMapped.r, srgbMapped.g, srgbMapped.b )
    , p3Raw = p3Raw
    , p3Mapped = ( p3Mapped.r, p3Mapped.g, p3Mapped.b )
    , mappedCSrgb = srgbMapped.mappedC
    , mappedCP3 = p3Mapped.mappedC
    , inGamutSrgb = srgbMapped.inGamut
    , inGamutP3 = p3Mapped.inGamut
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "OKLCH Color Demo"
    , body =
        [ div
            [ style "font-family" "system-ui, -apple-system, sans-serif"
            , style "max-width" "800px"
            , style "margin" "2rem auto"
            , style "padding" "2rem"
            ]
            [ h1 [] [ text "OKLCH → sRGB + P3 + Gamut Mapping Demo" ]
            , div [ style "margin" "2rem 0" ]
                [ h2 [] [ text "Input" ]
                , viewSlider "L (Lightness)" model.l 0 1 0.01 ChangeL
                , viewSlider "C (Chroma)" model.c 0 0.4 0.01 ChangeC
                , viewSlider "h (Hue)" model.h 0 360 1 ChangeH
                , viewOklchColorBox model.l model.c model.h
                ]
            , div [ style "margin" "2rem 0" ]
                [ h2 [] [ text "Results (Elm computed)" ]
                , h3 [] [ text "sRGB" ]
                , viewColorBox "Raw sRGB (no gamut mapping)" model.result.srgbRaw Nothing
                , viewColorBox "Gamut Mapped sRGB" model.result.srgbMapped (Just (rgbToHexString model.result.srgbMapped))
                , h3 [ style "margin-top" "2rem" ] [ text "Display P3" ]
                , viewP3ColorBox "Raw display-p3 (no gamut mapping)" model.result.p3Raw
                , viewP3ColorBox "Gamut Mapped display-p3" model.result.p3Mapped
                ]
            , div
                [ style "margin" "2rem 0"
                , style "padding" "1rem"
                , style "background-color" "#f5f5f5"
                , style "border-radius" "8px"
                ]
                [ p []
                    [ span [ style "font-weight" "bold" ] [ text "Out of Gamut (sRGB): " ]
                    , span
                        [ style "color"
                            (if model.result.inGamutSrgb then
                                "green"

                             else
                                "red"
                            )
                        ]
                        [ text
                            (if model.result.inGamutSrgb then
                                "No"

                             else
                                "Yes"
                            )
                        ]
                    ]
                , p []
                    [ span [ style "font-weight" "bold" ] [ text "Out of Gamut (P3): " ]
                    , span
                        [ style "color"
                            (if model.result.inGamutP3 then
                                "green"

                             else
                                "red"
                            )
                        ]
                        [ text
                            (if model.result.inGamutP3 then
                                "No"

                             else
                                "Yes"
                            )
                        ]
                    ]
                , p []
                    [ span [ style "font-weight" "bold" ] [ text "Original C: " ]
                    , text (String.fromFloat (roundTo 4 model.c))
                    ]
                , p []
                    [ span [ style "font-weight" "bold" ] [ text "Mapped C (sRGB): " ]
                    , text (String.fromFloat (roundTo 4 model.result.mappedCSrgb))
                    ]
                , p []
                    [ span [ style "font-weight" "bold" ] [ text "Mapped C (P3): " ]
                    , text (String.fromFloat (roundTo 4 model.result.mappedCP3))
                    ]
                ]
            ]
        ]
    }


viewSlider : String -> Float -> Float -> Float -> Float -> (String -> Msg) -> Html Msg
viewSlider labelText currentValue minVal maxVal stepVal msg =
    div [ style "margin" "1rem 0" ]
        [ label
            [ style "display" "block"
            , style "margin-bottom" "0.5rem"
            , style "font-weight" "bold"
            ]
            [ text (labelText ++ ": " ++ String.fromFloat (roundTo 3 currentValue)) ]
        , input
            [ type_ "range"
            , Html.Attributes.min (String.fromFloat minVal)
            , Html.Attributes.max (String.fromFloat maxVal)
            , Html.Attributes.step (String.fromFloat stepVal)
            , value (String.fromFloat currentValue)
            , onInput msg
            , style "width" "100%"
            ]
            []
        ]


viewColorBox : String -> ( Float, Float, Float ) -> Maybe String -> Html Msg
viewColorBox title ( r, g, b ) maybeHex =
    div [ style "margin" "1rem 0" ]
        [ p [ style "font-weight" "bold" ] [ text title ]
        , div
            [ style "width" "100%"
            , style "height" "100px"
            , style "background-color" (rgbToString r g b)
            , style "border" "2px solid #333"
            , style "border-radius" "8px"
            ]
            []
        , p
            [ style "font-size" "0.9rem"
            , style "color" "#666"
            , style "margin-top" "0.5rem"
            ]
            [ text ("rgb(" ++ String.fromFloat (roundTo 3 r) ++ ", " ++ String.fromFloat (roundTo 3 g) ++ ", " ++ String.fromFloat (roundTo 3 b) ++ ")") ]
        , case maybeHex of
            Just hexString ->
                p
                    [ style "font-size" "0.9rem"
                    , style "color" "#666"
                    , style "margin-top" "0.25rem"
                    ]
                    [ text ("hex: " ++ hexString) ]

            Nothing ->
                text ""
        ]


viewP3ColorBox : String -> ( Float, Float, Float ) -> Html Msg
viewP3ColorBox title ( r, g, b ) =
    div [ style "margin" "1rem 0" ]
        [ p [ style "font-weight" "bold" ] [ text title ]
        , div
            [ style "width" "100%"
            , style "height" "100px"
            , style "background-color" (p3ToString r g b)
            , style "border" "2px solid #333"
            , style "border-radius" "8px"
            ]
            []
        , p
            [ style "font-size" "0.9rem"
            , style "color" "#666"
            , style "margin-top" "0.5rem"
            ]
            [ text ("color(display-p3 " ++ String.fromFloat (roundTo 3 r) ++ " " ++ String.fromFloat (roundTo 3 g) ++ " " ++ String.fromFloat (roundTo 3 b) ++ ")") ]
        ]


viewOklchColorBox : Float -> Float -> Float -> Html Msg
viewOklchColorBox l c h =
    let
        oklchString =
            "oklch(" ++ String.fromFloat (roundTo 3 l) ++ " " ++ String.fromFloat (roundTo 3 c) ++ " " ++ String.fromFloat (roundTo 1 h) ++ ")"
    in
    div [ style "margin" "1.5rem 0" ]
        [ p [ style "font-weight" "bold" ] [ text "CSS oklch() (Browser Native)" ]
        , div
            [ style "width" "100%"
            , style "height" "100px"
            , style "background-color" oklchString
            , style "border" "2px solid #333"
            , style "border-radius" "8px"
            ]
            []
        , p
            [ style "font-size" "0.9rem"
            , style "color" "#666"
            , style "margin-top" "0.5rem"
            ]
            [ text oklchString ]
        ]


rgbToString : Float -> Float -> Float -> String
rgbToString r g b =
    let
        r255 =
            clamp 0 255 (round (r * 255))

        g255 =
            clamp 0 255 (round (g * 255))

        b255 =
            clamp 0 255 (round (b * 255))
    in
    "rgb(" ++ String.fromInt r255 ++ ", " ++ String.fromInt g255 ++ ", " ++ String.fromInt b255 ++ ")"


p3ToString : Float -> Float -> Float -> String
p3ToString r g b =
    "color(display-p3 " ++ String.fromFloat r ++ " " ++ String.fromFloat g ++ " " ++ String.fromFloat b ++ ")"


rgbToHexString : ( Float, Float, Float ) -> String
rgbToHexString ( r, g, b ) =
    let
        toChannelHex value =
            let
                v255 =
                    clamp 0 255 (round (value * 255))
            in
            toHex2 v255

        toHex2 n =
            let
                high =
                    n // 16

                low =
                    modBy 16 n
            in
            hexDigit high ++ hexDigit low

        hexDigit n =
            case n of
                0 ->
                    "0"

                1 ->
                    "1"

                2 ->
                    "2"

                3 ->
                    "3"

                4 ->
                    "4"

                5 ->
                    "5"

                6 ->
                    "6"

                7 ->
                    "7"

                8 ->
                    "8"

                9 ->
                    "9"

                10 ->
                    "A"

                11 ->
                    "B"

                12 ->
                    "C"

                13 ->
                    "D"

                14 ->
                    "E"

                15 ->
                    "F"

                _ ->
                    "0"
    in
    "#" ++ toChannelHex r ++ toChannelHex g ++ toChannelHex b


roundTo : Int -> Float -> Float
roundTo decimals val =
    let
        factor =
            10 ^ toFloat decimals
    in
    toFloat (round (val * factor)) / factor



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }
