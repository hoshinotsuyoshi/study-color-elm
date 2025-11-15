{-
OKLCH Color Converter

An interactive color space converter with visual demonstrations:
- OKLCH to sRGB/Display P3 conversion
- Gamut mapping for out-of-gamut colors
- CIE 1931 xy chromaticity diagram
- URL-based color sharing

Based on Oklab color space by Björn Ottosson
https://bottosson.github.io/posts/oklab/
-}


port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (Html, div, input, text, label, p, h1, h2, h3, span, button, a, footer)
import Html.Attributes exposing (style, type_, min, max, step, value, href, target, rel)
import Html.Events exposing (onInput, onClick)
import Oklch exposing (oklchToSrgbRaw, oklchToSrgbMapped, oklchToP3Raw, oklchToP3Mapped)
import ChromaticityDiagram
import Svg
import Svg.Attributes
import Url
import Url.Parser exposing (Parser, (<?>))
import Url.Parser.Query as Query
import Process
import Task


-- MODEL


type Tab
    = ChromaticityTab
    | ColorsTab


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , l : Float
    , c : Float
    , h : Float
    , result : ResultColor
    , activeTab : Tab
    , copiedText : Maybe String
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
      , activeTab = ColorsTab
      , copiedText = Nothing
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
    | SwitchTab Tab
    | CopyToClipboard String
    | ClearCopied


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

        SwitchTab tab ->
            ( { model | activeTab = tab }
            , Cmd.none
            )

        CopyToClipboard text ->
            ( { model | copiedText = Just text }
            , Cmd.batch
                [ copyToClipboard text
                , Task.perform (always ClearCopied) (Process.sleep 2000)
                ]
            )

        ClearCopied ->
            ( { model | copiedText = Nothing }
            , Cmd.none
            )



-- PORTS


port copyToClipboard : String -> Cmd msg


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



-- CHROMATICITY DIAGRAM HELPERS


computeXy : Float -> Float -> Float -> { x : Float, y : Float }
computeXy l c h =
    let
        ( labL, labA, labB ) =
            oklchToOklabLocal l c h

        ( xyzX, xyzY, xyzZ ) =
            oklabToXyzLocal labL labA labB

        ( xyX, xyY ) =
            xyzToXyLocal ( xyzX, xyzY, xyzZ )
    in
    { x = xyX, y = xyY }


oklchToOklabLocal : Float -> Float -> Float -> ( Float, Float, Float )
oklchToOklabLocal l c hDeg =
    let
        hRad =
            degrees hDeg

        a =
            c * cos hRad

        b =
            c * sin hRad
    in
    ( l, a, b )


oklabToXyzLocal : Float -> Float -> Float -> ( Float, Float, Float )
oklabToXyzLocal l a b =
    let
        l_ =
            l + 0.3963377774 * a + 0.2158037573 * b

        m_ =
            l - 0.1055613458 * a - 0.0638541728 * b

        s_ =
            l - 0.0894841775 * a - 1.291485548 * b

        lLms =
            l_ * l_ * l_

        mLms =
            m_ * m_ * m_

        sLms =
            s_ * s_ * s_

        x =
            1.2270138511 * lLms - 0.5577999807 * mLms + 0.2812561489 * sLms

        y =
            -0.0405801784 * lLms + 1.1122568696 * mLms - 0.0716766787 * sLms

        z =
            -0.0763812845 * lLms - 0.4214819784 * mLms + 1.5861632204 * sLms
    in
    ( x, y, z )


xyzToXyLocal : ( Float, Float, Float ) -> ( Float, Float )
xyzToXyLocal ( x, y, z ) =
    let
        sum =
            x + y + z
    in
    if sum < 1.0e-10 then
        ( 0, 0 )

    else
        ( x / sum, y / sum )



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
                [ viewSlider "L (Lightness)" model.l 0 1 0.01 ChangeL
                , viewSlider "C (Chroma)" model.c 0 0.4 0.01 ChangeC
                , viewSlider "h (Hue)" model.h 0 360 1 ChangeH
                ]
            , div [ style "margin" "2rem 0" ]
                [ viewOklchColorBox model.l model.c model.h
                ]
            , viewTabs model
            , viewFooter
            ]
        ]
    }


viewTabs : Model -> Html Msg
viewTabs model =
    div [ style "margin" "2rem 0" ]
        [ -- Tab buttons
          div [ style "display" "flex", style "gap" "0", style "margin-bottom" "1rem" ]
            [ button
                [ onClick (SwitchTab ColorsTab)
                , style "flex" "1"
                , style "padding" "0.5rem 1rem"
                , style "border" "1px solid #ccc"
                , style "border-radius" "4px 0 0 4px"
                , style "background-color"
                    (if model.activeTab == ColorsTab then
                        "#5a7fa0"
                     else
                        "#fff"
                    )
                , style "color"
                    (if model.activeTab == ColorsTab then
                        "#fff"
                     else
                        "#333"
                    )
                , style "cursor" "pointer"
                ]
                [ text "sRGB/P3" ]
            , button
                [ onClick (SwitchTab ChromaticityTab)
                , style "flex" "1"
                , style "padding" "0.5rem 1rem"
                , style "border" "1px solid #ccc"
                , style "border-radius" "0 4px 4px 0"
                , style "background-color"
                    (if model.activeTab == ChromaticityTab then
                        "#5a7fa0"
                     else
                        "#fff"
                    )
                , style "color"
                    (if model.activeTab == ChromaticityTab then
                        "#fff"
                     else
                        "#333"
                    )
                , style "cursor" "pointer"
                ]
                [ text "Diagram (xy plot)" ]
            ]
        , -- Tab content
          case model.activeTab of
            ChromaticityTab ->
                div []
                    [ viewChromaticity model
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

            ColorsTab ->
                div []
                    [ h3 [] [ text "sRGB" ]
                    , viewColorBox "Raw sRGB (no gamut mapping)" model.result.srgbRaw Nothing model.copiedText
                    , viewColorBox "Gamut Mapped sRGB" model.result.srgbMapped (Just (rgbToHexString model.result.srgbMapped)) model.copiedText
                    , h3 [ style "margin-top" "2rem" ] [ text "Display P3" ]
                    , viewP3ColorBox "Raw display-p3 (no gamut mapping)" model.result.p3Raw
                    , viewP3ColorBox "Gamut Mapped display-p3" model.result.p3Mapped
                    ]
        ]


viewFooter : Html Msg
viewFooter =
    footer
        [ style "margin-top" "3rem"
        , style "padding-top" "2rem"
        , style "border-top" "1px solid #ddd"
        , style "text-align" "center"
        ]
        [ a
            [ href "https://github.com/hoshinotsuyoshi/study-color-elm"
            , target "_blank"
            , rel "noopener noreferrer"
            , style "display" "inline-flex"
            , style "align-items" "center"
            , style "gap" "0.5rem"
            , style "color" "#333"
            , style "text-decoration" "none"
            ]
            [ Svg.svg
                [ Svg.Attributes.width "24"
                , Svg.Attributes.height "24"
                , Svg.Attributes.viewBox "0 0 16 16"
                , Svg.Attributes.fill "currentColor"
                ]
                [ Svg.path
                    [ Svg.Attributes.d "M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0016 8c0-4.42-3.58-8-8-8z"
                    ]
                    []
                ]
            , text "GitHub"
            ]
        ]


viewChromaticity : Model -> Html Msg
viewChromaticity model =
    let
        xyPoint =
            computeXy model.l model.c model.h
    in
    div [ style "max-width" "800px", style "margin" "0 auto" ]
        [ Svg.svg
            [ Svg.Attributes.viewBox "0 0 1 1"
            , Svg.Attributes.width "100%"
            , Svg.Attributes.style "display: block;"
            ]
            [ ChromaticityDiagram.viewContent
            , ChromaticityDiagram.plotPoint xyPoint
            ]
        ]


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


viewColorBox : String -> ( Float, Float, Float ) -> Maybe String -> Maybe String -> Html Msg
viewColorBox title ( r, g, b ) maybeHex copiedText =
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
                div
                    [ style "display" "flex"
                    , style "align-items" "center"
                    , style "gap" "0.5rem"
                    , style "margin-top" "0.25rem"
                    ]
                    [ p
                        [ style "font-size" "0.9rem"
                        , style "color" "#666"
                        , style "margin" "0"
                        ]
                        [ text ("hex: " ++ hexString) ]
                    , button
                        [ onClick (CopyToClipboard hexString)
                        , style "padding" "0.1rem 0.4rem"
                        , style "font-size" "0.75rem"
                        , style "border" "1px solid #ccc"
                        , style "border-radius" "4px"
                        , style "background-color" "#f5f5f5"
                        , style "cursor" "pointer"
                        , style "line-height" "1.2"
                        ]
                        [ text
                            (if copiedText == Just hexString then
                                "Copied!"
                             else
                                "Copy"
                            )
                        ]
                    ]

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
            , style "height" "32px"
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
