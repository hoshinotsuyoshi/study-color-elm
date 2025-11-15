module Main exposing (main)

import Browser
import Html exposing (Html, div, input, text, label, p, h1, h2, span)
import Html.Attributes exposing (style, type_, min, max, step, value)
import Html.Events exposing (onInput)
import Oklch exposing (oklchToSrgbRaw, oklchToSrgbMapped)


-- MODEL


type alias Model =
    { l : Float
    , c : Float
    , h : Float
    , result : ResultColor
    }


type alias ResultColor =
    { srgbRaw : ( Float, Float, Float )
    , srgbMapped : ( Float, Float, Float )
    , mappedC : Float
    , inGamut : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        initialL =
            0.7

        initialC =
            0.15

        initialH =
            150
    in
    ( { l = initialL
      , c = initialC
      , h = initialH
      , result = computeResult initialL initialC initialH
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = ChangeL String
    | ChangeC String
    | ChangeH String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeL str ->
            let
                newL =
                    String.toFloat str |> Maybe.withDefault model.l
            in
            ( { model
                | l = newL
                , result = computeResult newL model.c model.h
              }
            , Cmd.none
            )

        ChangeC str ->
            let
                newC =
                    String.toFloat str |> Maybe.withDefault model.c
            in
            ( { model
                | c = newC
                , result = computeResult model.l newC model.h
              }
            , Cmd.none
            )

        ChangeH str ->
            let
                newH =
                    String.toFloat str |> Maybe.withDefault model.h
            in
            ( { model
                | h = newH
                , result = computeResult model.l model.c newH
              }
            , Cmd.none
            )


computeResult : Float -> Float -> Float -> ResultColor
computeResult l c h =
    let
        raw =
            oklchToSrgbRaw l c h

        mapped =
            oklchToSrgbMapped l c h
    in
    { srgbRaw = raw
    , srgbMapped = ( mapped.r, mapped.g, mapped.b )
    , mappedC = mapped.mappedC
    , inGamut = mapped.inGamut
    }



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "font-family" "system-ui, -apple-system, sans-serif"
        , style "max-width" "800px"
        , style "margin" "2rem auto"
        , style "padding" "2rem"
        ]
        [ h1 [] [ text "OKLCH → sRGB + Gamut Mapping Demo" ]
        , div [ style "margin" "2rem 0" ]
            [ h2 [] [ text "Input" ]
            , viewSlider "L (Lightness)" model.l 0 1 0.01 ChangeL
            , viewSlider "C (Chroma)" model.c 0 0.4 0.01 ChangeC
            , viewSlider "h (Hue)" model.h 0 360 1 ChangeH
            , viewOklchColorBox model.l model.c model.h
            ]
        , div [ style "margin" "2rem 0" ]
            [ h2 [] [ text "Results (Elm computed)" ]
            , viewColorBox "Raw sRGB (no gamut mapping)" model.result.srgbRaw
            , viewColorBox "Gamut Mapped sRGB" model.result.srgbMapped
            ]
        , div
            [ style "margin" "2rem 0"
            , style "padding" "1rem"
            , style "background-color" "#f5f5f5"
            , style "border-radius" "8px"
            ]
            [ p []
                [ span [ style "font-weight" "bold" ] [ text "Out of Gamut: " ]
                , span
                    [ style "color"
                        (if model.result.inGamut then
                            "green"

                         else
                            "red"
                        )
                    ]
                    [ text
                        (if model.result.inGamut then
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
                [ span [ style "font-weight" "bold" ] [ text "Mapped C: " ]
                , text (String.fromFloat (roundTo 4 model.result.mappedC))
                ]
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


viewColorBox : String -> ( Float, Float, Float ) -> Html Msg
viewColorBox title ( r, g, b ) =
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
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
