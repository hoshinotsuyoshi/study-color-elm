{-
CIE 1931 xy Chromaticity Diagram

This module renders the CIE 1931 xy chromaticity diagram including:
- Spectral locus (wavelengths 380-780nm)
- sRGB and Display P3 gamut triangles
- Background color field visualization
- Real-time color plotting

Data source:
• CIE 1931 2° Standard Observer color matching functions
  Data from CVRL (Colour & Vision Research Laboratory)
  http://cvrl.org/
-}


module ChromaticityDiagram exposing (view, viewContent, plotPoint)

import Svg exposing (Svg, svg, rect, polyline, circle, polygon, path)
import Svg.Attributes exposing (..)


-- CIE 1931 2° Standard Observer Color Matching Functions
-- Wavelengths from 380nm to 780nm in 5nm steps


type alias CMF =
    { wavelength : Float
    , x : Float
    , y : Float
    , z : Float
    }


cie1931CMFs : List CMF
cie1931CMFs =
    [ { wavelength = 380, x = 0.001368, y = 0.000039, z = 0.006450 }
    , { wavelength = 385, x = 0.002236, y = 0.000064, z = 0.010550 }
    , { wavelength = 390, x = 0.004243, y = 0.000120, z = 0.020050 }
    , { wavelength = 395, x = 0.007650, y = 0.000217, z = 0.036210 }
    , { wavelength = 400, x = 0.014310, y = 0.000396, z = 0.067850 }
    , { wavelength = 405, x = 0.023190, y = 0.000640, z = 0.110200 }
    , { wavelength = 410, x = 0.043510, y = 0.001210, z = 0.207400 }
    , { wavelength = 415, x = 0.077630, y = 0.002180, z = 0.371300 }
    , { wavelength = 420, x = 0.134380, y = 0.004000, z = 0.645600 }
    , { wavelength = 425, x = 0.214770, y = 0.007300, z = 1.039050 }
    , { wavelength = 430, x = 0.283900, y = 0.011600, z = 1.385600 }
    , { wavelength = 435, x = 0.328500, y = 0.016840, z = 1.622960 }
    , { wavelength = 440, x = 0.348280, y = 0.023000, z = 1.747060 }
    , { wavelength = 445, x = 0.348060, y = 0.029800, z = 1.782600 }
    , { wavelength = 450, x = 0.336200, y = 0.038000, z = 1.772110 }
    , { wavelength = 455, x = 0.318700, y = 0.048000, z = 1.744100 }
    , { wavelength = 460, x = 0.290800, y = 0.060000, z = 1.669200 }
    , { wavelength = 465, x = 0.251100, y = 0.073900, z = 1.528100 }
    , { wavelength = 470, x = 0.195360, y = 0.090980, z = 1.287640 }
    , { wavelength = 475, x = 0.142100, y = 0.112600, z = 1.041900 }
    , { wavelength = 480, x = 0.095640, y = 0.139020, z = 0.812950 }
    , { wavelength = 485, x = 0.057950, y = 0.169300, z = 0.616200 }
    , { wavelength = 490, x = 0.032010, y = 0.208020, z = 0.465180 }
    , { wavelength = 495, x = 0.014700, y = 0.258600, z = 0.353300 }
    , { wavelength = 500, x = 0.004900, y = 0.323000, z = 0.272000 }
    , { wavelength = 505, x = 0.002400, y = 0.407300, z = 0.212300 }
    , { wavelength = 510, x = 0.009300, y = 0.503000, z = 0.158200 }
    , { wavelength = 515, x = 0.029100, y = 0.608200, z = 0.111700 }
    , { wavelength = 520, x = 0.063270, y = 0.710000, z = 0.078250 }
    , { wavelength = 525, x = 0.109600, y = 0.793200, z = 0.057250 }
    , { wavelength = 530, x = 0.165500, y = 0.862000, z = 0.042160 }
    , { wavelength = 535, x = 0.225750, y = 0.914850, z = 0.029840 }
    , { wavelength = 540, x = 0.290400, y = 0.954000, z = 0.020300 }
    , { wavelength = 545, x = 0.359700, y = 0.980300, z = 0.013400 }
    , { wavelength = 550, x = 0.433450, y = 0.994950, z = 0.008750 }
    , { wavelength = 555, x = 0.512050, y = 1.000000, z = 0.005750 }
    , { wavelength = 560, x = 0.594500, y = 0.995000, z = 0.003900 }
    , { wavelength = 565, x = 0.678400, y = 0.978600, z = 0.002750 }
    , { wavelength = 570, x = 0.762100, y = 0.952000, z = 0.002100 }
    , { wavelength = 575, x = 0.842500, y = 0.915400, z = 0.001800 }
    , { wavelength = 580, x = 0.916300, y = 0.870000, z = 0.001650 }
    , { wavelength = 585, x = 0.978600, y = 0.816300, z = 0.001400 }
    , { wavelength = 590, x = 1.026300, y = 0.757000, z = 0.001100 }
    , { wavelength = 595, x = 1.056700, y = 0.694900, z = 0.001000 }
    , { wavelength = 600, x = 1.062200, y = 0.631000, z = 0.000800 }
    , { wavelength = 605, x = 1.045600, y = 0.566800, z = 0.000600 }
    , { wavelength = 610, x = 1.002600, y = 0.503000, z = 0.000340 }
    , { wavelength = 615, x = 0.938400, y = 0.441200, z = 0.000240 }
    , { wavelength = 620, x = 0.854450, y = 0.381000, z = 0.000190 }
    , { wavelength = 625, x = 0.751400, y = 0.321000, z = 0.000100 }
    , { wavelength = 630, x = 0.642400, y = 0.265000, z = 0.000050 }
    , { wavelength = 635, x = 0.541900, y = 0.217000, z = 0.000030 }
    , { wavelength = 640, x = 0.447900, y = 0.175000, z = 0.000020 }
    , { wavelength = 645, x = 0.360800, y = 0.138200, z = 0.000010 }
    , { wavelength = 650, x = 0.283500, y = 0.107000, z = 0.000000 }
    , { wavelength = 655, x = 0.218700, y = 0.081600, z = 0.000000 }
    , { wavelength = 660, x = 0.164900, y = 0.061000, z = 0.000000 }
    , { wavelength = 665, x = 0.121200, y = 0.044580, z = 0.000000 }
    , { wavelength = 670, x = 0.087400, y = 0.032000, z = 0.000000 }
    , { wavelength = 675, x = 0.063600, y = 0.023200, z = 0.000000 }
    , { wavelength = 680, x = 0.046770, y = 0.017000, z = 0.000000 }
    , { wavelength = 685, x = 0.032900, y = 0.011920, z = 0.000000 }
    , { wavelength = 690, x = 0.022700, y = 0.008210, z = 0.000000 }
    , { wavelength = 695, x = 0.015840, y = 0.005723, z = 0.000000 }
    , { wavelength = 700, x = 0.011359, y = 0.004102, z = 0.000000 }
    , { wavelength = 705, x = 0.008111, y = 0.002929, z = 0.000000 }
    , { wavelength = 710, x = 0.005790, y = 0.002091, z = 0.000000 }
    , { wavelength = 715, x = 0.004109, y = 0.001484, z = 0.000000 }
    , { wavelength = 720, x = 0.002899, y = 0.001047, z = 0.000000 }
    , { wavelength = 725, x = 0.002049, y = 0.000740, z = 0.000000 }
    , { wavelength = 730, x = 0.001440, y = 0.000520, z = 0.000000 }
    , { wavelength = 735, x = 0.001000, y = 0.000361, z = 0.000000 }
    , { wavelength = 740, x = 0.000690, y = 0.000249, z = 0.000000 }
    , { wavelength = 745, x = 0.000476, y = 0.000172, z = 0.000000 }
    , { wavelength = 750, x = 0.000332, y = 0.000120, z = 0.000000 }
    , { wavelength = 755, x = 0.000235, y = 0.000085, z = 0.000000 }
    , { wavelength = 760, x = 0.000166, y = 0.000060, z = 0.000000 }
    , { wavelength = 765, x = 0.000117, y = 0.000042, z = 0.000000 }
    , { wavelength = 770, x = 0.000083, y = 0.000030, z = 0.000000 }
    , { wavelength = 775, x = 0.000059, y = 0.000021, z = 0.000000 }
    , { wavelength = 780, x = 0.000042, y = 0.000015, z = 0.000000 }
    ]



-- COLOR CONVERSION FUNCTIONS


xyzToXy : ( Float, Float, Float ) -> ( Float, Float )
xyzToXy ( x, y, z ) =
    let
        sum =
            x + y + z
    in
    if sum < 1.0e-10 then
        ( 0, 0 )

    else
        ( x / sum, y / sum )


xyYToXyz : Float -> Float -> Float -> ( Float, Float, Float )
xyYToXyz xCoord yCoord yValue =
    if yCoord < 1.0e-10 then
        ( 0, 0, 0 )

    else
        let
            xVal =
                (yValue / yCoord) * xCoord

            zVal =
                (yValue / yCoord) * (1 - xCoord - yCoord)
        in
        ( xVal, yValue, zVal )


xyzToLinearSrgb : ( Float, Float, Float ) -> ( Float, Float, Float )
xyzToLinearSrgb ( x, y, z ) =
    let
        r =
            3.2404542 * x - 1.5371385 * y - 0.4985314 * z

        g =
            -0.969266 * x + 1.8760108 * y + 0.041556 * z

        b =
            0.0556434 * x - 0.2040259 * y + 1.0572252 * z
    in
    ( r, g, b )


linearToSrgb : Float -> Float
linearToSrgb linear =
    if linear <= 0.0031308 then
        12.92 * linear

    else
        1.055 * (linear ^ (1 / 2.4)) - 0.055


isInSrgbGamut : ( Float, Float, Float ) -> Bool
isInSrgbGamut ( r, g, b ) =
    r >= 0 && r <= 1 && g >= 0 && g <= 1 && b >= 0 && b <= 1



-- SPECTRAL LOCUS COMPUTATION


computeSpectralLocus : List ( Float, Float )
computeSpectralLocus =
    cie1931CMFs
        |> List.map
            (\cmf ->
                let
                    ( xCoord, yCoord ) =
                        xyzToXy ( cmf.x, cmf.y, cmf.z )
                in
                ( xCoord, yCoord )
            )



-- GAMUT TRIANGLES


srgbPrimaries : List ( Float, Float )
srgbPrimaries =
    let
        -- sRGB primaries in linear RGB
        rPrimary =
            ( 1, 0, 0 )

        gPrimary =
            ( 0, 1, 0 )

        bPrimary =
            ( 0, 0, 1 )

        -- Convert to XYZ using sRGB → XYZ matrix (inverse of xyzToLinearSrgb)
        linearSrgbToXyz ( r, g, b ) =
            ( 0.4124564 * r + 0.3575761 * g + 0.1804375 * b
            , 0.2126729 * r + 0.7151522 * g + 0.0721750 * b
            , 0.0193339 * r + 0.1191920 * g + 0.9503041 * b
            )
    in
    [ rPrimary, gPrimary, bPrimary ]
        |> List.map (linearSrgbToXyz >> xyzToXy)


p3Primaries : List ( Float, Float )
p3Primaries =
    let
        -- Display-P3 primaries in linear RGB
        rPrimary =
            ( 1, 0, 0 )

        gPrimary =
            ( 0, 1, 0 )

        bPrimary =
            ( 0, 0, 1 )

        -- Convert to XYZ using P3 → XYZ matrix
        linearP3ToXyz ( r, g, b ) =
            ( 0.4865709486 * r + 0.2656676932 * g + 0.1982172852 * b
            , 0.2289745640 * r + 0.6917385218 * g + 0.0792869141 * b
            , 0.0000000000 * r + 0.0451133816 * g + 1.0439444780 * b
            )
    in
    [ rPrimary, gPrimary, bPrimary ]
        |> List.map (linearP3ToXyz >> xyzToXy)



-- BACKGROUND RENDERING


xyToSrgbColor : Float -> Float -> String
xyToSrgbColor xCoord yCoord =
    let
        -- Use Y = 0.5 for mid-luminance
        ( xVal, yVal, zVal ) =
            xyYToXyz xCoord yCoord 0.5

        ( linR, linG, linB ) =
            xyzToLinearSrgb ( xVal, yVal, zVal )

        -- Gamma correct
        sR =
            linearToSrgb linR

        sG =
            linearToSrgb linG

        sB =
            linearToSrgb linB

        -- Clamp to [0,1]
        clampedR =
            clamp 0 1 sR

        clampedG =
            clamp 0 1 sG

        clampedB =
            clamp 0 1 sB

        -- Convert to 0-255
        r255 =
            round (clampedR * 255)

        g255 =
            round (clampedG * 255)

        b255 =
            round (clampedB * 255)
    in
    "rgb(" ++ String.fromInt r255 ++ "," ++ String.fromInt g255 ++ "," ++ String.fromInt b255 ++ ")"


renderBackground : Int -> Svg msg
renderBackground gridSize =
    let
        step =
            1.0 / toFloat gridSize
    in
    Svg.g []
        (List.range 0 (gridSize - 1)
            |> List.concatMap
                (\i ->
                    List.range 0 (gridSize - 1)
                        |> List.map
                            (\j ->
                                let
                                    -- xy chromaticity coordinates (center of cell)
                                    xCoord =
                                        (toFloat i + 0.5) * step

                                    yCoord =
                                        (toFloat j + 0.5) * step

                                    -- SVG coordinates (top-left of cell)
                                    svgX =
                                        toFloat i * step

                                    -- Flip Y axis: SVG (0,0) is top-left, xy (0,0) is bottom-left
                                    svgY =
                                        1.0 - (toFloat (j + 1) * step)
                                in
                                rect
                                    [ x (String.fromFloat svgX)
                                    , y (String.fromFloat svgY)
                                    , width (String.fromFloat step)
                                    , height (String.fromFloat step)
                                    , fill (xyToSrgbColor xCoord yCoord)
                                    , stroke "none"
                                    ]
                                    []
                            )
                )
        )



-- HELPER FUNCTIONS FOR SVG


pointsToPath : List ( Float, Float ) -> String
pointsToPath points =
    points
        |> List.map (\( xVal, yVal ) -> String.fromFloat xVal ++ "," ++ String.fromFloat (1 - yVal))
        |> String.join " "


trianglePoints : List ( Float, Float ) -> String
trianglePoints points =
    case points of
        [ p1, p2, p3 ] ->
            pointsToPath [ p1, p2, p3 ]

        _ ->
            ""



-- MAIN VIEW FUNCTION


viewContent : Svg msg
viewContent =
    Svg.g []
        [ -- Background chromaticity field
          renderBackground 60
        , -- Spectral locus (horseshoe)
          polyline
            [ points (pointsToPath computeSpectralLocus)
            , fill "none"
            , stroke "black"
            , strokeWidth "0.002"
            ]
            []
        , -- Close the horseshoe with purple line
          let
                firstPoint =
                    List.head computeSpectralLocus |> Maybe.withDefault ( 0, 0 )

                lastPoint =
                    List.reverse computeSpectralLocus |> List.head |> Maybe.withDefault ( 0, 0 )
            in
            polyline
                [ points (pointsToPath [ firstPoint, lastPoint ])
                , fill "none"
                , stroke "black"
                , strokeWidth "0.002"
                , strokeDasharray "0.01,0.01"
                ]
                []
        , -- sRGB gamut triangle
          polygon
            [ points (trianglePoints srgbPrimaries)
            , fill "none"
            , stroke "white"
            , strokeWidth "0.003"
            ]
            []
        , -- Display-P3 gamut triangle
          polygon
            [ points (trianglePoints p3Primaries)
            , fill "none"
            , stroke "#00ff00"
            , strokeWidth "0.003"
            ]
            []
        ]


view : Svg msg
view =
    svg
        [ viewBox "0 0 1 1"
        , width "800"
        , height "800"
        ]
        [ viewContent ]


plotPoint : { x : Float, y : Float } -> Svg msg
plotPoint point =
    circle
        [ cx (String.fromFloat point.x)
        , cy (String.fromFloat (1 - point.y))
        , r "0.01"
        , fill "red"
        , stroke "white"
        , strokeWidth "0.002"
        ]
        []
