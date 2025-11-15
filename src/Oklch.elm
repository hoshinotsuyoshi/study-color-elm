module Oklch exposing
    ( oklchToSrgbRaw
    , oklchToSrgbMapped
    , isInSrgbGamut
    , MappedResult
    )

-- Type for gamut mapping result


type alias MappedResult =
    { r : Float
    , g : Float
    , b : Float
    , mappedC : Float
    , inGamut : Bool
    }

-- OKLCH to Oklab conversion
-- OKLCH uses cylindrical coordinates (L, C, h) where h is in degrees


oklchToOklab : Float -> Float -> Float -> ( Float, Float, Float )
oklchToOklab l c h =
    let
        -- Convert hue from degrees to radians
        hRad =
            degrees h

        a =
            c * cos hRad

        b =
            c * sin hRad
    in
    ( l, a, b )



-- Oklab to XYZ D65 conversion
-- Using the standard Oklab transformation matrices


oklabToXyz : Float -> Float -> Float -> ( Float, Float, Float )
oklabToXyz l a b =
    let
        -- First inverse transformation (LMS)
        l_ =
            l + 0.3963377774 * a + 0.2158037573 * b

        m_ =
            l - 0.1055613458 * a - 0.0638541728 * b

        s_ =
            l - 0.0894841775 * a - 1.291485548 * b

        -- Cube to get linear LMS
        lLms =
            l_ * l_ * l_

        mLms =
            m_ * m_ * m_

        sLms =
            s_ * s_ * s_

        -- LMS to XYZ D65
        x =
            1.2270138511 * lLms - 0.5577999807 * mLms + 0.2812561489 * sLms

        y =
            -0.0405801784 * lLms + 1.1122568696 * mLms - 0.0716766787 * sLms

        z =
            -0.0763812845 * lLms - 0.4214819784 * mLms + 1.5861632204 * sLms
    in
    ( x, y, z )



-- XYZ D65 to linear sRGB


xyzToLinearSrgb : Float -> Float -> Float -> ( Float, Float, Float )
xyzToLinearSrgb x y z =
    let
        r =
            3.2404542 * x - 1.5371385 * y - 0.4985314 * z

        g =
            -0.969266 * x + 1.8760108 * y + 0.041556 * z

        b =
            0.0556434 * x - 0.2040259 * y + 1.0572252 * z
    in
    ( r, g, b )



-- Linear sRGB to gamma-corrected sRGB (single channel)


linearToSrgb : Float -> Float
linearToSrgb linear =
    if linear <= 0.0031308 then
        12.92 * linear

    else
        1.055 * (linear ^ (1 / 2.4)) - 0.055



-- Check if sRGB values are in gamut [0, 1]


isInSrgbGamut : ( Float, Float, Float ) -> Bool
isInSrgbGamut ( r, g, b ) =
    r >= 0 && r <= 1 && g >= 0 && g <= 1 && b >= 0 && b <= 1



-- Gamut mapping: Binary search to find maximum C that fits in sRGB gamut
-- Uses 24 iterations for precision (~6e-8)


gamutMapC : Float -> Float -> Float -> Float
gamutMapC l c h =
    let
        iterate : Int -> Float -> Float -> Float
        iterate n low high =
            if n <= 0 then
                low

            else
                let
                    mid =
                        (low + high) / 2

                    ( labL, labA, labB ) =
                        oklchToOklab l mid h

                    ( xyzX, xyzY, xyzZ ) =
                        oklabToXyz labL labA labB

                    ( linR, linG, linB ) =
                        xyzToLinearSrgb xyzX xyzY xyzZ

                    srgbR =
                        linearToSrgb linR

                    srgbG =
                        linearToSrgb linG

                    srgbB =
                        linearToSrgb linB
                in
                if isInSrgbGamut ( srgbR, srgbG, srgbB ) then
                    iterate (n - 1) mid high

                else
                    iterate (n - 1) low mid
    in
    iterate 24 0 c



-- Convert OKLCH to sRGB without gamut mapping (clipping)


oklchToSrgbRaw : Float -> Float -> Float -> ( Float, Float, Float )
oklchToSrgbRaw l c h =
    let
        ( labL, labA, labB ) =
            oklchToOklab l c h

        ( xyzX, xyzY, xyzZ ) =
            oklabToXyz labL labA labB

        ( linR, linG, linB ) =
            xyzToLinearSrgb xyzX xyzY xyzZ

        r =
            linearToSrgb linR

        g =
            linearToSrgb linG

        b =
            linearToSrgb linB
    in
    ( r, g, b )



-- Convert OKLCH to sRGB with gamut mapping
-- Returns: MappedResult record


oklchToSrgbMapped : Float -> Float -> Float -> MappedResult
oklchToSrgbMapped l c h =
    let
        ( rawR, rawG, rawB ) =
            oklchToSrgbRaw l c h

        inGamut =
            isInSrgbGamut ( rawR, rawG, rawB )
    in
    if inGamut then
        { r = rawR
        , g = rawG
        , b = rawB
        , mappedC = c
        , inGamut = True
        }

    else
        let
            mappedC =
                gamutMapC l c h

            ( mappedR, mappedG, mappedB ) =
                oklchToSrgbRaw l mappedC h
        in
        { r = mappedR
        , g = mappedG
        , b = mappedB
        , mappedC = mappedC
        , inGamut = False
        }
