module Color exposing (..)

import Hex


type alias HSV =
    { hue : Float
    , saturation : Float
    , value : Float
    }


type alias RGB =
    { red : Int
    , green : Int
    , blue : Int
    }


type alias RGBA =
    { rgb : RGB, alpha : Float }


type alias HSVA =
    { hsv : HSV, alpha : Float }


type RGBAComponent
    = Red
    | Green
    | Blue
    | Alpha


toHSV : RGB -> HSV
toHSV rgb =
    let
        red =
            byteToFloat rgb.red

        green =
            byteToFloat rgb.green

        blue =
            byteToFloat rgb.blue

        value =
            Basics.max red (Basics.max blue green)

        chroma =
            value - Basics.min red (Basics.min blue green)
    in
    { hue =
        if chroma == 0 then
            0

        else
            60
                * (if value == red then
                    (green - blue) / chroma

                   else if value == green then
                    2 + ((blue - red) / chroma)

                   else
                    -- if value == blue then
                    4 + ((red - green) / chroma)
                  )
    , saturation =
        if value == 0 then
            0

        else
            chroma / value
    , value = value
    }


toRGBF : HSV -> Int -> Float
toRGBF hsv n =
    let
        k =
            floor (toFloat n + hsv.hue / 60)
    in
    hsv.value
        - hsv.value
        * hsv.saturation
        * toFloat (Basics.max 0 (Basics.min k (Basics.min (4 - k) 1)))


toRGB : HSV -> RGB
toRGB hsv =
    let
        r =
            toRGBF hsv 5

        g =
            toRGBF hsv 3

        b =
            toRGBF hsv 1
    in
    { red = floatToByte r, green = floatToByte g, blue = floatToByte b }


toHSVA : RGBA -> HSVA
toHSVA rgba =
    { hsv = toHSV rgba.rgb, alpha = rgba.alpha }


toRGBA : HSVA -> RGBA
toRGBA hsva =
    { rgb = toRGB hsva.hsv, alpha = hsva.alpha }


toOctet : Int -> String
toOctet n =
    String.padLeft 2 '0' (Hex.toString n)


floatToByte : Float -> Int
floatToByte alpha =
    round (alpha * 255)


byteToFloat : Int -> Float
byteToFloat alpha =
    toFloat alpha / 255.0


isByte : Int -> Bool
isByte n =
    n >= 0 && n <= 255


parseByte : String -> Maybe Int
parseByte s =
    String.toInt s
        |> Maybe.andThen
            (\n ->
                if isByte n then
                    Just n

                else
                    Nothing
            )


parseAlpha : String -> Maybe Float
parseAlpha s =
    String.toFloat s
        |> Maybe.andThen
            (\f ->
                if f >= 0.0 && f <= 1.0 then
                    Just f

                else
                    Nothing
            )


formatRGBA : RGBA -> String
formatRGBA { rgb, alpha } =
    "rgba("
        ++ String.fromInt rgb.red
        ++ ", "
        ++ String.fromInt rgb.green
        ++ ", "
        ++ String.fromInt rgb.blue
        ++ ", "
        ++ String.fromFloat alpha
        ++ ")"


formatSource : RGBA -> String
formatSource { rgb, alpha } =
    String.join " "
        (List.map String.fromInt
            [ rgb.red, rgb.green, rgb.blue, floatToByte alpha ]
        )


formatHex : RGBA -> String
formatHex { rgb, alpha } =
    String.join ""
        (List.map toOctet
            [ rgb.red, rgb.green, rgb.blue, floatToByte alpha ]
        )


setComponent : RGBA -> RGBAComponent -> Int -> RGBA
setComponent rgba component value =
    let
        rgb =
            rgba.rgb
    in
    case component of
        Red ->
            { rgba | rgb = { rgb | red = value } }

        Green ->
            { rgba | rgb = { rgb | green = value } }

        Blue ->
            { rgba | rgb = { rgb | blue = value } }

        Alpha ->
            { rgba | alpha = byteToFloat value }


gradient : RGBA -> RGBAComponent -> String
gradient rgba component =
    "linear-gradient(to right,"
        ++ formatRGBA (setComponent rgba component 0)
        ++ ","
        ++ formatRGBA (setComponent rgba component 255)
        ++ ")"


colorFromSourceNumbers : Int -> Int -> Int -> Int -> Maybe RGBA
colorFromSourceNumbers r g b a =
    if List.all isByte [ r, g, b, a ] then
        Just { rgb = { red = r, green = g, blue = b }, alpha = byteToFloat a }

    else
        Nothing


colorFromSourceValues : String -> String -> String -> String -> Maybe RGBA
colorFromSourceValues r g b a =
    Maybe.withDefault
        Nothing
        (Maybe.map4
            colorFromSourceNumbers
            (String.toInt r)
            (String.toInt g)
            (String.toInt b)
            (String.toInt a)
        )


colorFromString : String -> Maybe RGBA
colorFromString sourceColor =
    let
        words =
            String.words sourceColor

        red =
            List.head words

        redTail =
            List.tail words

        green =
            Maybe.andThen List.head redTail

        greenTail =
            Maybe.andThen List.tail redTail

        blue =
            Maybe.andThen List.head greenTail

        blueTail =
            Maybe.andThen List.tail greenTail

        alpha =
            Maybe.andThen List.head blueTail
    in
    Maybe.withDefault
        Nothing
        (Maybe.map4 colorFromSourceValues red green blue alpha)
