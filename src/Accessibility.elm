module Accessibility exposing (..)

import Color exposing (..)


type WCAGContrast
    = Enhanced
    | Minimum
    | Insufficient


type alias ContrastResult =
    { normal : WCAGContrast
    , large : WCAGContrast
    , ratio : Float
    }


contrastToString : WCAGContrast -> String
contrastToString contrast =
    case contrast of
        Enhanced ->
            "enhanced"

        Minimum ->
            "minimum"

        Insufficient ->
            "insufficient"


premultiply : Float -> Int -> Float -> Int -> Int
premultiply w1 c1 w2 c2 =
    roundByte ((w1 * toFloat c1) + (w2 * toFloat c2))


compositeColor : RGBA -> RGB -> RGB
compositeColor color background =
    let
        fgWeight =
            color.alpha

        bgWeight =
            1.0 - fgWeight
    in
    { red = premultiply fgWeight color.rgb.red bgWeight background.red
    , green = premultiply fgWeight color.rgb.green bgWeight background.green
    , blue = premultiply fgWeight color.rgb.blue bgWeight background.blue
    }



-- luminance: https://www.w3.org/TR/WCAG21/#dfn-relative-luminance


mapLForSRGB : Float -> Float
mapLForSRGB f =
    if f <= 0.04045 then
        f / 12.92

    else
        ((f + 0.055) / 1.055) ^ 2.4


relativeLuminance : RGB -> Float
relativeLuminance color =
    let
        r =
            mapLForSRGB (byteToFloat color.red)

        g =
            mapLForSRGB (byteToFloat color.green)

        b =
            mapLForSRGB (byteToFloat color.blue)
    in
    0.2126 * r + 0.7152 * g + 0.0722 * b


contrastRatio : RGBA -> RGB -> Float
contrastRatio fg bg =
    let
        efg =
            compositeColor fg bg

        l1 =
            0.05 + relativeLuminance efg

        l2 =
            0.05 + relativeLuminance bg
    in
    max l1 l2 / min l1 l2


getContrast : RGBA -> RGB -> ContrastResult
getContrast text bg =
    let
        ratio =
            contrastRatio text bg
    in
    if ratio >= 7.0 then
        { normal = Enhanced, large = Enhanced, ratio = ratio }

    else if ratio >= 4.5 then
        { normal = Minimum, large = Enhanced, ratio = ratio }

    else if ratio >= 3.0 then
        { normal = Insufficient, large = Minimum, ratio = ratio }

    else
        { normal = Insufficient, large = Insufficient, ratio = ratio }
