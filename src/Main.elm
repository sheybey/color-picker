module Main exposing (main)

import Browser
import Hex
import Html
    exposing
        ( Html
        , button
        , div
        , input
        , label
        , main_
        , option
        , p
        , select
        , text
        )
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


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


type alias OutputFormat =
    { name : String
    , description : String
    , function : RGBA -> String
    }


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


type RGBAComponent
    = Red
    | Green
    | Blue
    | Alpha


find : List a -> (a -> Bool) -> Maybe a
find list test =
    List.head list
        |> Maybe.andThen
            (\x ->
                if test x then
                    Just x

                else
                    List.tail list
                        |> Maybe.andThen
                            (\tail ->
                                find tail test
                            )
            )


stringToFormat : String -> Maybe OutputFormat
stringToFormat f =
    find formats (\x -> x.name == f)


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


type alias Model =
    { redInput : String
    , greenInput : String
    , blueInput : String
    , alphaInput : String
    , color : RGBA
    , format : OutputFormat
    , renderBold : Bool
    , renderFont : String
    , previewBackground : String
    }


type Msg
    = ChangeRed String
    | ChangeGreen String
    | ChangeBlue String
    | ChangeAlpha String
    | ChangeFormat String
    | ChangeOutput String
    | ChangeBold Bool
    | ChangeFont String
    | ChangePreviewBackground String


formatOutput : Model -> String
formatOutput mdl =
    mdl.format.function mdl.color


optionFormat : OutputFormat -> Html Msg
optionFormat fmt =
    option [ value fmt.name ] [ text fmt.description ]


defaultFormat : OutputFormat
defaultFormat =
    { name = "source"
    , description = "Source Engine (R G B A)"
    , function = formatSource
    }


formats : List OutputFormat
formats =
    [ defaultFormat
    , { name = "rgba"
      , description = "HTML (rgba(R, G, B, A))"
      , function = formatRGBA
      }
    , { name = "hex"
      , description = "Hexadecimal (RRGGBBAA)"
      , function = formatHex
      }
    ]


defaultColor : RGBA
defaultColor =
    { rgb =
        { red = 0
        , green = 0
        , blue = 0
        }
    , alpha = 1.0
    }


model : Model
model =
    { redInput = "0"
    , blueInput = "0"
    , greenInput = "0"
    , alphaInput = "1"
    , color = defaultColor
    , format = defaultFormat
    , renderBold = False
    , renderFont = "sans-serif"
    , previewBackground = "#000000"
    }


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


update : Msg -> Model -> Model
update msg mdl =
    let
        oldColor =
            mdl.color

        oldRGB =
            oldColor.rgb
    in
    case msg of
        ChangeRed red ->
            case parseByte red of
                Just r ->
                    { mdl
                        | color = { oldColor | rgb = { oldRGB | red = r } }
                        , redInput = red
                    }

                Nothing ->
                    { mdl | redInput = red }

        ChangeGreen green ->
            case parseByte green of
                Just g ->
                    { mdl
                        | color = { oldColor | rgb = { oldRGB | green = g } }
                        , greenInput = green
                    }

                Nothing ->
                    { mdl | greenInput = green }

        ChangeBlue blue ->
            case parseByte blue of
                Just b ->
                    { mdl
                        | color = { oldColor | rgb = { oldRGB | blue = b } }
                        , blueInput = blue
                    }

                Nothing ->
                    { mdl | blueInput = blue }

        ChangeAlpha alpha ->
            case parseAlpha alpha of
                Just a ->
                    { mdl
                        | color = { oldColor | alpha = a }
                        , alphaInput = alpha
                    }

                Nothing ->
                    { mdl | alphaInput = alpha }

        ChangeFormat format ->
            let
                new_format =
                    Maybe.withDefault defaultFormat (stringToFormat format)
            in
            { mdl
                | format = new_format
            }

        ChangeOutput color_string ->
            case colorFromString color_string of
                Just new_color ->
                    { mdl
                        | color = new_color
                        , redInput = String.fromInt new_color.rgb.red
                        , greenInput = String.fromInt new_color.rgb.green
                        , blueInput = String.fromInt new_color.rgb.blue
                        , alphaInput = String.fromFloat new_color.alpha
                        , format = defaultFormat
                    }

                Nothing ->
                    mdl

        ChangeBold bold ->
            { mdl | renderBold = bold }

        ChangeFont font ->
            { mdl | renderFont = font }

        ChangePreviewBackground background ->
            { mdl | previewBackground = background }


inputStyles : List (Html.Attribute Msg)
inputStyles =
    [ class "bg-transparent order-0 py-1 px-1.5"
    , class "ring-blue-200 ring-1 rounded-sm text-sm"
    , class "focus:ring-2 active:ring-2"
    , class "focus:ring-blue-600 active:ring-blue-600"
    ]


inputConstraints : RGBAComponent -> List (Html.Attribute Msg)
inputConstraints component =
    let
        max =
            if component == Alpha then
                "1"

            else
                "255"

        step =
            if component == Alpha then
                "0.001"

            else
                "1"
    in
    [ Html.Attributes.min "0"
    , Html.Attributes.max max
    , Html.Attributes.step step
    ]


componentToMsg : RGBAComponent -> (String -> Msg)
componentToMsg component =
    case component of
        Red ->
            ChangeRed

        Green ->
            ChangeGreen

        Blue ->
            ChangeBlue

        Alpha ->
            ChangeAlpha


rangeInput : Model -> RGBAComponent -> Html Msg
rangeInput mdl component =
    let
        mdlValue =
            case component of
                Red ->
                    mdl.redInput

                Green ->
                    mdl.greenInput

                Blue ->
                    mdl.blueInput

                Alpha ->
                    mdl.alphaInput

        msg =
            componentToMsg component
    in
    input
        ([ type_ "range"
         , class "appearance-none h-full"
         , value mdlValue
         , style
            "background-image"
            (gradient mdl.color component)
         , autocomplete False
         , tabindex -1
         , onInput msg
         ]
            ++ inputConstraints component
        )
        []


numberInput : Model -> RGBAComponent -> Html Msg
numberInput mdl component =
    let
        rgb =
            mdl.color.rgb

        mdlValue =
            case component of
                Red ->
                    String.fromInt rgb.red

                Green ->
                    String.fromInt rgb.green

                Blue ->
                    String.fromInt rgb.blue

                Alpha ->
                    String.fromFloat mdl.color.alpha

        msg =
            componentToMsg component

        idValue =
            case component of
                Red ->
                    "red"

                Green ->
                    "green"

                Blue ->
                    "blue"

                Alpha ->
                    "alpha"
    in
    input
        ([ type_ "number"
         , id idValue
         , value mdlValue
         , autocomplete False
         , onInput msg
         ]
            ++ inputConstraints component
            ++ inputStyles
        )
        []


previewText : Model -> String -> Html Msg
previewText mdl background =
    p
        [ class "px-2 py-1"
        , classList [ ( "font-bold", mdl.renderBold ) ]
        , style "color" (formatRGBA mdl.color)
        , style "background-color" background
        , style "font-family" mdl.renderFont
        ]
        [ text ("This is the selected color against " ++ background ++ ".") ]


view : Model -> Html Msg
view mdl =
    main_
        [ class "flex items-center justify-center flex-wrap"
        , class "gap-5 w-full"
        ]
        [ div [ id "color-preview" ]
            [ div
                [ class "w-full h-full"
                , style "background-color" (formatRGBA mdl.color) ]
                []
            ]
        , div
            [ class "flex flex-col gap-2" ]
            [ div
                [ class "grid grid-cols-ranges gap-2"
                , class "justify-content-start items-center"
                ]
                [ label [ for "red" ] [ text "Red:" ]
                , rangeInput mdl Red
                , numberInput mdl Red
                , label [ for "green" ] [ text "Green: " ]
                , rangeInput mdl Green
                , numberInput mdl Green
                , label [ for "blue" ] [ text "Blue: " ]
                , rangeInput mdl Blue
                , numberInput mdl Blue
                , label [ for "alpha" ] [ text "Alpha: " ]
                , rangeInput mdl Alpha
                , numberInput mdl Alpha
                ]
            , div
                [ class "flex items-center justify-between flex-wrap mt-2 gap-y-2" ]
                [ div
                    [ class "flex items-center gap-2" ]
                    [ label [ for "render-bold" ] [ text "Bold" ]
                    , input
                        [ type_ "checkbox"
                        , id "render-bold"
                        , checked mdl.renderBold
                        , onInput (\_ -> ChangeBold (not mdl.renderBold))
                        ]
                        []
                    ]
                , div
                    [ class "flex items-center gap-2" ]
                    [ label [ for "render-font" ] [ text "Font:" ]
                    , select
                        ([ id "render-font"
                         , value mdl.renderFont
                         , onInput ChangeFont
                         ]
                            ++ inputStyles
                        )
                        [ option [ value "sans-serif" ] [ text "Sans Serif" ]
                        , option [ value "serif" ] [ text "Serif" ]
                        , option [ value "monospace" ] [ text "Monospace" ]
                        ]
                    ]
                ]
            , previewText mdl "white"
            , previewText mdl "black"
            , div
                [ class "flex gap-2 items-center" ]
                [ input
                    ([ type_ "color"
                     , value mdl.previewBackground
                     , onInput ChangePreviewBackground
                     ]
                        ++ inputStyles
                    )
                    []
                , previewText mdl mdl.previewBackground
                ]
            , select
                ([ id "format"
                 , onInput ChangeFormat
                 , value mdl.format.name
                 ]
                    ++ inputStyles
                )
                (List.map optionFormat formats)
            , div
                [ class "gap-2 flex items-center" ]
                [ input
                    ([ type_ "text"
                     , value (formatOutput mdl)
                     , onInput ChangeOutput
                     , class "flex-grow"
                     ]
                        ++ inputStyles
                    )
                    []
                ]
            ]
        ]

main =
    Browser.sandbox { init = model, update = update, view = view }
