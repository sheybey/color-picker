port module Main exposing (main)

import Accessibility exposing (contrastToString, getContrast)
import Browser
import Color exposing (..)
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
        , span
        , text
        )
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


port copy : String -> Cmd msg


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


type alias OutputFormat =
    { name : String
    , description : String
    , function : RGBA -> String
    }


stringToFormat : String -> Maybe OutputFormat
stringToFormat f =
    find formats (\x -> x.name == f)


createOption : String -> ( String, String ) -> Html Msg
createOption mdl ( value_, text_ ) =
    option [ value value_, selected (value_ == mdl) ] [ text text_ ]


optionFormat : Model -> OutputFormat -> Html Msg
optionFormat mdl fmt =
    createOption mdl.format.name ( fmt.name, fmt.description )


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


type alias Model =
    { redInput : String
    , greenInput : String
    , blueInput : String
    , alphaInput : String
    , color : RGBA
    , format : OutputFormat
    , renderBold : Bool
    , renderSize : String
    , renderFont : String
    , previewBackground : String
    }


type ChangeModelMsg
    = ChangeRed String
    | ChangeGreen String
    | ChangeBlue String
    | ChangeAlpha String
    | ChangeFormat String
    | ChangeOutput String
    | ChangeBold Bool
    | ChangeSize String
    | ChangeFont String
    | ChangePreviewBackground String


type Msg
    = CopyOutput
    | ChangeModel ChangeModelMsg


changeModel : (a -> ChangeModelMsg) -> a -> Msg
changeModel msg a =
    ChangeModel (msg a)


formatOutput : Model -> String
formatOutput mdl =
    mdl.format.function mdl.color


model : Model
model =
    { redInput = "0"
    , blueInput = "0"
    , greenInput = "0"
    , alphaInput = "1"
    , color = defaultColor
    , format = defaultFormat
    , renderBold = False
    , renderSize = "text-md"
    , renderFont = "sans-serif"
    , previewBackground = "#000000"
    }


updateModel : ChangeModelMsg -> Model -> Model
updateModel msg mdl =
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

        ChangeSize size ->
            { mdl | renderSize = size }

        ChangePreviewBackground background ->
            { mdl | previewBackground = background }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg mdl =
    case msg of
        ChangeModel changeMsg ->
            ( updateModel changeMsg mdl, Cmd.none )

        CopyOutput ->
            ( mdl, copy (mdl.format.function mdl.color) )


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


componentToChangeMsg : RGBAComponent -> (String -> ChangeModelMsg)
componentToChangeMsg component =
    case component of
        Red ->
            ChangeRed

        Green ->
            ChangeGreen

        Blue ->
            ChangeBlue

        Alpha ->
            ChangeAlpha


componentToMsg : RGBAComponent -> String -> Msg
componentToMsg component s =
    ChangeModel (componentToChangeMsg component s)


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


roundTo : Int -> Float -> String
roundTo places f =
    let
        mult =
            toFloat (10 ^ places)
    in
    String.fromFloat (toFloat (round (mult * f)) / mult)


previewText : Model -> String -> Html Msg
previewText mdl background =
    let
        contrast =
            getContrast mdl.color (backgroundColor background)

        ratio =
            roundTo 2 contrast.ratio
    in
    div []
        [ span [ class "pb-2 text-sm" ]
            [ text "Contrast: "
            , text ratio
            , text ", "
            , text (contrastToString contrast.normal)
            , text ". Large text: "
            , text (contrastToString contrast.large)
            ]
        , p
            [ class "px-2 py-1"
            , class mdl.renderSize
            , classList [ ( "font-bold", mdl.renderBold ) ]
            , style "color" (formatRGBA mdl.color)
            , style "background-color" background
            , style "font-family" mdl.renderFont
            ]
            [ text "This is the selected color against "
            , text background
            , text "."
            ]
        ]


previewSizes : List ( String, String )
previewSizes =
    [ ( "text-sm", "Small" )
    , ( "text-md", "Medium" )
    , ( "text-lg", "Large" )
    , ( "text-xl", "XL" )
    , ( "text-2xl", "2XL" )
    , ( "text-3xl", "3XL" )
    ]


previewFonts : List ( String, String )
previewFonts =
    [ ( "sans-serif", "Sans Serif" )
    , ( "serif", "Serif" )
    , ( "monospace", "Monospace" )
    ]


backgroundColor : String -> RGB
backgroundColor color =
    let
        black =
            { red = 0, green = 0, blue = 0 }
    in
    case color of
        "white" ->
            { red = 255, green = 255, blue = 255 }

        "black" ->
            black

        _ ->
            colorFromHexString color
                |> Result.withDefault black


view : Model -> Html Msg
view mdl =
    main_
        [ class "flex items-center justify-center flex-wrap"
        , class "gap-5 w-full"
        ]
        [ div [ id "color-preview" ]
            [ div
                [ class "w-full h-full"
                , style "background-color" (formatRGBA mdl.color)
                ]
                []
            ]
        , div
            [ class "flex flex-col gap-2 max-w-full px-4" ]
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
                [ class "flex items-center justify-between flex-wrap"
                , class "mt-2 gap-y-2"
                ]
                [ div
                    [ class "flex items-center gap-2" ]
                    [ label [ for "render-bold" ] [ text "Bold:" ]
                    , input
                        [ type_ "checkbox"
                        , id "render-bold"
                        , checked mdl.renderBold
                        , onInput (\_ -> changeModel ChangeBold (not mdl.renderBold))
                        ]
                        []
                    ]
                , div
                    [ class "flex items-center gap-2" ]
                    [ label [ for "render-size" ] [ text "Size:" ]
                    , select
                        ([ id "render-size"
                         , value mdl.renderSize
                         , onInput (changeModel ChangeSize)
                         ]
                            ++ inputStyles
                        )
                        (List.map
                            (createOption mdl.renderSize)
                            previewSizes
                        )
                    ]
                , div
                    [ class "flex items-center gap-2" ]
                    [ label [ for "render-font" ] [ text "Font:" ]
                    , select
                        ([ id "render-font"
                         , value mdl.renderFont
                         , onInput (changeModel ChangeFont)
                         ]
                            ++ inputStyles
                        )
                        (List.map
                            (createOption mdl.renderFont)
                            previewFonts
                        )
                    ]
                ]
            , previewText mdl "white"
            , previewText mdl "black"
            , div
                [ class "flex gap-2 items-center" ]
                [ input
                    ([ type_ "color"
                     , value mdl.previewBackground
                     , onInput (changeModel ChangePreviewBackground)
                     ]
                        ++ inputStyles
                    )
                    []
                , previewText mdl mdl.previewBackground
                ]
            , div [ class "flex gap-2 items-center" ]
                [ label [ for "format" ] [ text "Format:" ]
                , select
                    ([ id "format"
                     , class "flex-grow"
                     , onInput (changeModel ChangeFormat)
                     , value mdl.format.name
                     ]
                        ++ inputStyles
                    )
                    (List.map (optionFormat mdl) formats)
                ]
            , div
                [ class "gap-2 flex items-center" ]
                [ button (onClick CopyOutput :: inputStyles) [ text "Copy" ]
                , input
                    ([ type_ "text"
                     , value (formatOutput mdl)
                     , onInput (changeModel ChangeOutput)
                     , class "flex-grow"
                     ]
                        ++ inputStyles
                    )
                    []
                ]
            ]
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
