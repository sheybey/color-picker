module Main exposing (main)

import Browser
import Html exposing (Html, Attribute, div, input, text, main_, label, p, select, option)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


type alias OutputFormat =
    { name : String
    , description : String
    , function : Color -> String
    }


type alias Color =
    { red : Int
    , green : Int
    , blue : Int
    , alpha : Float
    }


type alias Model =
    { red : Int
    , green : Int
    , blue : Int
    , alpha : Float
    , format : OutputFormat
    }


type Msg
    = ChangeRed String
    | ChangeGreen String
    | ChangeBlue String
    | ChangeAlpha String
    | ChangeFormat String
    | ChangeOutput String


find : List a -> (a -> Bool) -> Maybe a
find list test =
    case List.head list of
        Just x ->
            if test x then
                Just x
            else
                case List.tail list of
                    Just t ->
                        find t test

                    Nothing ->
                        Nothing

        Nothing ->
            Nothing


string_to_format : String -> Maybe OutputFormat
string_to_format f =
    find formats (\x -> x.name == f)


model_to_color : Model -> Color
model_to_color mdl =
    { red = mdl.red, green = mdl.green, blue = mdl.blue, alpha = mdl.alpha }


to_hex_string n =
    (if (n > 15) then
        to_hex_string (n // 16)
     else
        ""
    )
        ++ case modBy 16 n of
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
                "This doesn't seem physically possible!"


to_octet : Int -> String
to_octet n =
    String.padLeft 2 '0' (to_hex_string n)


interp : Float -> Int
interp alpha =
    round (alpha * 255)


reverse_interp : Int -> Float
reverse_interp alpha =
    (toFloat alpha) / 255.0


clamp : number -> number -> number -> number
clamp value minimum maximum =
    Basics.min (Basics.max value minimum) maximum


format_rgba : Color -> String
format_rgba { red, green, blue, alpha } =
    "rgba("
        ++ (String.fromInt red)
        ++ ", "
        ++ (String.fromInt green)
        ++ ", "
        ++ (String.fromInt blue)
        ++ ", "
        ++ (String.fromFloat alpha)
        ++ ")"


format_source : Color -> String
format_source { red, green, blue, alpha } =
    String.join " "
        (List.map String.fromInt
            [ red, green, blue, interp alpha ]
        )


format_hex : Color -> String
format_hex { red, green, blue, alpha } =
    String.join ""
        (List.map to_octet
            [ red, green, blue, interp alpha ]
        )


format_output : Model -> String
format_output mdl =
    mdl.format.function (model_to_color mdl)


gradient : Color -> Color -> String
gradient from to =
    "linear-gradient(to right, "
        ++ format_rgba from
        ++ ","
        ++ format_rgba to
        ++ ")"


option_format : OutputFormat -> Html Msg
option_format fmt =
    option [ value fmt.name ] [ text fmt.description ]


default_format : OutputFormat
default_format =
    { name = "source"
    , description = "Source Engine (R G B A)"
    , function = format_source
    }


formats : List OutputFormat
formats =
    [ default_format
    , { name = "rgba"
      , description = "HTML (rgba(R, G, B, A))"
      , function = format_rgba
      }
    , { name = "hex"
      , description = "Hexadecimal (RRGGBBAA)"
      , function = format_hex
      }
    ]


model : Model
model =
    { red = 0
    , green = 0
    , blue = 0
    , alpha = 1.0
    , format = default_format
    }


unpack_maybe : List (Maybe a) -> Maybe a
unpack_maybe list =
    case List.head list of
        Just maybe ->
            maybe

        Nothing ->
            Nothing


unpack_maybe_tail : List (Maybe a) -> (List (Maybe a) -> Maybe b) -> Maybe b
unpack_maybe_tail list callback =
    case List.tail list of
        Just tail ->
            callback tail

        Nothing ->
            Nothing


list_to_alpha : List (Maybe Int) -> Maybe { alpha : Float }
list_to_alpha alpha =
    case unpack_maybe alpha of
        Just a ->
            Just { alpha = clamp (reverse_interp a) 0 1 }

        Nothing ->
            Nothing


list_to_blue_alpha : List (Maybe Int) -> Maybe { blue : Int, alpha : Float }
list_to_blue_alpha blue_alpha =
    case unpack_maybe blue_alpha of
        Just blue ->
            case unpack_maybe_tail blue_alpha list_to_alpha of
                Just a ->
                    Just { blue = clamp blue 0 255, alpha = a.alpha }

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


list_to_green_blue_alpha :
    List (Maybe Int)
    ->
        Maybe
            { green : Int
            , blue : Int
            , alpha : Float
            }
list_to_green_blue_alpha green_blue_alpha =
    case unpack_maybe green_blue_alpha of
        Just green ->
            case unpack_maybe_tail green_blue_alpha list_to_blue_alpha of
                Just blue_alpha ->
                    Just
                        { green = clamp green 0 255
                        , blue = blue_alpha.blue
                        , alpha = blue_alpha.alpha
                        }

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


model_from_maybe_list : List (Maybe Int) -> Maybe Model
model_from_maybe_list red_green_blue_alpha =
    case unpack_maybe red_green_blue_alpha of
        Just red ->
            case
                (unpack_maybe_tail
                    red_green_blue_alpha
                    list_to_green_blue_alpha
                )
            of
                Just green_blue_alpha ->
                    Just
                        { format = default_format
                        , red = clamp red 0 255
                        , green = green_blue_alpha.green
                        , blue = green_blue_alpha.blue
                        , alpha = green_blue_alpha.alpha
                        }

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


model_from_string : String -> Maybe Model
model_from_string source_color =
    model_from_maybe_list (List.map (String.toInt) (String.words source_color))


update : Msg -> Model -> Model
update msg mdl =
    case msg of
        ChangeRed red ->
            { mdl | red = Maybe.withDefault 0 (String.toInt red) }

        ChangeGreen green ->
            { mdl | green = Maybe.withDefault 0 (String.toInt green) }

        ChangeBlue blue ->
            { mdl | blue = Maybe.withDefault 0 (String.toInt blue) }

        ChangeAlpha alpha ->
            { mdl | alpha = Maybe.withDefault 1.0 (String.toFloat alpha) }

        ChangeFormat format ->
            { mdl
                | format =
                    Maybe.withDefault
                        default_format
                        (string_to_format format)
            }

        ChangeOutput source_color ->
            Maybe.withDefault mdl (model_from_string source_color)


view : Model -> Html Msg
view mdl =
    main_ []
        [ div [ id "color-preview" ]
            [ div
                [ style "background-color" (format_rgba (model_to_color mdl))
                ]
                []
            ]
        , div []
            [ div [ id "controls" ]
                [ div [ id "labels" ]
                    [ label [ for "red" ] [ text "Red:" ]
                    , label [ for "green" ] [ text "Green: " ]
                    , label [ for "blue" ] [ text "Blue: " ]
                    , label [ for "alpha" ] [ text "Alpha: " ]
                    ]
                , div [ id "ranges" ]
                    [ input
                        [ type_ "range"
                        , class "red"
                        , value (String.fromInt mdl.red)
                        , style
                            "background-image"
                            (gradient
                                (model_to_color { mdl | red = 0 })
                                (model_to_color { mdl | red = 255 })
                            )
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "255"
                        , autocomplete False
                        , onInput ChangeRed
                        ]
                        []
                    , input
                        [ type_ "range"
                        , class "green"
                        , value (String.fromInt mdl.green)
                        , style
                            "background-image"
                            (gradient
                                (model_to_color { mdl | green = 0 })
                                (model_to_color { mdl | green = 255 })
                            )
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "255"
                        , autocomplete False
                        , onInput ChangeGreen
                        ]
                        []
                    , input
                        [ type_ "range"
                        , class "blue"
                        , value (String.fromInt mdl.blue)
                        , style
                            "background-image"
                            (gradient
                                (model_to_color { mdl | blue = 0 })
                                (model_to_color { mdl | blue = 255 })
                            )
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "255"
                        , autocomplete False
                        , onInput ChangeBlue
                        ]
                        []
                    , input
                        [ type_ "range"
                        , class "alpha"
                        , value (String.fromFloat mdl.alpha)
                        , style
                            "background-image"
                            (gradient
                                (model_to_color { mdl | alpha = 0.0 })
                                (model_to_color { mdl | alpha = 1.0 })
                            )
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "1"
                        , autocomplete False
                        , step "0.001"
                        , onInput ChangeAlpha
                        ]
                        []
                    ]
                , div [ id "boxes" ]
                    [ input
                        [ type_ "number"
                        , id "red"
                        , value (String.fromInt mdl.red)
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "255"
                        , autocomplete False
                        , onInput ChangeRed
                        ]
                        []
                    , input
                        [ type_ "number"
                        , id "green"
                        , value (String.fromInt mdl.green)
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "255"
                        , autocomplete False
                        , onInput ChangeGreen
                        ]
                        []
                    , input
                        [ type_ "number"
                        , id "blue"
                        , value (String.fromInt mdl.blue)
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "255"
                        , autocomplete False
                        , onInput ChangeBlue
                        ]
                        []
                    , input
                        [ type_ "number"
                        , id "alpha"
                        , value (String.fromFloat mdl.alpha)
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "1"
                        , step "0.001"
                        , autocomplete False
                        , onInput ChangeAlpha
                        ]
                        []
                    ]
                ]
            , div []
                [ p
                    [ id "white"
                    , style "color" (format_rgba (model_to_color mdl))
                    ]
                    [ text "This is the selected color against white." ]
                , p
                    [ id "black"
                    , style "color" (format_rgba (model_to_color mdl))
                    ]
                    [ text "This is the selected color against black." ]
                , select
                    [ id "format"
                    , onInput ChangeFormat
                    , value mdl.format.name
                    ]
                    (List.map option_format formats)
                , input
                    [ type_ "text"
                    , value (format_output mdl)
                    , onInput ChangeOutput
                    ]
                    []
                ]
            ]
        ]


main =
    Browser.sandbox { init = model, update = update, view = view }
