module Main exposing (main)

import Html exposing (Html, Attribute, div, input, text, main_, label, p, select, option)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)


-- rtfeldman/hex 1.0.0

import Hex


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


to_hex_string : Int -> String
to_hex_string n =
    String.padLeft 2 '0' (Hex.toString n)


interp : Float -> Int
interp alpha =
    round (alpha * 255)


format_rgba : Color -> String
format_rgba { red, green, blue, alpha } =
    "rgba("
        ++ (toString red)
        ++ ", "
        ++ (toString green)
        ++ ", "
        ++ (toString blue)
        ++ ", "
        ++ (toString alpha)
        ++ ")"


format_source : Color -> String
format_source { red, green, blue, alpha } =
    String.join " "
        (List.map toString
            [ red, green, blue, interp alpha ]
        )


format_hex : Color -> String
format_hex { red, green, blue, alpha } =
    String.join ""
        (List.map to_hex_string
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
    { name = "source", description = "Source Engine (R G B A)", function = format_source }


formats : List OutputFormat
formats =
    [ default_format
    , { name = "rgba", description = "HTML (rgba(R, G, B, A))", function = format_rgba }
    , { name = "hex", description = "Hexadecimal (RRGGBBAA)", function = format_hex }
    ]


model : Model
model =
    { red = 0
    , green = 0
    , blue = 0
    , alpha = 1.0
    , format = default_format
    }


update : Msg -> Model -> Model
update msg mdl =
    case msg of
        ChangeRed red ->
            { mdl | red = Result.withDefault 0 (String.toInt red) }

        ChangeGreen green ->
            { mdl | green = Result.withDefault 0 (String.toInt green) }

        ChangeBlue blue ->
            { mdl | blue = Result.withDefault 0 (String.toInt blue) }

        ChangeAlpha alpha ->
            { mdl | alpha = Result.withDefault 1.0 (String.toFloat alpha) }

        ChangeFormat format ->
            { mdl | format = Maybe.withDefault default_format (string_to_format format) }


view : Model -> Html Msg
view mdl =
    main_ []
        [ div [ id "color-preview" ]
            [ div [ style [ ( "background-color", format_rgba (model_to_color mdl) ) ] ] []
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
                        , value (toString mdl.red)
                        , style
                            [ ( "background-image"
                              , (gradient
                                    (model_to_color { mdl | red = 0 })
                                    (model_to_color { mdl | red = 255 })
                                )
                              )
                            ]
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "255"
                        , autocomplete False
                        , onInput ChangeRed
                        ]
                        []
                    , input
                        [ type_ "range"
                        , class "green"
                        , value (toString mdl.green)
                        , style
                            [ ( "background-image"
                              , (gradient
                                    (model_to_color { mdl | green = 0 })
                                    (model_to_color { mdl | green = 255 })
                                )
                              )
                            ]
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "255"
                        , autocomplete False
                        , onInput ChangeGreen
                        ]
                        []
                    , input
                        [ type_ "range"
                        , class "blue"
                        , value (toString mdl.blue)
                        , style
                            [ ( "background-image"
                              , (gradient
                                    (model_to_color { mdl | blue = 0 })
                                    (model_to_color { mdl | blue = 255 })
                                )
                              )
                            ]
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "255"
                        , autocomplete False
                        , onInput ChangeBlue
                        ]
                        []
                    , input
                        [ type_ "range"
                        , class "alpha"
                        , value (toString mdl.alpha)
                        , style
                            [ ( "background-image"
                              , (gradient
                                    (model_to_color { mdl | alpha = 0.0 })
                                    (model_to_color { mdl | alpha = 1.0 })
                                )
                              )
                            ]
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
                        , value (toString mdl.red)
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "255"
                        , autocomplete False
                        , onInput ChangeRed
                        ]
                        []
                    , input
                        [ type_ "number"
                        , id "green"
                        , value (toString mdl.green)
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "255"
                        , autocomplete False
                        , onInput ChangeGreen
                        ]
                        []
                    , input
                        [ type_ "number"
                        , id "blue"
                        , value (toString model.blue)
                        , Html.Attributes.min "0"
                        , Html.Attributes.max "255"
                        , autocomplete False
                        , onInput ChangeBlue
                        ]
                        []
                    , input
                        [ type_ "number"
                        , id "alpha"
                        , value (toString mdl.alpha)
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
                [ p [ id "white", style [ ( "color", format_rgba (model_to_color mdl) ) ] ]
                    [ text "This is the selected color against white." ]
                , p [ id "black", style [ ( "color", format_rgba (model_to_color mdl) ) ] ]
                    [ text "This is the selected color against black." ]
                , select [ id "format", onInput ChangeFormat ]
                    (List.map option_format formats)
                , input
                    [ type_ "text"
                    , readonly True
                    , value (format_output mdl)
                    ]
                    []
                ]
            ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = model, view = view, update = update }
