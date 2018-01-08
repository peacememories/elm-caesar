module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Window
import Element exposing (..)
import Element.Input as Input
import Element.Background as Background
import Element.Font as Font
import Color exposing (..)
import String
import Char


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }



--
-- MODEL
--


type Msg
    = CiphertextChanged String
    | OffsetChanged Int
    | DoNothing
    | WindowSizeChanged Window.Size


type alias Model =
    { ciphertext : String
    , offset : Int
    , windowSize : Window.Size
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        OffsetChanged offset ->
            { model
                | offset = offset
            }

        CiphertextChanged ciphertext ->
            { model
                | ciphertext = ciphertext
            }

        WindowSizeChanged windowSize ->
            { model
                | windowSize = windowSize
            }

        DoNothing ->
            model


initModel : Model
initModel =
    { ciphertext = ""
    , offset = 0
    , windowSize =
        { width = 0
        , height = 0
        }
    }



--
-- VIEW
--


view : Model -> Html Msg
view model =
    layout [] <|
        -- Page Background
        el
            [ Background.color darkCharcoal
            , width fill
            , height fill
            ]
        <|
            -- Main Layout
            column
                [ width (px 400)
                , height (px 600)
                ]
                [ el
                    [ Font.size 32
                    , Font.center
                    , padding 5
                    , Font.color darkCharcoal
                    , Background.color darkGray
                    , width fill
                    ]
                    (text "Caesar Cipher")
                , Input.multiline
                    [ Background.color darkCharcoal
                    , Font.color darkGray
                    ]
                    { onChange = Just CiphertextChanged
                    , text = model.ciphertext
                    , placeholder =
                        Just
                            (Input.placeholder
                                [ Font.color lightCharcoal
                                ]
                             <|
                                text "Ciphertext"
                            )
                    , label = Input.labelBelow [ hidden True ] <| text "Ciphertexts"
                    , notice = Nothing
                    }
                ]


range : List (Attribute msg) -> (Int -> msg) -> Int -> Element msg
range attrs tag value =
    el attrs <|
        html <|
            Html.input
                [ Html.Attributes.type_ "range"
                , Html.Attributes.value (toString value)
                , Html.Attributes.style
                    [ ( "box-sizing", "border-box" )
                    , ( "width", "100%" )
                    , ( "height", "100%" )
                    , ( "margin", "0" )
                    , ( "outline", "none" )
                    , ( "border-color", "rgba(155,203,255,1.0)" )
                    , ( "pointer-events", "auto" )
                    ]
                , Html.Events.onInput
                    (\text ->
                        String.toInt text
                            |> Result.withDefault value
                            |> tag
                    )
                , Html.Attributes.attribute "min" "0"
                , Html.Attributes.attribute "max" "25"
                ]
                []


paragraphs : List (Attribute msg) -> String -> Element msg
paragraphs attrs str =
    String.lines str
        |> List.map (Element.text >> List.singleton >> paragraph [])
        |> textColumn attrs



--
-- UTILS
--


encrypt : Int -> Int -> Int -> Int
encrypt offset base value =
    (value + offset - base) % 26 + base


encryptChar : Int -> Char -> Char
encryptChar offset char =
    case ( Char.isUpper char, Char.isLower char ) of
        ( True, _ ) ->
            Char.toCode char
                |> encrypt offset (Char.toCode 'A')
                |> Char.fromCode

        ( _, True ) ->
            Char.toCode char
                |> encrypt offset (Char.toCode 'a')
                |> Char.fromCode

        _ ->
            char


caesarCipher : Int -> String -> String
caesarCipher offset text =
    String.toList text
        |> List.map (encryptChar offset)
        |> String.fromList
