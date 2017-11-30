module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Element exposing (..)
import Element.Input exposing (..)
import Element.Attributes exposing (..)
import Style exposing (StyleSheet)
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


type alias Model =
    { ciphertext : String
    , offset : Int
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

        DoNothing ->
            model


initModel : Model
initModel =
    { ciphertext = ""
    , offset = 0
    }



--
-- VIEW
--


styleSheet : StyleSheet elem variation
styleSheet =
    Style.styleSheet []


view : Model -> Html Msg
view model =
    layout styleSheet (mainLayout model)


mainLayout : Model -> Element () variation Msg
mainLayout model =
    column ()
        []
        [ multiline ()
            []
            { onChange = CiphertextChanged
            , value = ""
            , label = labelAbove <| Element.text "Ciphertext"
            , options = []
            }
        , Element.text (caesarCipher model.offset model.ciphertext)
        , row () [] [ range () OffsetChanged model.offset ]
        ]
        |> el () [ center, minWidth (percent 300) ]


range : style -> (Int -> msg) -> Int -> Element style variation msg
range style tag value =
    html <|
        Html.input
            [ Html.Attributes.type_ "range"
            , Html.Attributes.value (toString value)
            , Html.Events.onInput
                (\text ->
                    String.toInt text
                        |> Result.withDefault value
                        |> tag
                )
            , Html.Attributes.attribute "min" "0"
            , Html.Attributes.attribute "max" "26"
            ]
            []



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
