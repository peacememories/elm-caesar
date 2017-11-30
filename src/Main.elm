module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Element exposing (..)
import Element.Input exposing (..)
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
        , range () OffsetChanged model.offset
        ]


range : style -> (Int -> msg) -> Int -> Element style variation msg
range style tag value =
    html <|
        Html.input [ Html.Attributes.type_ "range" ] []



--
-- UTILS
--


caesarCipher : Int -> String -> String
caesarCipher offset text =
    let
        cipher char =
            Char.toCode char
                |> (+) offset
                |> Char.fromCode
    in
        String.toList text
            |> List.map cipher
            |> String.fromList
