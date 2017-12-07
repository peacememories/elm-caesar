module Main exposing (main)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Element exposing (..)
import Element.Input exposing (..)
import Element.Attributes exposing (..)
import Color exposing (..)
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
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


type Styles
    = None
    | Header
    | Body
    | Page
    | TextField
    | Label
    | Background


globalSheet : StyleSheet Styles variation
globalSheet =
    styleSheet
        [ style None []
        , style Header
            [ Color.background darkCharcoal
            , Color.text white
            , Font.typeface [ Font.sansSerif ]
            , Font.size 40
            ]
        , style Page
            [ Color.background darkGray
            ]
        , style TextField
            [ Color.background darkCharcoal
            , Color.text white
            , Font.typeface [ Font.sansSerif ]
            , Font.size 20
            ]
        , style Label
            [ Font.typeface [ Font.sansSerif ]
            , Font.size 20
            , Color.text darkCharcoal
            ]
        , style Background
            [ Color.background darkCharcoal ]
        ]


view : Model -> Html Msg
view model =
    viewport globalSheet (mainLayout model)


mainLayout : Model -> Element Styles variation Msg
mainLayout model =
    column Background
        [ height fill
        , width fill
        , center
        , verticalCenter
        ]
        [ column Page
            [ height fill
            , width fill
            , maxWidth <| px 500
            , maxHeight <| px 800
            ]
            [ pageHeader
            , pageContent model
            ]
        ]


pageHeader : Element Styles variation msg
pageHeader =
    row None
        [ center
        , width fill
        , verticalCenter
        , height (px 60)
        ]
        [ Element.text "Caesar Cipher"
            |> h1 None []
        ]
        |> header Header []


pageContent : Model -> Element Styles variation Msg
pageContent model =
    column None
        [ spacing 20, height fill ]
        [ row None
            [ height <| fillPortion 1 ]
            [ multiline TextField
                [ height <| fill
                , width fill
                , padding 10
                ]
                { onChange = CiphertextChanged
                , value = model.ciphertext
                , label =
                    placeholder
                        { text = "Ciphertext"
                        , label = hiddenLabel "Ciphertext"
                        }
                , options = []
                }
            ]
        , textLayout TextField
            [ height <| fillPortion 1
            , padding 10
            , yScrollbar
            ]
            (splitToParagraphs <|
                caesarCipher
                    model.offset
                    model.ciphertext
            )
        , row None
            [ spacing 10 ]
            [ Element.text (toString model.offset)
                |> el None [ alignRight ]
                |> el Label [ minWidth <| px 30 ]
            , range None [ width fill ] OffsetChanged model.offset
            ]
        ]
        |> mainContent Body [ height fill, padding 20, center ]


range : style -> List (Element.Attribute variation msg) -> (Int -> msg) -> Int -> Element style variation msg
range style attrs tag value =
    el style attrs <|
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


splitToParagraphs : String -> List (Element Styles variation msg)
splitToParagraphs str =
    String.lines str
        |> List.map (Element.text >> List.singleton >> paragraph None [])
