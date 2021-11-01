module Reto exposing (..)

import Html.Styled as Htmls exposing (div)
import Html.Styled.Attributes as Attr exposing (class)
import Html.Styled.Events as Events
import Tailwind.Breakpoints as TwBp
import Tailwind.Utilities as Tw


type alias Model =
    { intentos : Int
    , listo : Bool
    , intento : Intentos
    , queRespondio : String
    , vaDeNuez : Bool
    }


type Intentos
    = VaPues
    | YaRespondio
    | VaDeNuevo
    | EstaFrito
    | YaOk


newModel : Model
newModel =
    { intentos = 0
    , listo = False
    , intento = VaPues
    , queRespondio = ""
    , vaDeNuez = False
    }


type Msg
    = IntentaDeNuez
    | Respondio String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Respondio conQue ->
            let
                seLaSupo =
                    if conQue == "4" then
                        True

                    else
                        False
            in
            { model
                | queRespondio = conQue
                , intento =
                    if seLaSupo then
                        YaOk

                    else
                        YaRespondio
                , vaDeNuez = not seLaSupo
            }

        IntentaDeNuez ->
                { model
                    | queRespondio = ""
                    , intento = if model.intentos >= 3 then EstaFrito else VaDeNuevo
                    , vaDeNuez = False
                    , intentos = model.intentos + 1
                }



view : Model -> Htmls.Html Msg
view model =
    div
        []
        [ viewChallenge model ]


viewChallenge : Model -> Htmls.Html Msg
viewChallenge model =
    div
        [ class "la-base-modal" ]
        [ div
            [ Attr.css <|
                [ Tw.bg_green_100
                , Tw.shadow
                , Tw.rounded_lg
                , Tw.mx_auto
                , Tw.mt_24
                , Tw.w_10over12
                , Tw.h_64
                , TwBp.md [ Tw.max_w_md, Tw.mx_auto, Tw.mt_48 ]
                ]
                    ++ (if model.intento == YaRespondio then
                            [ Tw.animate_bounce ]

                        else
                            []
                       )
            ]
            [ Htmls.h3
                [ Attr.css
                    [ Tw.pt_4
                    , Tw.ml_3
                    , Tw.text_xl
                    , Tw.leading_6
                    , Tw.font_medium
                    , Tw.text_gray_900
                    , TwBp.md [ Tw.ml_6 ]
                    ]
                ]
                [ Htmls.text "Validación Rápida" ]
            , Htmls.p
                [ Attr.css
                    [ Tw.mt_2
                    , Tw.mx_6
                    , Tw.text_base
                    , Tw.leading_5
                    , Tw.text_gray_500
                    ]
                ]
                [ Htmls.text "Contesta lo siguiente para validar que eres humano y no un bot" ]
            , div
                [ Attr.css
                    [ Tw.w_4over5
                    , Tw.bg_yellow_100
                    , Tw.mt_6
                    , Tw.mx_auto
                    , Tw.h_32
                    ]
                ]
                [ Htmls.p
                    [ Attr.css
                        [ Tw.pt_5
                        , Tw.pl_12
                        , Tw.text_base
                        , Tw.font_medium
                        , Tw.text_gray_700
                        ]
                    ]
                    [ Htmls.text "Resuleve la siguiente ecuación: " ]
                , div
                    [ Attr.css
                        [ Tw.ml_6
                        , Tw.mt_4
                        , Tw.flex
                        , Tw.flex_row
                        , Tw.items_center
                        , Tw.content_center
                        , Tw.justify_center
                        , Tw.text_base
                        ]
                    ]
                    [ Htmls.p
                        []
                        [ Htmls.text "7 + " ]
                    , Htmls.label
                        [ Attr.css [ Tw.sr_only ]
                        , Attr.for "valor"
                        ]
                        [ Htmls.text "número" ]
                    , Htmls.input
                        [ Attr.css
                            [ Tw.text_center
                            , Tw.mx_2
                            , Tw.w_5
                            , Tw.rounded_md
                            , Tw.shadow_sm
                            , TwBp.sm [ Tw.leading_5, Tw.text_sm ]
                            ]

                        -- Tw.block, Tw.w_full del .apparel-campo
                        , Attr.id "valor-challenge"
                        , Attr.autofocus True
                        , case model.intento of
                            VaPues ->
                                Attr.placeholder "?"

                            YaRespondio ->
                                Attr.value model.queRespondio

                            VaDeNuevo ->
                                Attr.value model.queRespondio

                            YaOk ->
                                Attr.css [ Tw.animate_ping ]

                            EstaFrito ->
                                class "no-se-que-decirle"
                        , Events.onInput Respondio
                        ]
                        []
                    , Htmls.p
                        []
                        [ Htmls.text "= 11" ]
                    ]
                , if model.intentos >= 1 then
                    Htmls.p
                        [ Attr.css
                            ([ Tw.text_right, Tw.pt_4, Tw.mx_4 ]
                                ++ (if model.intentos == 1 then
                                        [ Tw.text_black ]

                                    else if model.intentos == 2 then
                                        [ Tw.text_red_500 ]

                                    else
                                        [ Tw.text_red_500, Tw.font_bold, Tw.italic ]
                                   )
                            )
                        ]
                        [ Htmls.text "Intenta de nuevo!" ]

                  else
                    Htmls.p [] []
                ]
            ]
        ]
