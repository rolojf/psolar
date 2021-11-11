module Footer exposing (..)

import Css
import Css.Global
import HeroIcons
import Html exposing (Html)
import Html.Styled as Htmls exposing (div)
import Html.Styled.Attributes as Attr
import Html.Styled.Attributes.Aria as Aria
import Html.Styled.Events as Events
import Svg.Styled as Svg exposing (path, svg)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as TwBp
import Tailwind.Utilities as Tw


ligaAlPie : String -> String -> Htmls.Html msg
ligaAlPie liga texto =
    div
        [ Attr.css [ Tw.px_5, Tw.py_2 ] ]
        [ Htmls.a
            [ Attr.href liga
            , Attr.css
                [ Tw.text_base
                , Tw.text_gray_600
                , Css.hover [ Tw.text_black ]
                ]
            ]
            [ Htmls.text texto ]
        ]


ligaIcono : String -> String -> SocialIcons -> Htmls.Html msg
ligaIcono direccion srCual iconoSocial =
    Htmls.a
        [ Attr.href direccion
        , Attr.css [ Tw.text_gray_400, Css.hover [ Tw.text_gray_500 ] ]
        ]
        [ Htmls.span
            [ Attr.css [ Tw.sr_only ] ]
            [ Htmls.text srCual ]
        , case iconoSocial of
            Facebook ->
                HeroIcons.svgFacebook |> Htmls.fromUnstyled

            Instagram ->
                HeroIcons.svgInstagram |> Htmls.fromUnstyled

            Twitter ->
                HeroIcons.svgTwitter |> Htmls.fromUnstyled

            Github ->
                HeroIcons.svgGithub |> Htmls.fromUnstyled
        ]


viewFooter : List (Htmls.Html msg) -> List (Htmls.Html msg) -> Html msg
viewFooter ligasNav icons2show =
    Htmls.footer
        []
        [ div
            []
            [ div
                [ Attr.css [ Tw.bg_white, Tw.py_4 ] ]
                []
            , div
                [ Attr.css [ Tw.bg_gray_200 ] ]
                [ div
                    [ Attr.css
                        [ Tw.max_w_7xl
                        , Tw.mx_auto
                        , Tw.py_12
                        , Tw.px_4
                        , Tw.overflow_hidden
                        , TwBp.lg [ Tw.px_8 ]
                        , TwBp.sm [ Tw.px_6 ]
                        ]
                    ]
                    [ Htmls.nav
                        [ Attr.css
                            [ Tw.neg_mx_5
                            , Tw.neg_my_2
                            , Tw.flex
                            , Tw.flex_wrap
                            , Tw.justify_center
                            ]
                        , Aria.ariaLabel "Footer"
                        ]
                        ligasNav
                    , div
                        [ Attr.css
                            [ Tw.mt_8
                            , Tw.flex
                            , Tw.justify_center
                            , Tw.space_x_6
                            ]
                        ]
                        icons2show
                    , Htmls.p
                        [ Attr.css
                            [ Tw.mt_8
                            , Tw.text_center
                            , Tw.text_base
                            , Tw.text_gray_500
                            ]
                        ]
                        [ Htmls.text "&copy; 2020 Workflow, Inc. All rights reserved." ]
                    ]
                ]
            ]
        ]
        |> Htmls.toUnstyled


type SocialIcons
    = Facebook
    | Instagram
    | Twitter
    | Github
