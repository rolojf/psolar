module Notifica exposing (..)

import Browser.Dom as Dom
import Browser.Navigation
import Cloudinary
import Css
import Html exposing (Html)
import Html.Styled as Htmls exposing (div, text)
import Html.Styled.Attributes as Attr exposing (class, css)
import Html.Styled.Attributes.Aria as Aria
import Html.Styled.Events as Events
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path
import Process
import Reto
import Shared
import Svg.Styled as Svg exposing (path, svg)
import Svg.Styled.Attributes as SvgAttr
import Tailwind.Breakpoints as TwBp
import Tailwind.Utilities as Tw
import Task
import View exposing (View)


retroFinal : Htmls.Html msg
retroFinal =
    {- This example requires Tailwind CSS v2.0+ -}
    {- Global notification live region, render this permanently at the end of the document -}
    div
        [ Attr.attribute "aria-live" "assertive"
        , css
            [ Tw.fixed
            , Tw.inset_0
            , Tw.flex
            , Tw.items_end
            , Tw.px_4
            , Tw.py_6
            , Tw.pointer_events_none
            , TwBp.sm
                [ Tw.p_6
                , Tw.items_start
                ]
            ]
        ]
        [ div
            [ css
                [ Tw.w_full
                , Tw.flex
                , Tw.flex_col
                , Tw.items_center
                , Tw.space_y_4
                , TwBp.sm
                    [ Tw.items_end
                    ]
                ]
            ]
            [ {-
                 Notification panel, dynamically insert this into the live region when it needs to be displayed

                 Entering: "transform ease-out duration-300 transition"
                   From: "translate-y-2 opacity-0 sm:translate-y-0 sm:translate-x-2"
                   To: "translate-y-0 opacity-100 sm:translate-x-0"
                 Leaving: "transition ease-in duration-100"
                   From: "opacity-100"
                   To: "opacity-0"
              -}
              div
                [ css
                    [ Tw.max_w_sm
                    , Tw.w_full
                    , Tw.bg_white
                    , Tw.shadow_lg
                    , Tw.rounded_lg
                    , Tw.pointer_events_auto
                    , Tw.ring_1
                    , Tw.ring_black
                    , Tw.ring_opacity_5
                    , Tw.overflow_hidden
                    ]
                ]
                [ div
                    [ css [ Tw.p_4 ] ]
                    [ div
                        [ css
                            [ Tw.flex
                            , Tw.items_start
                            ]
                        ]
                        [ div
                            [ css [ Tw.flex_shrink_0 ] ]
                            [ {- Heroicon name: outline/check-circle -}
                              svg
                                [ SvgAttr.css
                                    [ Tw.h_6
                                    , Tw.w_6
                                    , Tw.text_green_400
                                    ]
                                , SvgAttr.fill "none"
                                , SvgAttr.viewBox "0 0 24 24"
                                , SvgAttr.stroke "currentColor"
                                , Attr.attribute "aria-hidden" "true"
                                ]
                                [ path
                                    [ SvgAttr.strokeLinecap "round"
                                    , SvgAttr.strokeLinejoin "round"
                                    , SvgAttr.strokeWidth "2"
                                    , SvgAttr.d "M9 12l2 2 4-4m6 2a9 9 0 11-18 0 9 9 0 0118 0z"
                                    ]
                                    []
                                ]
                            ]
                        , div
                            [ css
                                [ Tw.ml_3
                                , Tw.w_0
                                , Tw.flex_1
                                , Tw.pt_0_dot_5
                                ]
                            ]
                            [ Htmls.p
                                [ css
                                    [ Tw.text_sm
                                    , Tw.font_medium
                                    , Tw.text_gray_900
                                    ]
                                ]
                                [ text "Successfully saved!" ]
                            , Htmls.p
                                [ css
                                    [ Tw.mt_1
                                    , Tw.text_sm
                                    , Tw.text_gray_500
                                    ]
                                ]
                                [ text "Anyone with a link can now view this file." ]
                            ]
                        , div
                            [ css
                                [ Tw.ml_4
                                , Tw.flex_shrink_0
                                , Tw.flex
                                ]
                            ]
                            [ Htmls.button
                                [ css
                                    [ Tw.bg_white
                                    , Tw.rounded_md
                                    , Tw.inline_flex
                                    , Tw.text_gray_400
                                    , Css.focus
                                        [ Tw.outline_none
                                        , Tw.ring_2
                                        , Tw.ring_offset_2
                                        , Tw.ring_indigo_500
                                        ]
                                    , Css.hover
                                        [ Tw.text_gray_500
                                        ]
                                    ]
                                ]
                                [ Htmls.span
                                    [ css [ Tw.sr_only ] ]
                                    [ text "Close" ]
                                , {- Heroicon name: solid/x -}
                                  svg
                                    [ SvgAttr.css
                                        [ Tw.h_5
                                        , Tw.w_5
                                        ]
                                    , SvgAttr.viewBox "0 0 20 20"
                                    , SvgAttr.fill "currentColor"
                                    , Attr.attribute "aria-hidden" "true"
                                    ]
                                    [ path
                                        [ SvgAttr.fillRule "evenodd"
                                        , SvgAttr.d "M4.293 4.293a1 1 0 011.414 0L10 8.586l4.293-4.293a1 1 0 111.414 1.414L11.414 10l4.293 4.293a1 1 0 01-1.414 1.414L10 11.414l-4.293 4.293a1 1 0 01-1.414-1.414L8.586 10 4.293 5.707a1 1 0 010-1.414z"
                                        , SvgAttr.clipRule "evenodd"
                                        ]
                                        []
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
