module HeroIcons exposing (..)

import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Svg as Svg exposing (path, svg)
import Svg.Attributes as SvgAttr


solidX : Html msg
solidX =
    svg
        [ SvgAttr.class "h-5 w-5"
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


outlineMenu =
    svg
        [ SvgAttr.class "h-6 w-6"
        , SvgAttr.fill "none"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.stroke "currentColor"
        , Attr.attribute "aria-hidden" "true"
        ]
        [ path
            [ SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.d "M4 6h16M4 12h16M4 18h16"
            ]
            []
        ]


outlineX =
    svg
        [ SvgAttr.class "h-6 w-6"
        , SvgAttr.fill "none"
        , SvgAttr.viewBox "0 0 24 24"
        , SvgAttr.stroke "currentColor"
        , Attr.attribute "aria-hidden" "true"
        ]
        [ path
            [ SvgAttr.strokeLinecap "round"
            , SvgAttr.strokeLinejoin "round"
            , SvgAttr.strokeWidth "2"
            , SvgAttr.d "M6 18L18 6M6 6l12 12"
            ]
            []
        ]


outlineCheckCircle =
    svg
        [ SvgAttr.class "h-6 w-6 text-green-400"
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
