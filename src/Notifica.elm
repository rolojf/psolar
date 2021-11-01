module Notifica exposing (..)

import Browser.Dom as Dom
import Browser.Navigation
import Cloudinary
import HeroIcons
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Events as Events
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path
import Process
import Reto
import Shared
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Svg as Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import Task
import View exposing (View)


type Msg
    = CierraPues


notifAppear : Bool -> Animation
notifAppear show =
    if show then
        Animation.fromTo
            { duration = 300
            , options = [ Animation.easeOut ]
            }
            [ P.opacity 0, P.scale 0.92 ]
            [ P.opacity 1, P.scale 1 ]

    else
        Animation.fromTo
            { duration = 125
            , options = [ Animation.easeIn ]
            }
            [ P.opacity 1, P.scale 1, P.y 0.8 ]
            [ P.opacity 0, P.scale 0.92, P.y 0 ]



retroFinal : Html Msg -> String -> String -> Bool -> Html Msg
retroFinal icono titulo subtitulo aparece =
    {- This example requires Tailwind CSS v2.0+ -}
    {- Global notification live region, render this permanently at the end of the document -}
    Animated.div
        (notifAppear aparece)
        [ Attr.attribute "aria-live" "assertive"
        , class "fixed inset-0 flex items-end px-4 py-6 z-20 pointer-events-none sm:p-6 lg:items-center"
        ]
        [ div [ class "w-full flex flex-col items-center space-y-4z sm:items-start lg:items-end" ]
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
                [ class "max-w-sm w-full bg-gray-200 shadow-lg rounded-lg pointer-events-auto ring-1 ring-black ring-opacity-5 overflow-hidden" ]
                [ div
                    [ class "p-4" ]
                    [ div
                        [ class "flex items-start" ]
                        [ div
                            [ class "flex-shrink-0" ]
                            [ icono ]
                        , div
                            [ class "ml-3 w-0 flex-1 pt-0.5" ]
                            [ Html.p
                                [ class "text-sm font-medium text-gray-900" ]
                                [ text titulo ]
                            , Html.p
                                [ class "mt-1 text-sm text-gray-500" ]
                                [ text subtitulo ]
                            ]
                        , div
                            [ class "ml-4 flex-shrink-0 flex" ]
                            [ Html.button
                                [ class "bg-white rounded-md inline-flex text-gray-400 hover:text-gray-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                                , Events.onClick CierraPues
                                ]
                                [ Html.span
                                    [ class "sr-only" ]
                                    [ text "Close" ]
                                , HeroIcons.solidX
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
