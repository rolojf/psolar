module Page.Index exposing (Data, Model, Msg, page)

import Cloudinary
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import Html as Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Styled as Htmls
import Html.Styled.Attributes as AttrS
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Shared
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import View exposing (View)


type alias Model =
    ()


type alias Msg =
    Never


type alias RouteParams =
    {}


page : Page RouteParams Data
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildNoState { view = view }


data : DataSource Data
data =
    DataSource.succeed ()


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "elm-pages"
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = "TODO"
        , locale = Nothing
        , title = "TODO title" -- metadata.title -- TODO
        }
        |> Seo.website


type alias Data =
    ()


view :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel static =
    { title = "Por ahora nada"
    , body =
        [ viewHero
        ]
    }


viewHero =
    div
        [ class "relative bg-white overflow-hidden" ]
        [ div
            [ class "max-w-7xl mx-auto" ]
            [ div
                [ class "relative z-10 pb-8 bg-white sm:pb-16 md:pb-20 lg:max-w-2xl lg:w-full lg:pb-28 xl:pb-32" ]
                [ svg
                    [ SvgAttr.class "hidden lg:block absolute right-0 inset-y-0 h-full w-48 text-white transform translate-x-1/2"
                    , SvgAttr.fill "currentColor"
                    , SvgAttr.viewBox "0 0 100 100"
                    , SvgAttr.preserveAspectRatio "none"
                    , Attr.attribute "aria-hidden" "true"
                    ]
                    [ Svg.polygon
                        [ SvgAttr.points "50,0 100,0 50,100 0,100" ]
                        []
                    ]
                , div []
                    [ div
                        [ class "relative pt-6 px-4 sm:px-6 lg:px-8" ]
                        [ Html.nav
                            [ class "relative flex items-center justify-between sm:h-10 lg:justify-start"
                            , Attr.attribute "aria-label" "Global"
                            ]
                            [ div
                                [ class "flex items-center flex-grow flex-shrink-0 lg:flex-grow-0" ]
                                [ div
                                    [ class "flex items-center justify-between w-full md:w-auto" ]
                                    [ Html.a
                                        [ Attr.href "#" ]
                                        [ Html.span
                                            [ class "sr-only" ]
                                            [ text "Workflow" ]
                                        , Html.img
                                            [ class "h-8 w-auto sm:h-10"
                                            , Attr.src "https://tailwindui.com/img/logos/workflow-mark-indigo-600.svg"
                                            ]
                                            []
                                        ]
                                    , div
                                        [ class "-mr-2 flex items-center md:hidden" ]
                                        [ Html.button
                                            [ Attr.type_ "button"
                                            , class "bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"
                                            , Attr.attribute "aria-expanded" "false"
                                            ]
                                            [ Html.span
                                                [ class "sr-only" ]
                                                [ text "Open main menu" ]
                                            , {- Heroicon name: outline/menu -}
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
                                            ]
                                        ]
                                    ]
                                ]
                            , div
                                [ class "hidden md:block md:ml-10 md:pr-4 md:space-x-8" ]
                                [ Html.a
                                    [ Attr.href "#"
                                    , class "font-medium text-gray-500 hover:text-gray-900"
                                    ]
                                    [ text "Product" ]
                                , Html.a
                                    [ Attr.href "#"
                                    , class "font-medium text-gray-500 hover:text-gray-900"
                                    ]
                                    [ text "Features" ]
                                , Html.a
                                    [ Attr.href "#"
                                    , class "font-medium text-gray-500 hover:text-gray-900"
                                    ]
                                    [ text "Marketplace" ]
                                , Html.a
                                    [ Attr.href "#"
                                    , class "font-medium text-gray-500 hover:text-gray-900"
                                    ]
                                    [ text "Company" ]
                                , Html.a
                                    [ Attr.href "#"
                                    , class "font-medium text-indigo-600 hover:text-indigo-500"
                                    ]
                                    [ text "Log in" ]
                                ]
                            ]
                        ]
                    , {-
                         Mobile menu, show/hide based on menu open state.

                         Entering: "duration-150 ease-out"
                           From: "opacity-0 scale-95"
                           To: "opacity-100 scale-100"
                         Leaving: "duration-100 ease-in"
                           From: "opacity-100 scale-100"
                           To: "opacity-0 scale-95"
                      -}
                      div
                        [ class "absolute z-10 top-0 inset-x-0 p-2 transition transform origin-top-right md:hidden" ]
                        [ div
                            [ class "rounded-lg shadow-md bg-white ring-1 ring-black ring-opacity-5 overflow-hidden" ]
                            [ div
                                [ class "px-5 pt-4 flex items-center justify-between" ]
                                [ div []
                                    [ Html.img
                                        [ class "h-8 w-auto"
                                        , Attr.src "https://tailwindui.com/img/logos/workflow-mark-indigo-600.svg"
                                        , Attr.alt ""
                                        ]
                                        []
                                    ]
                                , div
                                    [ class "-mr-2" ]
                                    [ Html.button
                                        [ Attr.type_ "button"
                                        , class "bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"
                                        ]
                                        [ Html.span
                                            [ class "sr-only" ]
                                            [ text "Close main menu" ]
                                        , {- Heroicon name: outline/x -}
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
                                        ]
                                    ]
                                ]
                            , div
                                [ class "px-2 pt-2 pb-3 space-y-1" ]
                                [ Html.a
                                    [ Attr.href "#"
                                    , class "block px-3 py-2 rounded-md text-base font-medium text-gray-700 hover:text-gray-900 hover:bg-gray-50"
                                    ]
                                    [ text "Product" ]
                                , Html.a
                                    [ Attr.href "#"
                                    , class "block px-3 py-2 rounded-md text-base font-medium text-gray-700 hover:text-gray-900 hover:bg-gray-50"
                                    ]
                                    [ text "Features" ]
                                , Html.a
                                    [ Attr.href "#"
                                    , class "block px-3 py-2 rounded-md text-base font-medium text-gray-700 hover:text-gray-900 hover:bg-gray-50"
                                    ]
                                    [ text "Marketplace" ]
                                , Html.a
                                    [ Attr.href "#"
                                    , class "block px-3 py-2 rounded-md text-base font-medium text-gray-700 hover:text-gray-900 hover:bg-gray-50"
                                    ]
                                    [ text "Company" ]
                                ]
                            , Html.a
                                [ Attr.href "#"
                                , class "block w-full px-5 py-3 text-center font-medium text-indigo-600 bg-gray-50 hover:bg-gray-100"
                                ]
                                [ text "Log in" ]
                            ]
                        ]
                    ]
                , Html.main_
                    [ class "mt-10 mx-auto max-w-7xl px-4 sm:mt-12 sm:px-6 md:mt-16 lg:mt-20 lg:px-8 xl:mt-28" ]
                    [ div
                        [ class "sm:text-center lg:text-left"
                        ]
                        [ Html.h1
                            [ class "text-4xl tracking-tight font-extrabold text-gray-900 sm:text-5xl md:text-6xl" ]
                            [ Html.span
                                [ class "block xl:inline" ]
                                [ text "Cuidamos tu " ]
                            , Html.span
                                [ class "block text-blue-900 xl:inline" ]
                                [ text " Panel Solar " ]
                            , Html.span
                                [ class "block xl:inline" ]
                                [ text " para que ahorres más" ]
                            ]
                        , Html.p
                            [ class "mt-3 text-base text-gray-500 sm:mt-5 sm:text-lg sm:max-w-xl sm:mx-auto md:mt-5 md:text-xl lg:mx-0" ]
                            [ text "Con revisiones para ver que todo está bien, si no hay algo que afecte. Y mantenimiento para prevenir y corregir.  Alarga la vida y produce más." ]
                        , div
                            [ class "mt-5 sm:mt-8 sm:flex sm:justify-center lg:justify-start" ]
                            [ div
                                [ class "rounded-md shadow" ]
                                [ Html.a
                                    [ Attr.href "#"
                                    , class "w-full flex items-center justify-center px-8 py-3 border border-transparent text-base font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 md:py-4 md:text-lg md:px-10"
                                    ]
                                    [ text "Get started" ]
                                ]
                            , div
                                [ class "mt-3 sm:mt-0 sm:ml-3" ]
                                [ Html.a
                                    [ Attr.href "#"
                                    , class "w-full flex items-center justify-center px-8 py-3 border border-transparent text-base font-medium rounded-md text-indigo-700 bg-indigo-100 hover:bg-indigo-200 md:py-4 md:text-lg md:px-10"
                                    ]
                                    [ text "Live demo" ]
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        , div
            [ class "lg:absolute lg:inset-y-0 lg:right-0 lg:w-1/2" ]
            [ Html.img
                [ class "h-56 w-full object-cover object-top sm:h-72 md:h-96 lg:w-full lg:h-full lg:object-right"
                , Attr.src ( Cloudinary.url "f_auto,q_auto:best" "dreamstime_s_30697263_clymr0.jpg")
                , Attr.alt ""
                ]
                []
            ]
        ]
