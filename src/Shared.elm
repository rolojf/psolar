module Shared exposing (Data, Model, Msg(..), SharedMsg(..), UsuarioSt(..), template)

import Browser.Navigation
import Cloudinary
import DataSource
import HeroIcons
import Html as Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Events as Event
import Http
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import View exposing (View)


template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Just OnPageChange
    }


type Msg
    = OnPageChange
        { path : Path
        , query : Maybe String
        , fragment : Maybe String
        }
    | SharedMsg SharedMsg


type UsuarioSt
    = Desconocido
    | Rechazado
    | Conocido (Result Http.Error String)


type alias Data =
    ()


type SharedMsg
    = NoOp
    | CambiaStatus UsuarioSt
    | ErrorAlNotificar Http.Error


type alias Model =
    { showMobileMenu : Bool
    , usuarioStatus : UsuarioSt
    , errorAlNotificar : Maybe Http.Error
    }


init :
    Maybe Browser.Navigation.Key
    -> Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : Path
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Cmd Msg )
init navigationKey flags maybePagePath =
    ( { showMobileMenu = False
      , usuarioStatus = Desconocido
      , errorAlNotificar = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPageChange _ ->
            ( { model | showMobileMenu = False }, Cmd.none )

        SharedMsg mensajePasado ->
            case mensajePasado of
                CambiaStatus nuevoSt ->
                    ( { model | usuarioStatus = nuevoSt }
                    , Cmd.none
                    )

                NoOp ->
                    ( model, Cmd.none )

                ErrorAlNotificar cualError ->
                    ( { model | errorAlNotificar = Just cualError }
                    , Cmd.none
                    )


subscriptions : Path -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


data : DataSource.DataSource Data
data =
    DataSource.succeed ()


view :
    Data
    ->
        { path : Path
        , route : Maybe Route
        }
    -> Model
    -> (Msg -> msg)
    -> View msg
    -> { body : Html msg, title : String }
view sharedData page model toMsg pageView =
    { body =
        div
            []
            ((case model.errorAlNotificar of
                Nothing ->
                    []

                Just error ->
                    [ div [] [ text <| viewHttpError error ] ]
             )
                ++ pageView.body
            )
    , title = pageView.title
    }


type LigaTipo
    = Otra Path
    | Interna Route


viewHero menuOpen =
    let
        direccionEspecial : { texto : String, dir : LigaTipo }
        direccionEspecial =
            { texto = "Comunícate"
            , dir = Interna Route.Contacto
            }

        direcciones : List { texto : String, dir : LigaTipo }
        direcciones =
            [ { texto = "Más Información"
              , dir =
                    "#features"
                        |> Path.fromString
                        |> Otra
              }
            ]

        clasesMenuItems : ( Bool, Bool ) -> Html.Attribute Msg
        clasesMenuItems ( esMovil, especial ) =
            case ( esMovil, especial ) of
                ( True, True ) ->
                    class "block w-full px-5 py-3 text-center font-medium text-blue-900 bg-gray-50 hover:bg-gray-200"

                ( True, False ) ->
                    class "block px-5 py-3 rounded-md font-medium text-base text-gray-700 hover:text-gray-900 hover:bg-gray-50"

                ( False, True ) ->
                    class "font-medium text-blue-900 hover:text-blue-500"

                ( False, False ) ->
                    class "font-medium text-gray-500 hover:text-gray-900"

        menuItem : ( Bool, Bool ) -> { texto : String, dir : LigaTipo } -> Html Msg
        menuItem tipClases dirToLink =
            case dirToLink.dir of
                Otra camino ->
                    Html.a
                        [ Attr.href <| Path.toRelative camino
                        , clasesMenuItems tipClases
                        ]
                        [ text dirToLink.texto ]

                Interna rutaLiga ->
                    Route.link
                        rutaLiga
                        [ clasesMenuItems tipClases ]
                        [ text dirToLink.texto ]

        menuAppear : Bool -> Animation
        menuAppear show =
            if show then
                Animation.fromTo
                    { duration = 180
                    , options = [ Animation.easeOut ]
                    }
                    [ P.opacity 0, P.scale 0.92 ]
                    [ P.opacity 1, P.scale 1 ]

            else
                Animation.fromTo
                    { duration = 125
                    , options = [ Animation.easeIn ]
                    }
                    [ P.opacity 1, P.scale 1 ]
                    [ P.opacity 0, P.scale 0.92 ]

        movilMenu =
            div
                [ class "rounded-lg shadow-md bg-white ring-1 ring-black ring-opacity-5 overflow-hidden" ]
                [ div
                    [ class "px-5 pt-4 flex items-center justify-between" ]
                    [ div []
                        [ Html.img
                            [ class "h-8 w-auto"
                            , Attr.src <| Cloudinary.url "f_auto" "v1634944374/logo-psolar2_nrh1xt.svg"
                            , Attr.alt ""
                            ]
                            []
                        ]
                    , div
                        [ class "-mr-2" ]
                        [ Html.button
                            [ Attr.type_ "button"
                            , class "bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"

                            -- , Event.onClick ToggleMenu
                            ]
                            [ Html.span
                                [ class "sr-only" ]
                                [ text "Close main menu" ]
                            , HeroIcons.outlineX
                            ]
                        ]
                    ]
                , div
                    [ class "px-2 pt-2 pb-3 space-y-1" ]
                    (List.append
                        (List.map
                            (menuItem
                                ( True, False )
                            )
                            direcciones
                        )
                        (List.singleton <|
                            menuItem
                                ( True, True )
                                direccionEspecial
                        )
                    )
                ]
    in
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
                                            , Attr.src <| Cloudinary.url "f_auto" "v1634944374/logo-psolar2_nrh1xt.svg"
                                            ]
                                            []
                                        ]
                                    , div
                                        [ class "-mr-2 flex items-center md:hidden" ]
                                        [ Html.button
                                            [ Attr.type_ "button"
                                            , class "bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"
                                            , Attr.attribute "aria-expanded" "false"

                                            --, Event.onClick ToggleMenu
                                            ]
                                            [ Html.span
                                                [ class "sr-only" ]
                                                [ text "Open main menu" ]
                                            , HeroIcons.outlineMenu
                                            ]
                                        ]
                                    ]
                                ]
                            , div
                                [ class "hidden md:block md:ml-10 md:pr-4 md:space-x-8" ]
                                (List.append
                                    (List.map
                                        (menuItem ( False, False ))
                                        direcciones
                                    )
                                    (List.singleton <|
                                        menuItem
                                            ( False, True )
                                            direccionEspecial
                                    )
                                )
                            ]
                        ]
                    , Animated.div
                        (menuAppear menuOpen)
                        [ class "absolute z-10 top-0 inset-x-0 p-2 transition transform origin-top-right md:hidden" ]
                        [ movilMenu ]
                    ]
                ]
            ]
        ]


viewHttpError : Http.Error -> String
viewHttpError error =
    case error of
        Http.BadUrl texto ->
            "Bad Url " ++ texto ++ " al reportar evento."

        Http.Timeout ->
            "Se tardo en reportar evento."

        Http.NetworkError ->
            "Falla de red al reportar evento."

        Http.BadStatus cual ->
            "Status que regreso " ++ String.fromInt cual ++ " al reportar evento."

        Http.BadBody texto ->
            "Mensaje mal compuesto al reportar evento. " ++ texto
