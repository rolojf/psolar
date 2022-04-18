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
    | ToggleMenu


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

        ToggleMenu ->
            ( { model | showMobileMenu = not model.showMobileMenu }
            , Cmd.none
            )

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


view : Data -> { path : Path, route : Maybe Route } -> Model -> (Msg -> msg) -> View msg -> { body : Html msg, title : String }
view sharedData page model toMsg pageView =
    { body =
        div
            []
            (viewErroresAlNotificar model.errorAlNotificar
                ++ [ viewMenu
                        page.route
                        model.showMobileMenu
                        pageView.withMenu
                        toMsg
                   ]
                ++ pageView.body
            )
    , title = pageView.title
    }


viewMenu : Maybe Route -> Bool -> View.MenuInfo msg -> (Msg -> msg) -> Html msg
viewMenu ruta menuOpen wMenu toMsg =
    let
        clasesMenuItems : ( Bool, Bool ) -> Html.Attribute msg
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

        menuItem : Bool -> View.Liga -> Html msg
        menuItem esMovil laLiga =
            case laLiga.dir of
                View.Otra camino ->
                    Html.a
                        [ Attr.href <| Path.toRelative camino
                        , clasesMenuItems ( esMovil, laLiga.especial )
                        ]
                        [ text laLiga.queDice ]

                View.Interna rutaLiga ->
                    Route.link
                        rutaLiga
                        [ clasesMenuItems ( esMovil, laLiga.especial ) ]
                        [ text laLiga.queDice ]

        showMovilMenu : Bool -> Animation
        showMovilMenu show =
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

        movilMenu ligas =
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
                        [ Html.map toMsg <|
                            Html.button
                                [ Attr.type_ "button"
                                , class "bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"
                                , Event.onClick ToggleMenu
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
                    (List.map
                        (menuItem True)
                        ligas
                    )
                ]
    in
    case wMenu of
        View.NoMenu ->
            div [] []

        View.SiMenu ligas complementos ->
            div
                [ class "relative bg-white overflow-hidden" ]
                [ div
                    [ class "max-w-7xl mx-auto" ]
                    [ div
                        [ class <|
                            "relative z-10 pb-8 bg-white "
                                ++ (if ruta == Just Route.Index then
                                        "sm:pb-16 md:pb-20 xl:pb-32 lg:pb-28 lg:max-w-2xl "

                                    else
                                        ""
                                   )
                                ++ "lg:w-full"
                        ]
                        [ HeroIcons.menuSan1
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
                                                [ Html.map toMsg <|
                                                    Html.button
                                                        [ Attr.type_ "button"
                                                        , class "bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"
                                                        , Attr.attribute "aria-expanded" "false"
                                                        , Event.onClick ToggleMenu
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
                                        (List.map
                                            (menuItem False)
                                            ligas
                                        )
                                    ]
                                ]
                            , Animated.div
                                (showMovilMenu menuOpen)
                                [ class "absolute z-10 top-0 inset-x-0 p-2 transition transform origin-top-right md:hidden" ]
                                [ movilMenu ligas ]
                            ]
                        , complementos.mainHero
                        ]
                    ]
                , complementos.afterHero
                ]


viewErroresAlNotificar : Maybe Http.Error -> List (Html msg)
viewErroresAlNotificar cualError =
    case cualError of
        Nothing ->
            []

        Just error ->
            [ div [] [ text <| viewHttpError error ] ]


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
