module Shared exposing (Data, Model, Msg(..), SharedMsg(..), UsuarioSt(..), template)

import Analytics
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
import Url
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
    | NoOpS
    | AnalyticsUsoMenuLigaExterna String
    | AvisadoAnalytics (Result Http.Error ())


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


track : Msg -> Analytics.Event
track msg =
    case msg of
        OnPageChange nuevaPagina ->
            let
                queCambioReportar =
                    nuevaPagina.path
                        |> Path.toSegments
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault "index"
                        |> String.append ("cambio-pagina" ++ "-menuliga-interna-")
            in
            Analytics.eventoXReportar
                queCambioReportar

        AnalyticsUsoMenuLigaExterna queLiga ->
            Analytics.eventoXReportar
                queLiga

        _ ->
            Analytics.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnPageChange _ ->
            ( { model | showMobileMenu = False }
            , Analytics.toCmd
                (track msg)
                AvisadoAnalytics
            )

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

        NoOpS ->
            ( model, Cmd.none )

        AnalyticsUsoMenuLigaExterna _ ->
            ( model
            , Analytics.toCmd
                (track msg)
                AvisadoAnalytics
            )

        AvisadoAnalytics resulto ->
            ( case resulto of
                Err quePaso ->
                    { model | errorAlNotificar = Just quePaso }

                Ok _ ->
                    model
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


viewMenu : Maybe Route -> Bool -> View.MenuInfo -> (Msg -> msg) -> Html msg
viewMenu ruta menuOpen wMenu toMsg =
    let
        quePagina : String
        quePagina =
            ruta
                |> Maybe.map Route.routeToPath
                |> Maybe.withDefault [ "pagina-rara" ]
                |> List.foldr String.append ""

        quePaginaCompuesta : String
        quePaginaCompuesta =
            if String.isEmpty quePagina then
                "pag-index"

            else
                String.append "pag-" quePagina

        clasesMenuItems : ( Bool, Bool ) -> Html.Attribute Msg
        clasesMenuItems ( esMovil, especial ) =
            case ( esMovil, especial ) of
                ( True, True ) ->
                    class "tw block w-full px-5 py-3 text-center font-medium text-blue-900 bg-gray-50 hover:bg-gray-200"

                ( True, False ) ->
                    class "tw block px-5 py-3 rounded-md font-medium text-base text-gray-700 hover:text-gray-900 hover:bg-gray-50"

                ( False, True ) ->
                    class "tw font-medium text-blue-900 hover:text-blue-500"

                ( False, False ) ->
                    class "tw font-medium text-gray-500 hover:text-gray-900"

        menuItem : Bool -> View.Liga -> Html msg
        menuItem esMovil laLiga =
            case laLiga.dir of
                View.Otra camino ->
                    Html.a
                        [ Attr.href <| Path.toRelative camino
                        , clasesMenuItems ( esMovil, laLiga.especial )
                        , camino
                            |> Path.toSegments
                            |> List.reverse
                            |> List.head
                            |> Maybe.withDefault "-ligaexterna-rara-"
                            |> String.append (quePaginaCompuesta ++ "-menuliga-externa-")
                            |> AnalyticsUsoMenuLigaExterna
                            |> Event.onClick
                        ]
                        [ text laLiga.queDice ]
                        |> Html.map toMsg

                View.Interna rutaLiga ->
                    Route.link
                        rutaLiga
                        [ clasesMenuItems ( esMovil, laLiga.especial ) ]
                        [ text laLiga.queDice ]
                        |> Html.map toMsg

        showMovilMenu : Bool -> Animation
        showMovilMenu show =
            if show then
                Animation.fromTo
                    { duration = 180
                    , options = [ Animation.easeOut ]
                    }
                    [ P.opacity 0, P.scale 0.9 ]
                    [ P.opacity 1, P.scale 1 ]

            else
                Animation.fromTo
                    { duration = 125
                    , options = [ Animation.easeIn ]
                    }
                    [ P.opacity 1, P.scale 1 ]
                    [ P.opacity 0, P.scale 0.9 ]

        movilMenu ligas =
            div
                [ class "tw rounded-lg shadow-md bg-white ring-1 ring-black ring-opacity-5 overflow-hidden" ]
                [ div
                    [ class "tw px-5 pt-4 flex items-center justify-between" ]
                    [ div []
                        [ Html.img
                            [ class "h-8 w-auto"
                            , Attr.src <|
                                Cloudinary.url "f_auto" "v1634944374/logo-psolar2_nrh1xt.svg"
                            , Attr.alt ""
                            ]
                            []
                        ]
                    , div
                        [ class "tw -mr-2" ]
                        [ Html.map toMsg <|
                            Html.button
                                [ Attr.type_ "button"
                                , class "tw bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"
                                , Event.onClick ToggleMenu
                                ]
                                [ Html.span
                                    [ class "tw sr-only" ]
                                    [ text "Close main menu" ]
                                , HeroIcons.outlineX
                                ]
                        ]
                    ]
                , div
                    [ class "tw px-2 pt-2 pb-3 space-y-1" ]
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
                [ class "tw relative bg-white overflow-hidden" ]
                [ div
                    [ class "tw max-w-7xl mx-auto" ]
                    [ div
                        [ class <|
                            "tw relative z-10 pb-8 bg-white "
                                --sm:pb-16 md:pb-20 xl:pb-32 lg:max-w-2xl lg:w-full lg:pb-28"
                                ++ (if True then
                                        -- ruta == Just Route.Index then
                                        "tw sm:pb-16 md:pb-20 xl:pb-32 lg:max-w-2xl lg:w-full lg:pb-28 "
                                        --relative z-10 pb-8 bg-white lg:max-w-2xl lg:w-full

                                    else
                                        "tw lg:w-full"
                                   )
                        ]
                        [ -- HeroIcons.menuSan1
                          div []
                            [ div
                                [ class "tw relative pt-6 px-4 sm:px-6 lg:px-8" ]
                                [ Html.nav
                                    [ class "tw relative flex items-center justify-between sm:h-10 lg:justify-start"
                                    , Attr.attribute "aria-label" "Global"
                                    ]
                                    [ div
                                        [ class "tw flex items-center flex-grow flex-shrink-0 lg:flex-grow-0" ]
                                        [ div
                                            [ class "tw flex items-center justify-between w-full md:w-auto" ]
                                            [ Html.img
                                                [ class "tw h-8 w-auto sm:h-10"
                                                , Attr.src <|
                                                    Cloudinary.url "f_auto" "v1634944374/logo-psolar2_nrh1xt.svg"
                                                , Attr.alt "logo PSOLAR.MX"
                                                ]
                                                []
                                            , div
                                                [ class "tw -mr-2 flex items-center md:hidden" ]
                                                [ Html.map toMsg <|
                                                    Html.button
                                                        [ Attr.type_ "button"
                                                        , class "tw bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"
                                                        , Attr.attribute "aria-expanded" "false"
                                                        , Event.onClick ToggleMenu
                                                        ]
                                                        [ Html.span
                                                            [ class "tw sr-only" ]
                                                            [ text "Abrir menu principal" ]
                                                        , HeroIcons.outlineMenu
                                                        ]
                                                ]
                                            ]
                                        ]
                                    , div
                                        [ class "tw hidden md:block md:ml-10 md:pr-4 md:space-x-8" ]
                                        (List.map
                                            (menuItem False)
                                            ligas
                                        )
                                    , if menuOpen then
                                        Animated.div
                                            (showMovilMenu menuOpen)
                                            [ class "tw absolute z-10 top-0 inset-x-0 p-2 transition transform origin-top-right md:hidden" ]
                                            [ movilMenu ligas ]

                                      else
                                        div [] []
                                    ]
                                ]
                            ]
                        , complementos.mainHero
                            |> Html.map (\_ -> NoOpS)
                            |> Html.map toMsg
                        ]
                    ]
                , complementos.afterHero
                    |> Html.map (\_ -> NoOpS)
                    |> Html.map toMsg
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
