module Page.Index exposing (Data, Model, Msg, page)

import Array exposing (Array)
import Browser.Dom as Dom
import Browser.Navigation
import Cloudinary
import DataSource exposing (DataSource)
import DataSource.File as File
import Footer
import Head
import Head.Seo as Seo
import HeroIcons
import Html as Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Events as Event
import Html.Styled as Htmls
import Html.Styled.Attributes as AttrS
import Http
import OptimizedDecoder as Decode exposing (Decoder)
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path exposing (Path)
import Process
import Route exposing (Route)
import Shared
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import Task
import View exposing (View)


type alias Model =
    { menuOpen : Bool
    , verNotificaciones : Maybe Bool
    , showSlider : Bool
    , avanzoManual : Bool
    , dirAvance : DirAvanceManual
    , inicializado : Bool
    , cualSlideActivo : Int
    , aminar : Amimacion
    , cambia : Int
    , cuantasFotos : Int
    }


type Amimacion
    = Entra
    | Sale


type DirAvanceManual
    = None
    | Izq
    | Der


type alias RouteParams =
    {}


init : Maybe PageUrl -> Shared.Model -> StaticPayload templateData routeParams -> ( Model, Cmd Msg )
init _ _ _ =
    ( { menuOpen = False
      , verNotificaciones = Just True
      , showSlider = False
      , avanzoManual = False
      , dirAvance = None
      , inicializado = False
      , cualSlideActivo = 0
      , aminar = Entra
      , cambia = 0
      , cuantasFotos = Array.length textosGal
      }
    , Task.attempt CheckGalInView (Dom.getElement "slider-container")
    )


page : Page.PageWithState RouteParams Data Model Msg
page =
    Page.single
        { head = head
        , data = data
        }
        |> Page.buildWithSharedState
            { view = view
            , init = init
            , update = update
            , subscriptions = subscriptions
            }


subscriptions : Maybe PageUrl -> routeParams -> Path.Path -> Model -> Shared.Model -> Sub Msg
subscriptions _ _ _ _ _ =
    Sub.none


type Msg
    = ToggleMenu
    | CierraNoti
    | CheckGalInView (Result Dom.Error Dom.Element)
    | WaitToCheckAgainGalInView
    | WaitToGalAutoRotate
    | Avanza
    | Retrocede
    | Para
    | PresionoBotonIzq
    | PresionoBotonDer


type Effect
    = EsperaLuegoPara
    | Nada
    | LanzaPara


runEffect : Effect -> Cmd Msg
runEffect efecto =
    case efecto of
        EsperaLuegoPara ->
            Task.perform (\_ -> Para) (Process.sleep 1300)

        Nada ->
            Cmd.none

        LanzaPara ->
            Task.perform (\_ -> Para) (Task.succeed ())


update : PageUrl -> Maybe Browser.Navigation.Key -> Shared.Model -> StaticPayload templateData routeParams -> Msg -> Model -> ( Model, Cmd Msg, Maybe Shared.Msg )
update url maybeKey sharedM staticP msg model =
    case msg of
        ToggleMenu ->
            ( { model | menuOpen = not model.menuOpen }
            , Cmd.none
            , Nothing
            )

        CierraNoti ->
            ( { model | verNotificaciones = Just False }
            , Cmd.none
            , Nothing
            )

        CheckGalInView resultaPos ->
            let
                galInSight pos =
                    (pos.element.y - 0.7 * pos.viewport.height) < pos.viewport.y

                onView : Maybe Float
                onView =
                    case resultaPos of
                        Ok pos ->
                            if galInSight pos then
                                Nothing

                            else
                                Just (1.0 - (pos.viewport.y / pos.element.y))

                        _ ->
                            Just 1.0
            in
            ( { model
                | showSlider =
                    if onView == Nothing then
                        True

                    else
                        False
              }
            , case onView of
                Just waitingTime ->
                    Task.perform
                        (\_ -> WaitToCheckAgainGalInView)
                        (Process.sleep <| 200000 * waitingTime)

                -- debe ser 2000
                Nothing ->
                    Task.perform
                        (\_ -> WaitToGalAutoRotate)
                        (Process.sleep <| 6500)
            , Nothing
            )

        WaitToCheckAgainGalInView ->
            ( model
            , Task.attempt CheckGalInView (Dom.getElement "slider-container")
            , Nothing
            )

        WaitToGalAutoRotate ->
            if model.avanzoManual then
                ( { model | avanzoManual = False }
                , Task.perform
                    (\_ -> WaitToGalAutoRotate)
                    (Process.sleep <| 12500)
                , Nothing
                )

            else
                ( model
                , Cmd.batch
                    [ Task.perform
                        (\_ -> WaitToGalAutoRotate)
                        (Process.sleep <| 6500)
                    , Task.perform
                        (\_ ->
                            if model.dirAvance == Izq then
                                Retrocede

                            else
                                Avanza
                        )
                        (Task.succeed ())
                    ]
                , Nothing
                )

        PresionoBotonIzq ->
            update
                url
                maybeKey
                sharedM
                staticP
                Retrocede
                { model
                    | dirAvance = Izq
                    , avanzoManual = True
                }

        PresionoBotonDer ->
            update
                url
                maybeKey
                sharedM
                staticP
                Avanza
                { model
                    | dirAvance = Der
                    , avanzoManual = True
                }

        Avanza ->
            ( { model
                | cambia =
                    if model.showSlider then
                        1

                    else
                        0
                , aminar = Sale
                , showSlider = True
              }
            , if model.showSlider then
                Task.perform (\_ -> Para) (Process.sleep 1300)

              else
                Task.perform (\_ -> Para) (Task.succeed ())
            , Nothing
            )

        Retrocede ->
            ( { model
                | cambia =
                    if model.showSlider then
                        -1

                    else
                        0
                , aminar = Sale
                , showSlider = True
              }
            , if model.showSlider then
                Task.perform (\_ -> Para) (Process.sleep 1300)

              else
                Task.perform (\_ -> Para) (Task.succeed ())
            , Nothing
            )

        Para ->
            let
                nuevoSlideActivo : Int
                nuevoSlideActivo =
                    model.cualSlideActivo + model.cambia

                nuevoSlideActivoValidado : Int
                nuevoSlideActivoValidado =
                    if nuevoSlideActivo < 0 then
                        model.cuantasFotos - 1

                    else if nuevoSlideActivo == model.cuantasFotos then
                        0

                    else
                        nuevoSlideActivo
            in
            ( { model
                | cualSlideActivo = nuevoSlideActivoValidado
                , aminar = Entra
                , cambia = 0
              }
            , Cmd.none
            , Nothing
            )


type alias Data =
    { title : String
    , tags : List String
    , mainHead : HeaderText
    , beneficios : Beneficios
    }


type alias Beneficios =
    { preHeader : String
    , header : String
    , subHeader : String
    , motivos : List Arts
    }


type alias HeaderText =
    { preMainHeader : String
    , mainHeaderResaltado : String
    , postMainHeader : String
    , mainSubHeader : String
    }


type alias Arts =
    { cabeza : String
    , nota : String
    }


yamlDecoder : Decoder Data
yamlDecoder =
    let
        beneDecoder =
            Decode.map4 Beneficios
                (Decode.field "preHeader" Decode.string)
                (Decode.field "header" Decode.string)
                (Decode.field "subHeader" Decode.string)
                (Decode.field "art" artsDecoder
                    |> Decode.list
                    |> Decode.field "motivos"
                )

        artsDecoder =
            Decode.map2 Arts
                (Decode.field "cabeza" Decode.string)
                (Decode.field "nota" Decode.string)

        mainHeaderDecoder =
            Decode.map4 HeaderText
                (Decode.field "preMainHeader" Decode.string)
                (Decode.field "mainHeaderResaltado" Decode.string)
                (Decode.field "postMainHeader" Decode.string)
                (Decode.field "mainSubHeader" Decode.string)
    in
    Decode.map4 Data
        (Decode.field "title" Decode.string)
        (Decode.field "tags" (Decode.list Decode.string))
        (Decode.field "mainHead" mainHeaderDecoder)
        (Decode.field "beneficios" beneDecoder)


data : DataSource Data
data =
    File.onlyFrontmatter
        yamlDecoder
        "data/index.yaml"


head : StaticPayload Data RouteParams -> List Head.Tag
head static =
    let
        imagen : Seo.Image
        imagen =
            { alt = "Logo PSOLAR.MX"
            , dimensions = Just { width = 723, height = 716 }
            , mimeType = Just "png"
            , url =
                Pages.Url.external <|
                    Cloudinary.url "f_webp" "v1642824483/logoMod_mryxdq.png"
            }
    in
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "PSOLAR"
        , image = imagen
        , description = "Servicio Experto a Panel Solar"
        , locale = Nothing
        , title = "Servicio a Panel Solar"
        }
        |> Seo.website


view : Maybe PageUrl -> Shared.Model -> Model -> StaticPayload Data RouteParams -> View Msg
view maybeUrl sharedModel model static =
    { title = static.data.title
    , body =
        [ viewHero model.menuOpen static.data.mainHead
        , viewFeatures static.data.beneficios
        , viewNotificacion sharedModel.usuarioStatus model.verNotificaciones

        --, viewGaleria model
        , indexViewFooter
        ]
    }



-- Notificación


viewNotificacion : Shared.UsuarioSt -> Maybe Bool -> Html Msg
viewNotificacion usrStatus verNotif =
    let
        respFromPost : Result Http.Error String -> String
        respFromPost resp =
            case resp of
                Ok _ ->
                    "Registrado Ok, nos comunicaremos pronto."

                Err cual ->
                    case cual of
                        Http.BadUrl urlBad ->
                            "Pero, error en programa " ++ urlBad

                        Http.Timeout ->
                            "No respondió el servidor, Intente de nuevo."

                        Http.NetworkError ->
                            "Falló el internet."

                        Http.BadStatus codigo ->
                            "Servidor regresó error " ++ String.fromInt codigo

                        Http.BadBody infoEnviada ->
                            "Problemas con la información " ++ String.left 20 infoEnviada
    in
    case usrStatus of
        Shared.Conocido respBasin ->
            retroFinal
                HeroIcons.outlineCheckCircle
                "Formulario Recibido"
                (respFromPost respBasin)
                verNotif
                |> Html.map (\_ -> CierraNoti)

        Shared.Rechazado ->
            retroFinal
                HeroIcons.outlineCheckCircle
                "¡Información no registrada!"
                "Era necesario resolver la ecuación."
                verNotif
                |> Html.map (\_ -> CierraNoti)

        Shared.Desconocido ->
            div [] []


notifAppear : Maybe Bool -> Animation
notifAppear show =
    case show of
        Nothing ->
            Animation.empty

        Just siAmimar ->
            if siAmimar then
                Animation.fromTo
                    { duration = 750
                    , options =
                        [ Animation.delay 1100
                        , Animation.easeOut
                        ]
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


retroFinal : Html Msg -> String -> String -> Maybe Bool -> Html Msg
retroFinal icono titulo subtitulo debeAparecer =
    Animated.div
        (notifAppear debeAparecer)
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
                                , Event.onClick CierraNoti
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



-- View Footer


indexViewFooter : Html msg
indexViewFooter =
    let
        viewPieNavega : List (Htmls.Html msg)
        viewPieNavega =
            []

        {-
           [ Footer.ligaAlPie "#" "About"
           , Footer.ligaAlPie "#" "Blog"
           , Footer.ligaAlPie "#" "Jobs"
           , Footer.ligaAlPie "#" "Press"
           , Footer.ligaAlPie "#" "Accesibility"
           , Footer.ligaAlPie "#" "Partners"
           ]
        -}
        viewPieSocialIcons : List (Htmls.Html msg)
        viewPieSocialIcons =
            [ Footer.ligaIcono "https://github.com/rolojf/psolar" "GitHub" Footer.Github
            , Footer.ligaIcono "https://www.linkedin.com/in/rolando-flores-gzz-80887163/" "LinkedIn" Footer.LinkedIn

            --, Footer.ligaIcono "whatsapp.com" "Whatsapp" Footer.WhatsApp
            , Footer.ligaIcono
                (Route.Contacto |> Route.toPath |> Path.toRelative)
                "Correo"
                Footer.Email
            ]
    in
    Footer.viewFooter
        viewPieNavega
        viewPieSocialIcons
        "REFTEX INGENIERIA, S.A. de C.V. - 2022"



-- View of Above the Fold


type LigaTipo
    = Otra Path
    | Interna Route


viewHero menuOpen headText =
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

        clasesMenuItems : (Bool, Bool) -> Html.Attribute msg
        clasesMenuItems (esMovil, especial) =
            case ( esMovil, especial ) of
                ( True, True ) ->
                    class "block w-full px-5 py-3 text-center font-medium text-blue-900 bg-gray-50 hover:bg-gray-200"

                ( True, False ) ->
                    class "block px-5 py-3 rounded-md font-medium text-base text-gray-700 hover:text-gray-900 hover:bg-gray-50"

                ( False, True ) ->
                    class "font-medium text-blue-900 hover:text-blue-500"

                ( False, False ) ->
                    class "font-medium text-gray-500 hover:text-gray-900"

        menuItem : (Bool, Bool) -> { texto : String, dir : LigaTipo } -> Html msg
        menuItem tipClases dirToLink =
            case dirToLink.dir of
                Otra camino ->
                    Html.a
                        [ Attr.href <| Path.toRelative camino
                        , (clasesMenuItems tipClases)
                        ]
                        [ text dirToLink.texto ]

                Interna rutaLiga ->
                    Route.link
                        rutaLiga
                        [ (clasesMenuItems tipClases) ]
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
                    (List.append
                        (List.map
                            (menuItem
                                (True, False))
                            direcciones
                        )
                        (List.singleton <|
                            menuItem
                                (True, True)
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
                                (List.append
                                    (List.map
                                        (menuItem (False, False))
                                        direcciones
                                        )
                                    (List.singleton <|
                                        menuItem
                                            (False, True)
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
                , Html.main_
                    [ class "mt-10 mx-auto max-w-7xl px-4 sm:mt-12 sm:px-6 md:mt-16 lg:mt-20 lg:px-8 xl:mt-28" ]
                    [ div
                        [ class "sm:text-center lg:text-left"
                        ]
                        [ Html.h1
                            [ class "text-4xl font-serif font-extrabold text-gray-900 sm:text-5xl" ]
                            [ Html.span
                                [ class "block xl:inline" ]
                                [ text headText.preMainHeader ]
                            , Html.span
                                [ class "block text-blue-900 xl:inline" ]
                                [ text <| " " ++ headText.mainHeaderResaltado ++ " " ]
                            , Html.span
                                [ class "block xl:inline" ]
                                [ text headText.postMainHeader ]
                            ]
                        , Html.p
                            [ class "mt-3 text-base text-gray-500 sm:mt-5 sm:text-lg sm:max-w-xl sm:mx-auto md:mt-5 md:text-xl lg:mx-0" ]
                            [ text headText.mainSubHeader ]
                        , div
                            [ class "mt-5 sm:mt-8 sm:flex sm:justify-center lg:justify-start" ]
                            [ div
                                [ class "rounded-md shadow" ]
                                [ Route.link
                                    Route.Contacto
                                    [ class "w-full flex items-center justify-center px-8 py-3 border border-transparent text-base font-medium rounded-md text-white bg-blue-900 hover:bg-indigo-700 md:py-4 md:text-lg md:px-10" ]
                                    [ text "¡Contáctanos!" ]
                                ]
                            , div
                                [ class "mt-3 sm:mt-0 sm:ml-3" ]
                                [ Html.a
                                    [ Attr.href "#features"
                                    , class "w-full flex items-center justify-center px-8 py-3 border border-transparent text-base font-medium rounded-md text-indigo-700 bg-indigo-100 hover:bg-indigo-200 md:py-4 md:text-lg md:px-10"
                                    ]
                                    [ text "más info." ]
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
                , Attr.src (Cloudinary.url "f_auto,q_auto:best" "dreamstime_s_30697263_clymr0.jpg")
                , Attr.alt "Expertos trajamos por ti"
                ]
                []
            ]
        ]



-- View Features


viewFeatures : Beneficios -> Html msg
viewFeatures bene =
    let
        viewArts : Arts -> Html msg
        viewArts articulo =
            div
                [ class "relative" ]
                [ Html.dt []
                    [ HeroIcons.outlineCheck
                    , Html.p
                        [ class "ml-9 text-lg leading-6 font-medium text-gray-900"
                        ]
                        [ text articulo.cabeza ]
                    ]
                , Html.dd
                    [ class "mt-2 ml-9 text-base text-gray-500"
                    ]
                    [ text articulo.nota ]
                ]
    in
    div
        [ class "bg-white"
        , Attr.id "features"
        ]
        [ div
            [ class "max-w-7xl mx-auto py-16 px-4 sm:px-6 lg:py-24 lg:px-8 lg:grid lg:grid-cols-3 lg:gap-x-8" ]
            [ div []
                [ Html.h2
                    [ class "text-base font-semibold text-indigo-600 uppercase tracking-wide" ]
                    [ text bene.preHeader ]
                , Html.p
                    [ class "mt-2 text-3xl font-extrabold text-gray-900 font-serif tracking-wide" ]
                    [ text bene.header ]
                , Html.p
                    [ class "mt-4 text-lg text-gray-500" ]
                    [ text bene.subHeader ]
                ]
            , div
                [ class "mt-12 lg:mt-0 lg:col-span-2" ]
                [ Html.dl
                    [ class "space-y-10 sm:space-y-0 sm:grid sm:grid-cols-2 sm:grid-rows-3 sm:grid-flow-col sm:gap-x-6 sm:gap-y-10 lg:gap-x-8" ]
                    (List.map viewArts bene.motivos)
                ]
            ]
        ]



-- View Galeria


textosGal : Array String
textosGal =
    [ "Uno"
    , "Dos"
    , "tres"
    , "cuatro"
    , "cinco"
    ]
        |> Array.fromList


viewGaleria : Model -> Html Msg
viewGaleria modeloDeGal =
    let
        listadoCompletoImgs : Array String
        listadoCompletoImgs =
            List.map
                (\cual -> "https://picsum.photos/seed/" ++ String.fromChar cual ++ "/700/700")
                (String.toList "abcdefghijklmnopqrst")
                |> Array.fromList
    in
    viewGal
        listadoCompletoImgs
        textosGal
        modeloDeGal


viewGal : Array String -> Array String -> Model -> Html Msg
viewGal listadoCompletoImgs textos model =
    div
        [ Attr.id "slider-container" ]
        [ viewSlider
            model.showSlider
            listadoCompletoImgs
            textos
            model.cualSlideActivo
            model.aminar
        ]


viewSlider : Bool -> Array String -> Array String -> Int -> Amimacion -> Html Msg
viewSlider showIt listadoCompletoImgs textos slideActivo animar =
    let
        letraVa : Int -> Animation
        letraVa orden =
            Animation.fromTo
                { duration = 400
                , options =
                    [ Animation.delay (orden * 70)
                    , Animation.easeInQuint
                    ]
                }
                [ P.opacity 1
                , P.y 0
                ]
                [ P.opacity 0
                , P.y -60.0
                ]

        letraViene : Int -> Animation
        letraViene orden =
            Animation.fromTo
                { duration = 600
                , options =
                    [ Animation.delay (1000 + orden * 70)
                    , Animation.easeOutQuint
                    ]
                }
                [ P.opacity 0
                , P.y 60.0
                ]
                [ P.opacity 1
                , P.y 0
                ]

        fotoVa : Int -> Animation
        fotoVa orden =
            Animation.fromTo
                { duration = 400
                , options =
                    [ Animation.delay (500 + 120 * orden)
                    , Animation.easeInCubic
                    ]
                }
                [ P.opacity 1
                , P.y 0
                ]
                [ P.opacity 0
                , P.y -600.0
                ]

        fotoViene : Int -> Animation
        fotoViene orden =
            Animation.fromTo
                { duration = 400
                , options =
                    [ Animation.delay (200 * orden)
                    , Animation.easeInCubic
                    ]
                }
                [ P.opacity 0
                , P.y 600
                ]
                [ P.opacity 1
                , P.y 0
                ]

        despliega4 : Array String -> List (Html msg)
        despliega4 subListado =
            Array.toIndexedList subListado
                |> List.foldl
                    -- (a -> b -> b) -> b -> List a -> b -//- tuple  -> Html msg
                    (\( indice, direccion ) listadoAc ->
                        div
                            [ class <| "img img-" ++ String.fromInt (indice + 1) ]
                            [ case animar of
                                Sale ->
                                    Animated.html
                                        Html.img
                                        (fotoVa indice)
                                        [ Attr.src direccion ]
                                        []

                                Entra ->
                                    Animated.html
                                        Html.img
                                        (fotoViene indice)
                                        [ Attr.src direccion ]
                                        []
                            ]
                            :: listadoAc
                    )
                    []

        seccionDeImagenes desdeCual =
            div
                [ class "imgs" ]
                [ div
                    [ class "grid" ]
                  <|
                    despliega4 <|
                        Array.slice
                            desdeCual
                            (desdeCual + 4)
                            listadoCompletoImgs
                ]

        seccionTexto =
            div
                [ class "content" ]
                [ div
                    [ class "wrap" ]
                    (textos
                        |> Array.get slideActivo
                        |> Maybe.withDefault ""
                        |> String.toList
                        |> List.indexedMap
                            (\indice letra ->
                                case animar of
                                    Sale ->
                                        Animated.html
                                            Html.span
                                            (letraVa indice)
                                            [ class "letter" ]
                                            [ text (String.fromChar letra) ]

                                    Entra ->
                                        Animated.html
                                            Html.span
                                            (letraViene indice)
                                            [ class "letter" ]
                                            [ text (String.fromChar letra) ]
                            )
                    )
                ]
    in
    div
        [ class "slider" ]
        [ div
            [ class "nav" ]
            [ div
                [ class "next"
                , Event.onClick PresionoBotonDer
                ]
                []
            , div
                [ class "prev"
                , Event.onClick PresionoBotonIzq
                ]
                []
            , div
                [ class "explore-btn" ]
                [ text "Explore" ]
            , if showIt then
                div
                    [ class "item" ]
                    [ seccionDeImagenes (4 * slideActivo)
                    , seccionTexto
                    ]

              else
                div [ class "item" ]
                    [ seccionTexto
                    ]
            ]
        ]
