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
import Notifica
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
    , verNotificaciones : Bool
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


init :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload templateData routeParams
    -> ( Model, Cmd Msg )
init _ _ _ =
    ( { menuOpen = False
      , verNotificaciones = True
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


subscriptions :
    Maybe PageUrl
    -> routeParams
    -> Path.Path
    -> Model
    -> Shared.Model
    -> Sub Msg
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


update :
    PageUrl
    -> Maybe Browser.Navigation.Key
    -> Shared.Model
    -> StaticPayload templateData routeParams
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, Maybe Shared.Msg )
update url maybeKey sharedM staticP msg model =
    case msg of
        ToggleMenu ->
            ( { model | menuOpen = not model.menuOpen }
            , Cmd.none
            , Nothing
            )

        CierraNoti ->
            ( { model | verNotificaciones = False }
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
                        (Process.sleep <| 2000 * waitingTime)

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
                (Decode.field
                    "motivos"
                 <|
                    Decode.list <|
                        Decode.field "art" artsDecoder
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


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
head static =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = "psolar"
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


view :
    Maybe PageUrl
    -> Shared.Model
    -> Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel model static =
    { title = static.data.title
    , body =
        [ viewHero model.menuOpen static.data.mainHead
        , viewFeatures static.data.beneficios
        , if sharedModel.usuarioStatus == Shared.Conocido then
            Notifica.retroFinal
                HeroIcons.outlineCheckCircle
                "Maravillos Vas Bien"
                "Solo no te desesperes por favor hay que echarle ganas"
                model.verNotificaciones
                |> Html.map (\_ -> CierraNoti)

          else if sharedModel.usuarioStatus == Shared.Rechazado then
            Notifica.retroFinal
                HeroIcons.outlineCheckCircle
                "Pinche Bot Cularo"
                "Qué esperabas cabrón, solo así y ya?"
                model.verNotificaciones
                |> Html.map (\_ -> CierraNoti)

          else
            div [] []
        --, viewGaleria model
        , indexViewFooter
        ]
    }


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
            ]-}

        viewPieSocialIcons : List (Htmls.Html msg)
        viewPieSocialIcons =
            [ Footer.ligaIcono "https://github.com/rolojf/psolar" "GitHub" Footer.Github
            , Footer.ligaIcono "https://www.linkedin.com/in/rolando-flores-gzz-80887163/" "LinkedIn" Footer.LinkedIn
            --, Footer.ligaIcono "whatsapp.com" "Whatsapp" Footer.WhatsApp
            , Footer.ligaIcono
                (Route.Contacto |> Route.toPath |> Path.toRelative )
                 "Correo"
                 Footer.Email
            ]
    in
    Footer.viewFooter
        viewPieNavega
        viewPieSocialIcons
        "REFTEX INGENIERIA, S.A. de C.V. - 2022"


viewHero menuOpen headText =
    let
        direccionEspecial =
            { texto = "Huevón"
            , dir = Pages.Url.fromPath (Route.toPath Route.Contacto)
            }

        direcciones : List { texto : String, dir : Pages.Url.Url }
        direcciones =
            [ { texto = "Contáctanos"
              , dir = Pages.Url.fromPath (Route.toPath Route.Contacto)
              }
            , { texto = "Háblanos"
              , dir = Pages.Url.fromPath (Route.toPath Route.Contacto)
              }
            ]

        clasesComunItems =
            "font-medium hover:text-gray-900"

        clasesMenuItems : Bool -> String
        clasesMenuItems esMovil =
            if esMovil then
                "block px-3 py-2 rounded-md text-base text-gray-700 hover:bg-gray-50 " ++ clasesComunItems

            else
                clasesComunItems ++ " text-gray-500"

        menuItems : Bool -> { texto : String, dir : Pages.Url.Url } -> Html msg
        menuItems cualMenu direccion =
            Html.a
                [ Attr.href <| Pages.Url.toString direccion.dir
                , class <| clasesMenuItems cualMenu
                ]
                [ text direccion.texto ]

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
                    (List.map (menuItems True) direcciones)
                , Html.a
                    [ Attr.href "#"
                    , class "block w-full px-5 py-3 text-center font-medium text-indigo-600 bg-gray-50 hover:bg-gray-100"
                    ]
                    [ text direccionEspecial.texto ]
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
                                (List.map (menuItems False) direcciones
                                    ++ [ Html.a
                                            [ Attr.href "#"
                                            , class "text-indigo-600 hover:text-indigo-500"
                                            ]
                                            [ text direccionEspecial.texto ]
                                       ]
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
                , Attr.src (Cloudinary.url "f_auto,q_auto:best" "dreamstime_s_30697263_clymr0.jpg")
                , Attr.alt ""
                ]
                []
            ]
        ]


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
                    [ class "space-y-10 sm:space-y-0 sm:grid sm:grid-cols-2 sm:grid-rows-4 sm:grid-flow-col sm:gap-x-6 sm:gap-y-10 lg:gap-x-8" ]
                    (List.map viewArts bene.motivos)
                ]
            ]
        ]


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
