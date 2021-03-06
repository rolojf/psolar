module Page.Contacto exposing (Data, Model, Msg, page)

-- exposing (css)

import Analytics
import Browser.Dom as Dom
import Browser.Navigation
import Cloudinary
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Attributes.Aria as Aria
import Html.Events as Events
import Http
import Json.Encode as Encode
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path
import Process
import Reto
import Route
import Shared
import Task
import View exposing (View)


type alias Model =
    { nombre : String
    , comoSupo : String
    , correo : String
    , comentario : String
    , telefono : String
    , apellido : String
    , listo : Bool
    , reModel : Reto.Model
    , respondio : Bool
    , deBasin : Result Http.Error String
    }


type Msg
    = Nombre String
    | ComoSupo String
    | Correo String
    | Apellido String
    | Telefono String
    | Comentario String
    | CompletadoFormulario
    | EsperaPaEnfocar
    | ReMsg Reto.Msg
    | NoOp
    | RespondeBasin (Result Http.Error String)
    | Notificado (Result Http.Error ())


init : Maybe PageUrl -> Shared.Model -> StaticPayload templateData routeParams -> ( Model, Cmd Msg )
init _ _ _ =
    ( { nombre = ""
      , comoSupo = ""
      , correo = ""
      , comentario = ""
      , telefono = ""
      , apellido = ""
      , listo = False
      , reModel = Reto.newModel
      , respondio = False
      , deBasin = Err (Http.BadStatus 9999)
      }
    , Cmd.none
    )


update : PageUrl -> Maybe Browser.Navigation.Key -> Shared.Model -> StaticPayload templateData routeParams -> Msg -> Model -> ( Model, Cmd Msg, Maybe Shared.Msg )
update _ navKey sharedModel _ msg model =
    case msg of
        Nombre cCampo ->
            ( { model | nombre = cCampo }, Cmd.none, Nothing )

        ComoSupo cCampo ->
            ( { model | comoSupo = cCampo }, Cmd.none, Nothing )

        Correo cCampo ->
            ( { model | correo = cCampo }, Cmd.none, Nothing )

        Apellido cCampo ->
            ( { model | apellido = cCampo }, Cmd.none, Nothing )

        Telefono conQue ->
            let
                entered =
                    String.right 1 conQue

                conQue1 =
                    if String.contains entered "01234567890 _-.+" then
                        entered

                    else
                        ""
            in
            ( { model | telefono = String.dropRight 1 conQue ++ conQue1 }
            , Cmd.none
            , Nothing
            )

        Comentario cCampo ->
            ( { model | comentario = cCampo }, Cmd.none, Nothing )

        CompletadoFormulario ->
            ( { model | listo = True }
            , Cmd.batch
                [ Task.perform
                    (\_ -> EsperaPaEnfocar)
                    (Process.sleep 100)
                , Analytics.toCmd
                    (Analytics.eventoXReportar "completo-formulario")
                    Notificado
                ]
            , Nothing
            )

        EsperaPaEnfocar ->
            ( model
            , Task.attempt (\_ -> NoOp) (Dom.focus "valor-challenge")
            , Nothing
            )

        ReMsg reMsg ->
            let
                modeloQResulta =
                    Reto.update reMsg model.reModel

                cuerpoPost : Encode.Value
                cuerpoPost =
                    Encode.object
                        [ ( "name", Encode.string model.nombre )
                        , ( "apellido", Encode.string model.apellido )
                        , ( "correo", Encode.string model.correo )
                        , ( "telefono", Encode.string model.telefono )
                        , ( "llego", Encode.string model.comoSupo )
                        , ( "comentario", Encode.string model.comentario )
                        ]

                mandaForma =
                    Http.post
                        { url = "https://usebasin.com/f/41489cfac434"
                        , body = Http.jsonBody cuerpoPost
                        , expect = Http.expectString RespondeBasin
                        }
            in
            ( { model | reModel = modeloQResulta }
            , if modeloQResulta.vaDeNuez then
                Task.perform
                    (\_ -> ReMsg Reto.IntentaDeNuez)
                    (Process.sleep 500)

              else
                case modeloQResulta.intento of
                    Reto.YaOk ->
                        mandaForma

                    Reto.EstaFrito ->
                        navKey
                            |> Maybe.map
                                (\llave ->
                                    Browser.Navigation.pushUrl
                                        llave
                                        (Pages.Url.fromPath (Route.toPath Route.Index)
                                            |> Pages.Url.toString
                                        )
                                )
                            |> Maybe.withDefault Cmd.none

                    _ ->
                        Cmd.none
            , if modeloQResulta.intento == Reto.YaOk then
                Nothing

              else if modeloQResulta.intento == Reto.EstaFrito then
                Just (Shared.SharedMsg <| Shared.CambiaStatus Shared.Rechazado)

              else
                Just (Shared.SharedMsg <| Shared.CambiaStatus Shared.Desconocido)
            )

        NoOp ->
            ( model
            , Cmd.none
            , Nothing
            )

        RespondeBasin respuesta ->
            ( { model | deBasin = respuesta }
            , navKey
                |> Maybe.map
                    (\llave ->
                        Browser.Navigation.pushUrl
                            llave
                            (Pages.Url.fromPath (Route.toPath Route.Index)
                                |> Pages.Url.toString
                            )
                    )
                |> Maybe.withDefault Cmd.none
            , Shared.Conocido respuesta
                |> Shared.CambiaStatus
                |> Shared.SharedMsg
                |> Just
            )

        Notificado resulto ->
            ( model
            , Cmd.none
            , case resulto of
                Err quePaso ->
                    Just (Shared.SharedMsg <| Shared.ErrorAlNotificar quePaso)

                Ok _ ->
                    Nothing
            )


subscriptions : Maybe PageUrl -> routeParams -> Path.Path -> Model -> Shared.Model -> Sub Msg
subscriptions _ _ _ _ _ =
    Sub.none


type alias RouteParams =
    {}


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


type alias Data =
    ()


data : DataSource Data
data =
    DataSource.succeed ()


head :
    StaticPayload Data RouteParams
    -> List Head.Tag
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
        , description = "Hoja de Contactar a PSOLAR"
        , locale = Nothing
        , title = "Contactando a PSOLAR"
        }
        |> Seo.website


view :
    Maybe PageUrl
    -> Shared.Model
    -> Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel model static =
    { title = "Formulario de Contacto"
    , withMenu = View.NoMenu
    , body =
        [ div
            [ class "tw relative bg-white" ]
            [ viewLayout
            , viewFormulario model
            , if model.listo then
                div
                    [ class "tw lg:h-72" ]
                    [ if sharedModel.usuarioStatus == Shared.Desconocido then
                        Reto.view model.reModel
                            |> Html.map ReMsg

                      else
                        text ""
                    ]

              else
                div [] []
            ]
        ]
    }


viewLayout : Html Msg
viewLayout =
    div
        [ class "tw lg:absolute lg:inset-0" ]
        [ div
            [ class "tw lg:absolute lg:inset-y-0 lg:right-0 lg:w-1/2" ]
            [ Html.img
                [ class "tw h-56 w-full object-cover object-top lg:absolute lg:h-screen"
                , Attr.src <|
                    Cloudinary.url "f_auto" "v1619940728/dreamstime_m_29668275_t0oapr.jpg"
                , Attr.alt ""
                ]
                []
            ]
        ]


viewFormulario : Model -> Html Msg
viewFormulario model =
    let
        viewCampoNombre =
            div
                []
                [ Html.label
                    [ Attr.for "first_name"
                    , class "tw block text-sm font-medium text-gray-700"
                    ]
                    [ text "Nombre" ]
                , div
                    [ class "tw mt-1" ]
                    [ Html.input
                        [ Attr.type_ "text"
                        , Attr.name "first_name"
                        , Attr.id "first_name"
                        , Attr.required True
                        , Attr.minlength 2
                        , Attr.maxlength 15
                        , Attr.autocomplete True -- "given-name"
                        , class "tw block w-full shadow-sm sm:text-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 rounded-md"
                        , Events.onInput Nombre
                        ]
                        []
                    ]
                ]

        viewCampoApellido =
            div []
                [ Html.label
                    [ Attr.for "last_name"
                    , class "tw block text-sm font-medium text-gray-700"
                    ]
                    [ text "Apellido" ]
                , div
                    [ class "tw mt-1" ]
                    [ Html.input
                        [ Attr.type_ "text"
                        , Attr.name "last_name"
                        , Attr.id "last_name"
                        , Attr.autocomplete True -- "family-name"
                        , class "tw block w-full shadow-sm sm:text-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 rounded-md"
                        , Events.onInput Apellido
                        ]
                        []
                    ]
                ]

        viewCampoCorreo =
            div
                [ class "tw sm:col-span-2" ]
                [ Html.label
                    [ Attr.for "email"
                    , class "tw block text-sm font-medium text-gray-700"
                    ]
                    [ text "Correo Electr??nico" ]
                , div
                    [ class "tw mt-1" ]
                    [ Html.input
                        [ Attr.id "email"
                        , Attr.name "email"
                        , Attr.type_ "email"
                        , Attr.autocomplete True --"email"
                        , class "tw block w-full shadow-sm sm:text-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 rounded-md"
                        , Events.onInput Correo
                        ]
                        []
                    ]
                ]

        viewCampoTelefono =
            div
                [ class "tw sm:col-span-2" ]
                [ div
                    [ class "tw flex justify-between" ]
                    [ Html.label
                        [ Attr.for "phone"
                        , class "tw block text-sm font-medium text-gray-700"
                        ]
                        [ text "Tel??fono" ]
                    , Html.span
                        [ Attr.id "phone_description"
                        , class "tw text-sm text-gray-500"
                        ]
                        [ text "Opcional" ]
                    ]
                , div
                    [ class "tw mt-1" ]
                    [ Html.input
                        [ Attr.type_ "text"
                        , Attr.name "phone"
                        , Attr.id "phone"
                        , Attr.minlength 8
                        , Attr.maxlength 15
                        , Attr.value model.telefono
                        , Attr.autocomplete True -- "tel"
                        , Aria.ariaDescribedby "phone_description"
                        , class "tw block w-full shadow-sm sm:text-sm focus:ring-indigo-500 focus:border-indigo-500 border-gray-300 rounded-md"
                        , Events.onInput Telefono
                        ]
                        []
                    ]
                ]

        viewCampoComment =
            div
                [ class "tw sm:col-span-2" ]
                [ div
                    [ class "tw flex justify-between" ]
                    [ Html.label
                        [ Attr.for "how_can_we_help"
                        , class "tw block text-sm font-medium text-gray-700"
                        ]
                        [ text "Comentario" ]
                    , Html.span
                        [ Attr.id "how_can_we_help_description"
                        , class "tw text-sm text-gray-500"
                        ]
                        [ text ">Max. 500 caracteres" ]
                    ]
                , div
                    [ class "tw mt-1" ]
                    [ Html.textarea
                        [ Attr.id "how_can_we_help"
                        , Attr.name "how_can_we_help"
                        , Aria.ariaDescribedby "how_can_we_help_description"
                        , Attr.rows 4
                        , class "tw block w-full shadow-sm sm:text-sm focus:ring-indigo-500 focus:border-indigo-500 border_gray_300 rounded-md"
                        , Events.onInput Comentario
                        ]
                        []
                    ]
                ]

        viewComoSupoDeNos =
            div
                [ class "tw sm:col-span-2" ]
                [ Html.label
                    [ Attr.for "how_did_you_hear_about_us"
                    , class "tw block text-sm font-medium text-gray-700"
                    ]
                    [ text "??C??mo lleg?? con nosotros?" ]
                , div
                    [ class "tw mt-1" ]
                    [ Html.input
                        [ Attr.type_ "text"
                        , Attr.name "how_did_you_hear_about_us"
                        , Attr.id "how_did_you_hear_about_us"
                        , class "tw shadow-sm focus:ring-indigo-500 focus:border-indigo-500 block w_full sm:text-sm border-gray-300 rounded-md"
                        , Events.onInput ComoSupo
                        ]
                        []
                    ]
                ]

        viewBotonSubmit =
            div
                [ class "tw text-right sm:col-span-2" ]
                [ Html.button
                    [ Attr.type_ "submit"
                    , class "tw inline-flex justify-center py-2 px-4 border border-transparent shadow-sm text-sm font-medium rounded-md text-white bg-indigo-600 hover:bg-indigo-700 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                    ]
                    [ text "Enviar" ]
                ]
    in
    div
        [ class "tw relative py-8 px-4 sm:px-6 lg:px-8 lg:max-w-7xl lg:mx-auto lg:grid lg:grid-cols-2 lg:py-8" ]
        [ div
            [ class "tw lg:pr-8" ]
            [ div
                [ class "tw max-w-md mx-auto lg:mx-0 sm:max-w-lg" ]
                [ Html.h2
                    [ class "tw text-3xl font-extrabold tracking-tight sm:text-4xl font-serif" ]
                    [ text "??C??mo Podemos Ayudar?" ]
                , Html.p
                    [ class "tw mt-4 text-lg text-gray-500 sm:mt-3" ]
                    [ text "Responderemos tan pronto sea posible con un correo electr??nico o con un mensaje a su tel??fono. Gracias." ]
                , Html.form
                    [ Attr.action "#"
                    , Attr.method "POST"
                    , Events.onSubmit CompletadoFormulario
                    , class "tw mt-9 grid grid-cols-1 gap-y-6 sm:grid-cols-2 sm:gap-x-8"
                    ]
                    [ viewCampoNombre
                    , viewCampoApellido
                    , viewCampoCorreo
                    , viewCampoTelefono
                    , viewCampoComment
                    , viewComoSupoDeNos
                    , viewBotonSubmit
                    ]
                ]
            ]
        ]
