module Page.Contacto exposing (Data, Model, Msg, page)

-- exposing (css)

import Browser.Navigation
import Css
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Styled as Htmls exposing (div, text)
import Html.Styled.Attributes as Attr exposing (class)
import Html.Styled.Attributes.Aria as Aria
import Html.Styled.Events as Events
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path
import Process
import Reto
import Shared
import Tailwind.Breakpoints as TwBp
import Tailwind.Utilities as Tw
import Task
import View exposing (View)


type alias Model =
    { usuarioStatus : UsuarioSt
    , nombre : String
    , comoSupo : String
    , correo : String
    , comentario : String
    , telefono : String
    , apellido : String
    , listo : Bool
    , reModel : Reto.Model
    , respondio : Bool
    }


type UsuarioSt
    = Desconocido
    | Rechazado
    | Conocido
    | DiceQueRegresa


type Msg
    = Nombre String
    | ComoSupo String
    | Correo String
    | Apellido String
    | Telefono String
    | Comentario String
    | Enviado
    | ReMsg Reto.Msg


init :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload templateData routeParams
    -> ( Model, Cmd Msg )
init _ _ _ =
    ( { usuarioStatus = Desconocido
      , nombre = ""
      , comoSupo = ""
      , correo = ""
      , comentario = ""
      , telefono = ""
      , apellido = ""
      , listo = False
      , reModel = Reto.newModel
      , respondio = False
      }
    , Cmd.none
    )


update :
    PageUrl
    -> Maybe Browser.Navigation.Key
    -> Shared.Model
    -> StaticPayload templateData routeParams
    -> Msg
    -> Model
    -> ( Model, Cmd Msg, Maybe Shared.Msg )
update _ _ _ _ msg model =
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

        Enviado ->
            ( { model | listo = True }
            , Cmd.none
            , Nothing
            )

        ReMsg reMsg ->
            let
                modeloResultante =
                    Reto.update reMsg model.reModel
            in
            ( { model
                | reModel = modeloResultante
                , usuarioStatus =
                    if modeloResultante.intento == Reto.YaOk then
                        Conocido

                    else if modeloResultante.intento == Reto.EstaFrito then
                        Rechazado

                    else
                        Desconocido
              }
            , if modeloResultante.vaDeNuez then
                Task.perform
                    (\_ -> ReMsg Reto.IntentaDeNuez)
                    (Process.sleep 500)

              else
                Cmd.none
            , Nothing
            )


subscriptions :
    Maybe PageUrl
    -> routeParams
    -> Path.Path
    -> Model
    -> Shared.Model
    -> Sub Msg
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


view :
    Maybe PageUrl
    -> Shared.Model
    -> Model
    -> StaticPayload Data RouteParams
    -> View Msg
view maybeUrl sharedModel model static =
    { title = "Formulario de Contacto"
    , body =
        [ {- div
            [ Attr.css
                [ Tw.max_w_7xl
                , Tw.mx_auto
                , TwBp.sm [ Tw.px_6 ]
                , TwBp.lg [ Tw.px_8 ]
                ]
            ]
            [-} div
                [ Attr.css [ Tw.relative, Tw.bg_white ] ]
                [ viewLayout
                , viewFormulario model
                , if model.listo then
                    div
                        [ Attr.css [ TwBp.lg [ Tw.h_72 ] ] ]
                        [ case model.usuarioStatus of
                            Desconocido ->
                                Reto.view model.reModel |> Htmls.map ReMsg

                            Conocido ->
                                Htmls.h4 []
                                    [ text "HEY! Sos Familia!!" ]

                            Rechazado ->
                                Htmls.h4 []
                                    [ text "Pareces un Bot, intenta de nuevo" ]

                            _ ->
                                Htmls.h4 []
                                    [ text "¿Cómo chingados llegué a este estado?" ]
                        ]

                  else
                    div [] []
                ]
            ]
        -- ]
            |> List.map Htmls.toUnstyled
    }


viewLayout : Htmls.Html Msg
viewLayout =
    div
        [ Attr.css [ TwBp.lg [ Tw.absolute, Tw.inset_0 ] ] ]
        [ div
            [ Attr.css [ TwBp.lg [ Tw.absolute, Tw.inset_y_0, Tw.right_0, Tw.w_1over2 ] ] ]
            [ Htmls.img
                [ Attr.css
                    [ Tw.h_56
                    , Tw.w_full
                    , Tw.object_cover
                    , TwBp.lg [ Tw.absolute, Tw.h_screen ]
                    ]
                , Attr.src "https://images.unsplash.com/photo-1556761175-4b46a572b786?ixlib=rb-1.2.1&ixqx=g09zpRVLoT&ixid=eyJhcHBfaWQiOjEyMDd9&auto=format&fit=crop&w=1567&q=80"
                , Attr.alt ""
                ]
                []
            ]
        ]


viewFormulario : Model -> Htmls.Html Msg
viewFormulario model =
    let
        viewCampoNombre =
            div
                []
                [ Htmls.label
                    [ Attr.for "first_name"
                    , Attr.css
                        [ Tw.block
                        , Tw.text_sm
                        , Tw.font_medium
                        , Tw.text_gray_700
                        ]
                    ]
                    [ text "Nombre" ]
                , div
                    [ Attr.css [ Tw.mt_1 ] ]
                    [ Htmls.input
                        [ Attr.type_ "text"
                        , Attr.name "first_name"
                        , Attr.id "first_name"
                        , Attr.required True
                        , Attr.minlength 2
                        , Attr.maxlength 15
                        , Attr.autocomplete True -- "given-name"
                        , Attr.css
                            [ Tw.block
                            , Tw.w_full
                            , Tw.shadow_sm
                            , TwBp.sm [ Tw.text_sm ]
                            , Css.focus
                                [ Tw.ring_indigo_500
                                , Tw.border_indigo_500
                                ]
                            , Tw.border_gray_300
                            , Tw.rounded_md
                            ]
                        , Events.onInput Nombre
                        ]
                        []
                    ]
                ]

        viewCampoApellido =
            div []
                [ Htmls.label
                    [ Attr.for "last_name"
                    , Attr.css
                        [ Tw.block
                        , Tw.text_sm
                        , Tw.font_medium
                        , Tw.text_gray_700
                        ]
                    ]
                    [ text "Apellido" ]
                , div
                    [ Attr.css [ Tw.mt_1 ] ]
                    [ Htmls.input
                        [ Attr.type_ "text"
                        , Attr.name "last_name"
                        , Attr.id "last_name"
                        , Attr.autocomplete True -- "family-name"
                        , Attr.css
                            [ Tw.block
                            , Tw.w_full
                            , Tw.shadow_sm
                            , TwBp.sm [ Tw.text_sm ]
                            , Css.focus
                                [ Tw.ring_indigo_500
                                , Tw.border_indigo_500
                                ]
                            , Tw.border_gray_300
                            , Tw.rounded_md
                            ]
                        , Events.onInput Apellido
                        ]
                        []
                    ]
                ]

        viewCampoCorreo =
            div
                [ Attr.css [ TwBp.sm [ Tw.col_span_2 ] ] ]
                [ Htmls.label
                    [ Attr.for "email"
                    , Attr.css
                        [ Tw.block
                        , Tw.text_sm
                        , Tw.font_medium
                        , Tw.text_gray_700
                        ]
                    ]
                    [ text "Correo Electrónico" ]
                , div
                    [ Attr.css [ Tw.mt_1 ] ]
                    [ Htmls.input
                        [ Attr.id "email"
                        , Attr.name "email"
                        , Attr.type_ "email"
                        , Attr.autocomplete True --"email"
                        , Attr.css
                            [ Tw.block
                            , Tw.w_full
                            , Tw.shadow_sm
                            , TwBp.sm [ Tw.text_sm ]
                            , Css.focus
                                [ Tw.ring_indigo_500
                                , Tw.border_indigo_500
                                ]
                            , Tw.border_gray_300
                            , Tw.rounded_md
                            ]
                        , Events.onInput Correo
                        ]
                        []
                    ]
                ]

        viewCampoTelefono =
            div
                [ Attr.css [ TwBp.sm [ Tw.col_span_2 ] ] ]
                [ div
                    [ Attr.css
                        [ Tw.flex
                        , Tw.justify_between
                        ]
                    ]
                    [ Htmls.label
                        [ Attr.for "phone"
                        , Attr.css
                            [ Tw.block
                            , Tw.text_sm
                            , Tw.font_medium
                            , Tw.text_gray_700
                            ]
                        ]
                        [ text "Teléfono" ]
                    , Htmls.span
                        [ Attr.id "phone_description"
                        , Attr.css
                            [ Tw.text_sm
                            , Tw.text_gray_500
                            ]
                        ]
                        [ text "Opcional" ]
                    ]
                , div
                    [ Attr.css [ Tw.mt_1 ] ]
                    [ Htmls.input
                        [ Attr.type_ "text"
                        , Attr.name "phone"
                        , Attr.id "phone"
                        , Attr.minlength 8
                        , Attr.maxlength 15
                        , Attr.value model.telefono
                        , Attr.autocomplete True -- "tel"
                        , Aria.ariaDescribedby "phone_description"
                        , Attr.css
                            [ Tw.block
                            , Tw.w_full
                            , Tw.shadow_sm
                            , TwBp.sm [ Tw.text_sm ]
                            , Css.focus [ Tw.ring_indigo_500, Tw.border_indigo_500 ]
                            , Tw.border_gray_300
                            , Tw.rounded_md
                            ]
                        , Events.onInput Telefono
                        ]
                        []
                    ]
                ]

        viewCampoComment =
            div
                [ Attr.css [ TwBp.sm [ Tw.col_span_2 ] ] ]
                [ div
                    [ Attr.css [ Tw.flex, Tw.justify_between ] ]
                    [ Htmls.label
                        [ Attr.for "how_can_we_help"
                        , Attr.css [ Tw.block, Tw.text_sm, Tw.font_medium, Tw.text_gray_700 ]
                        ]
                        [ text "Comentario" ]
                    , Htmls.span
                        [ Attr.id "how_can_we_help_description"
                        , Attr.css [ Tw.text_sm, Tw.text_gray_500 ]
                        ]
                        [ text ">Max. 500 caracteres" ]
                    ]
                , div
                    [ Attr.css [ Tw.mt_1 ] ]
                    [ Htmls.textarea
                        [ Attr.id "how_can_we_help"
                        , Attr.name "how_can_we_help"
                        , Aria.ariaDescribedby "how_can_we_help_description"
                        , Attr.rows 4
                        , Attr.css
                            [ Tw.block
                            , Tw.w_full
                            , Tw.shadow_sm
                            , TwBp.sm [ Tw.text_sm ]
                            , Css.focus [ Tw.ring_indigo_500, Tw.border_indigo_500 ]
                            , Tw.border_gray_300
                            , Tw.rounded_md
                            ]
                        , Events.onInput Comentario
                        ]
                        []
                    ]
                ]

        viewComoSupoDeNos =
            div
                [ Attr.css [ TwBp.sm [ Tw.col_span_2 ] ] ]
                [ Htmls.label
                    [ Attr.for "how_did_you_hear_about_us"
                    , Attr.css [ Tw.block, Tw.text_sm, Tw.font_medium, Tw.text_gray_700 ]
                    ]
                    [ text "¿Cómo llegó con nosotros?" ]
                , div
                    [ Attr.css [ Tw.mt_1 ] ]
                    [ Htmls.input
                        [ Attr.type_ "text"
                        , Attr.name "how_did_you_hear_about_us"
                        , Attr.id "how_did_you_hear_about_us"
                        , Attr.css
                            [ Tw.shadow_sm
                            , Css.focus [ Tw.ring_indigo_500, Tw.border_indigo_500 ]
                            , Tw.block
                            , Tw.w_full
                            , TwBp.sm [ Tw.text_sm ]
                            , Tw.border_gray_300
                            , Tw.rounded_md
                            ]
                        , Events.onInput ComoSupo
                        ]
                        []
                    ]
                ]

        viewBotonSubmit =
            div
                [ Attr.css [ Tw.text_right, TwBp.sm [ Tw.col_span_2 ] ] ]
                [ Htmls.button
                    [ Attr.type_ "submit"
                    , Attr.css
                        [ Tw.inline_flex
                        , Tw.justify_center
                        , Tw.py_2
                        , Tw.px_4
                        , Tw.border
                        , Tw.border_transparent
                        , Tw.shadow_sm
                        , Tw.text_sm
                        , Tw.font_medium
                        , Tw.rounded_md
                        , Tw.text_white
                        , Tw.bg_indigo_600
                        , Css.hover [ Tw.bg_indigo_700 ]
                        , Css.focus [ Tw.outline_none, Tw.ring_2, Tw.ring_offset_2, Tw.ring_indigo_500 ]
                        ]
                    ]
                    [ text "Enviar" ]
                ]
    in
    div
        [ Attr.css
            [ Tw.relative
            , Tw.py_8
            , Tw.px_4
            , TwBp.sm [ Tw.px_6 ]
            , TwBp.lg
                [ Tw.px_8
                , Tw.max_w_7xl
                , Tw.mx_auto
                , Tw.grid
                , Tw.grid_cols_2
                , Tw.py_8
                ]
            ]
        ]
        [ div
            [ Attr.css [ TwBp.lg [ Tw.pr_8 ] ] ]
            [ div
                [ Attr.css
                    [ Tw.max_w_md
                    , Tw.mx_auto
                    , TwBp.lg [ Tw.mx_0 ]
                    , TwBp.sm [ Tw.max_w_lg ]
                    ]
                ]
                [ Htmls.h2
                    [ Attr.css
                        [ Tw.text_3xl
                        , Tw.font_extrabold
                        , Tw.tracking_tight
                        , TwBp.sm [ Tw.text_4xl ]
                        ]
                    , Attr.class "font-serif" ]
                    [ text "¿Cómo Podemos Ayudar?" ]
                , Htmls.p
                    [ Attr.css
                        [ Tw.mt_4
                        , Tw.text_lg
                        , Tw.text_gray_500
                        , TwBp.sm [ Tw.mt_3 ]
                        ]
                    ]
                    [ text "Responderemos tan pronto sea posible con un correo electrónico o con un mensaje a su teléfono. Gracias." ]
                , Htmls.form
                    [ Attr.action "#"
                    , Attr.method "POST"
                    , Events.onSubmit Enviado
                    , Attr.css
                        [ Tw.mt_9
                        , Tw.grid
                        , Tw.grid_cols_1
                        , Tw.gap_y_6
                        , TwBp.sm [ Tw.grid_cols_2, Tw.gap_x_8 ]
                        ]
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
