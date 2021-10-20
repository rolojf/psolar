module Page.Contacto exposing (Data, Model, Msg, page)

import Browser.Navigation
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import Html exposing (Html)
import Html.Styled as Htmls exposing (div)
import Html.Styled.Attributes as Attr exposing (class)
import Html.Styled.Events as Events
import Json.Decode as Decode
import Page exposing (Page, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Shared
import View exposing (View)
import Path


type alias Model =
    { usuarioStatus : Bool
    , nombre : String
    , comoSupo : String
    , correo : String
    , comentario : String
    , telefono : String
    , apellido : String
    , listo : Bool
    }


type Msg
    = Nombre String
    | ComoSupo String
    | Correo String
    | Apellido String
    | Telefono String
    | Comentario String
    | Enviado
    | Respondio Bool


init :
    Maybe PageUrl
    -> Shared.Model
    -> StaticPayload templateData routeParams
    -> ( Model, Cmd Msg )
init _ _ _ =
    ( { usuarioStatus = True
      , nombre = ""
      , comoSupo = ""
      , correo = ""
      , comentario = ""
      , telefono = ""
      , apellido = ""
      , listo = False
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
            ( { model | telefono = String.dropRight 1 conQue ++ conQue1 }, Cmd.none, Nothing )

        Comentario cCampo ->
            ( { model | comentario = cCampo }, Cmd.none, Nothing )

        Enviado ->
            ( { model | listo = True }
            , Cmd.none
            , Nothing
            )

        Respondio conQue ->
            ( { model
                | usuarioStatus =
                    if conQue then
                        True

                    else
                        False
              }
            , Cmd.none
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

view : Maybe PageUrl
          -> Shared.Model
          -> Model
          -> StaticPayload Data RouteParams
          -> View Msg
view maybeUrl sharedModel model static =
    View.placeholder "Contacto"
