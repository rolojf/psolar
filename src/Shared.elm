module Shared exposing (Data, Model, Msg(..), SharedMsg(..), UsuarioSt(..), template)

import Browser.Navigation
import DataSource
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import Path exposing (Path)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import Svg.Styled as Svg exposing (path, svg)
import Svg.Styled.Attributes as AttrSvg
import Tailwind.Breakpoints as TwBp
import Tailwind.Utilities as Tw
import View exposing (View)
import Http


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


type alias Model =
    { showMobileMenu : Bool
    , usuarioStatus : UsuarioSt
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
        Html.div
            []
            pageView.body
    , title = pageView.title
    }
