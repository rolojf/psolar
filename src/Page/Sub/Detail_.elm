module Page.Sub.Detail_ exposing (Data, Model, Msg, page)

import Browser.Navigation
import Cloudinary
import DataSource exposing (DataSource)
import Head
import Head.Seo as Seo
import HeroIcons
import Html as Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Events as Event
import Page exposing (Page, PageWithState, StaticPayload)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import Path exposing (Path)
import Route exposing (Route)
import Shared
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Svg exposing (path, svg)
import Svg.Attributes as SvgAttr
import View exposing (View)


type alias Model =
    { menuOpen : Bool }


init : Maybe PageUrl -> Shared.Model -> StaticPayload templateData routeParams -> ( Model, Cmd Msg )
init _ _ _ =
    ( { menuOpen = False }
    , Cmd.none
    )


subscriptions : Maybe PageUrl -> routeParams -> Path.Path -> Model -> Shared.Model -> Sub Msg
subscriptions _ _ _ _ _ =
    Sub.none


type Msg
    = ToggleMenu


update : PageUrl -> Maybe Browser.Navigation.Key -> Shared.Model -> StaticPayload templateData routeParams -> Msg -> Model -> ( Model, Cmd Msg, Maybe Shared.Msg )
update url maybeKey sharedM staticP msg model =
    case msg of
        ToggleMenu ->
            ( { model | menuOpen = not model.menuOpen }
            , Cmd.none
            , Nothing
            )


type alias RouteParams =
    { detail : String }


page : Page.PageWithState RouteParams Data Model Msg
page =
    Page.prerender
        { head = head
        , data = data
        , routes = routes
        }
        |> Page.buildWithSharedState
            { view = view
            , init = init
            , update = update
            , subscriptions = subscriptions
            }


routes : DataSource (List RouteParams)
routes =
    RouteParams "hola"
        |> List.singleton
        |> DataSource.succeed


data : RouteParams -> DataSource Data
data routeParams =
    DataSource.succeed ()


head : StaticPayload Data RouteParams -> List Head.Tag
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


view : Maybe PageUrl -> Shared.Model -> Model -> StaticPayload Data RouteParams -> View Msg
view maybeUrl sharedModel model static =
    { title = "unoDosTres"
    , body = [ div [] [ text "Pendiente por jalar info del markdown" ] ]
    , withMenu = View.SiMenu ligas { mainHero = viewHeroMain, afterHero = viewHeroAfter }
    }


ligas : List View.Liga
ligas =
    [ { queDice = "Comunícate"
      , dir = View.Interna Route.Contacto
      , especial = False
      }
    , { queDice = "Uno"
      , dir =
            "#"
                |> Path.fromString
                |> View.Otra
      , especial = False
      }
    , { queDice = "Dos"
      , dir =
            "#"
                |> Path.fromString
                |> View.Otra
      , especial = False
      }
    , { queDice = "Tres"
      , dir =
            "#"
                |> Path.fromString
                |> View.Otra
      , especial = False
      }
    , { queDice = "Regres al Inicio"
      , dir = View.Interna Route.Index
      , especial = True
      }
    ]


viewHeroMain =
    div [] []


viewHeroAfter =
    div [] []
