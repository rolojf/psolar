module Page.Sub.Detail_ exposing (Data, Model, Msg, page)

import Browser.Navigation
import Cloudinary
import DataSource exposing (DataSource)
import DataSource.File as File
import DataSource.Glob as Glob
import Head
import Head.Seo as Seo
import HeroIcons
import Html as Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Events as Event
import Markdown.Block
import MdConverter
import MenuDecoder
import OptimizedDecoder as Decode exposing (Decoder)
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


type alias Msg =
    ()


update : PageUrl -> Maybe Browser.Navigation.Key -> Shared.Model -> StaticPayload templateData routeParams -> Msg -> Model -> ( Model, Cmd Msg, Maybe Shared.Msg )
update url maybeKey sharedM staticP msg model =
    ( model
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
    let
        allMDFiles : DataSource (List MDFile)
        allMDFiles =
            Glob.succeed MDFile
                |> Glob.match (Glob.literal "data/")
                |> Glob.capture Glob.wildcard
                |> Glob.match (Glob.literal ".md")
                |> Glob.captureFilePath
                |> Glob.toDataSource
    in
    allMDFiles
        |> DataSource.map
            (List.map
                (\cadaMD -> RouteParams cadaMD.slug)
            )


type alias MDFile =
    { slug : String
    , filePath : String
    }


type alias Data =
    { delMD : ContenidoConDatos }


type alias ContenidoConDatos =
    { body : Result String (List Markdown.Block.Block)
    , title : String
    , menu : View.MenuInfo
    }


data : RouteParams -> DataSource Data
data routeParams =
    let
        miDecoder : String -> Decoder ContenidoConDatos
        miDecoder elCuerpo =
            Decode.map3 ContenidoConDatos
                (elCuerpo
                    |> MdConverter.parsea
                    |> Decode.succeed
                )
                (Decode.field "title" Decode.string)
                (MenuDecoder.opMenuToDecode
                    { mainHero = div [] []
                    , afterHero = div [] []
                    }
                )

        getDataFromMD =
            File.bodyWithFrontmatter
                miDecoder
            <|
                "data/"
                    ++ routeParams.detail
                    ++ ".md"
    in
    DataSource.map Data
        getDataFromMD


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


view : Maybe PageUrl -> Shared.Model -> Model -> StaticPayload Data RouteParams -> View Msg
view maybeUrl sharedModel model static =
    { title = static.data.delMD.title
    , body =
        [ Html.div
            [ class "tw prose prose-sm ml-3 md:prose-base md:max-w-md md:mx-auto lg:prose-lg lg:max-w-lg" ]
            (MdConverter.renderea static.data.delMD.body)
        ]
    , withMenu =
        static.data.delMD.menu
    }


viewHeroMain =
    div [] []


viewHeroAfter =
    div [] []
