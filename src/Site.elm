module Site exposing (config)

import DataSource
import Head
import LanguageTag
import LanguageTag.Language
import MimeType
import Pages.Manifest as Manifest
import Pages.Url
import Route
import SiteConfig exposing (SiteConfig)


type alias Data =
    ()


config : SiteConfig Data
config =
    { data = data
    , canonicalUrl = "https://psolar.mx"
    , manifest = manifest
    , head = head
    }


data : DataSource.DataSource Data
data =
    DataSource.succeed ()


head : Data -> List Head.Tag
head static =
    [ Head.icon [ ( 32, 32 ) ] MimeType.Png (Pages.Url.external "/favicon-32x32.png")
    , Head.icon [ ( 16, 16 ) ] MimeType.Png (Pages.Url.external "/favicon-16x16.png")
    , Head.appleTouchIcon (Just 180) (Pages.Url.external "/apple-touch-icon.png")
    , Head.rssLink "/blog/feed.xml"
    , Head.sitemapLink "/sitemap.xml"
    , LanguageTag.Language.es
        |> LanguageTag.build LanguageTag.emptySubtags
        |> Head.rootLanguage
    ]


manifest : Data -> Manifest.Config
manifest static =
    Manifest.init
        { name = "PSOLAR"
        , description = "Servicios de Revisión, Mantenimiento e Ampliación de Proyoectos de Panel Solar."
        , startUrl = Route.Index |> Route.toPath
        , icons =
            [ icon MimeType.Png 192 (Pages.Url.external "./android-chrome-192x192.png")
            , icon MimeType.Png 512 (Pages.Url.external "./android-chrome-512x512.png")
            ]
        }
        |> Manifest.withShortName "psolar"


webp : MimeType.MimeImage
webp =
    MimeType.OtherImage "webp"


icon : MimeType.MimeImage -> Int -> Pages.Url.Url -> Manifest.Icon
icon format width fuente =
    { src = fuente
    , sizes = [ ( width, width ) ]
    , mimeType = format |> Just
    , purposes = [ Manifest.IconPurposeAny, Manifest.IconPurposeMaskable ]
    }
