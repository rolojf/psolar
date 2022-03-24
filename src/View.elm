module View exposing (View, map, placeholder, Liga, LigaTipo(..), MenuInfo(..), MenuComplemento)

import Html as Html exposing (Html)
import Path exposing (Path)
import Route exposing (Route)


type alias View msg =
    { title : String
    , body : List (Html msg)
    , withMenu : MenuInfo
    }

type MenuInfo
   = NoMenu
   | SiMenu (List Liga) (MenuComplemento Never)


type alias MenuComplemento msg =
    { mainHero : Html msg
    , afterHero : Html msg

    }


type alias Liga =
    { direccion : LigaTipo
    , queDice : String
    }

type LigaTipo
    = Externa Path
    | Interna Route

map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn doc =
    { title = doc.title
    , body = List.map (Html.map fn) doc.body
    , withMenu = doc.withMenu
    }


placeholder : String -> View msg
placeholder moduleName =
    { title = "Placeholder - " ++ moduleName
    , body = [ Html.text moduleName ]
    , withMenu = NoMenu
    }
