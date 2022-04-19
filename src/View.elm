module View exposing (Liga, LigaTipo(..), MenuComplemento, MenuInfo(..), View, map, placeholder)

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
    | SiMenu (List Liga) MenuComplemento


type alias MenuComplemento =
    { mainHero : Html ()
    , afterHero : Html ()
    }


type alias Liga =
    { dir : LigaTipo
    , queDice : String
    , especial : Bool
    }


type LigaTipo
    = Otra Path
    | Interna Route


map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn doc =
    { title = doc.title
    , body = List.map (Html.map fn) doc.body
    , withMenu =
        case doc.withMenu of
            NoMenu ->
                NoMenu

            SiMenu ligas complementosAlMenu ->
                SiMenu
                    ligas
                    { mainHero = complementosAlMenu.mainHero
                    , afterHero = complementosAlMenu.afterHero
                    }
    }


placeholder : String -> View msg
placeholder moduleName =
    { title = "Placeholder - " ++ moduleName
    , body = [ Html.text moduleName ]
    , withMenu = NoMenu
    }
