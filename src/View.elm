module View exposing (View, map, placeholder)

import Html as Html exposing (Html)
import Route


type alias View msg =
    { title : String
    , body : List (Html msg)
    , withMenu : Maybe (List Liga)
    }

type alias Liga =
    { direccion : Route.Route
    , queDice : String
    }


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
    , withMenu = Nothing
    }
