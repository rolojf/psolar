module Analytics exposing
    ( Event
    , eventoXReportar
    , none
    , toCmd
    )

import Http
import Path



-------------------------------------------------------------------------------
-- TYPES --
-------------------------------------------------------------------------------


type Event
    = Event String
    | None



-------------------------------------------------------------------------------
-- API --
-------------------------------------------------------------------------------


eventoXReportar : String -> Event
eventoXReportar str =
    Event str


none : Event
none =
    None


toCmd : Event -> (Result Http.Error () -> msg) -> Cmd msg
toCmd event msg =
    case event of
        Event cualEvento ->
            let
                direccion =
                    [ "api-v2", cualEvento ++ ".json" ]
                        |> Path.join
                        |> Path.toAbsolute
            in
            Http.get
                { url = direccion
                , expect = Http.expectWhatever msg
                }

        None ->
            Cmd.none
