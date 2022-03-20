module Analytics exposing
    ( Event
    , eventoXReportar
    , none
    , toCmd
    )

import Http



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
            Http.get
                { url =
                    --"https://h8uqc13y50.execute-api.us-east-2.amazonaws.com/msg/"
                    "https://psolar.mx/api-v1/msg/"
                        ++ cualEvento
                , expect = Http.expectWhatever msg
                }

        None ->
            Cmd.none
