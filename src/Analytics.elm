module Analytics exposing
    ( Event
    , name
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


name : String -> Event
name str =
    Event str


none : Event
none =
    None


type Msg
    = Notificado (Result Http.Error ())


toCmd : Event -> Cmd Msg
toCmd event =
    case event of
        Event cualEvento ->
            Http.get
                { url =
                    "https://h8uqc13y50.execute-api.us-east-2.amazonaws.com/msg/"
                        ++ cualEvento
                , expect = Http.expectWhatever Notificado
                }

        None ->
            Cmd.none
