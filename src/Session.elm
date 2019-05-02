module Session exposing (Session(..), init, navKey)

import Browser.Navigation as Nav



-- TYPE


type Session
    = All Nav.Key



-- INIT


init : Nav.Key -> Session
init key =
    All key


navKey : Session -> Nav.Key
navKey session =
    case session of
        All key ->
            key
