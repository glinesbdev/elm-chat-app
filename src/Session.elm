module Session exposing (Session(..), chatName, init, navKey, roomName)

import Browser.Navigation as Nav



-- TYPE


type Session
    = Guest Nav.Key
    | LoggedIn (Maybe RoomName) ChatName Nav.Key



-- type alias ChatId =
--     String


type alias ChatName =
    String


type alias RoomName =
    String



-- INIT


init : Nav.Key -> Session
init key =
    Guest key


navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest key ->
            key

        LoggedIn _ _ key ->
            key


chatName : Session -> ChatName
chatName session =
    case session of
        Guest _ ->
            ""

        LoggedIn _ name _ ->
            name


roomName : Session -> Maybe RoomName
roomName session =
    case session of
        Guest _ ->
            Nothing

        LoggedIn id _ _ ->
            case id of
                Nothing ->
                    Just "default"

                Just realId ->
                    Just realId
