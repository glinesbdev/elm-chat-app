module Session exposing (Session(..), chatId, chatName, init, navKey, sessionType)

import Browser.Navigation as Nav



-- TYPE


type Session
    = Guest Nav.Key
    | LoggedIn (Maybe ChatId) ChatName Nav.Key


type alias ChatId =
    String


type alias ChatName =
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


chatName : Session -> Maybe ChatName
chatName session =
    case session of
        Guest _ ->
            Nothing

        LoggedIn _ name _ ->
            Just name


chatId : Session -> Maybe ChatId
chatId session =
    case session of
        Guest _ ->
            Nothing

        LoggedIn id _ _ ->
            case id of
                Nothing ->
                    Nothing

                Just realId ->
                    Just realId


sessionType : Session -> String
sessionType session =
    case session of
        Guest _ ->
            "Guest"

        LoggedIn _ _ _ ->
            "Logged In"
