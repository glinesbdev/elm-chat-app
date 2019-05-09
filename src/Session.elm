module Session exposing (Session(..), init, navKey, roomName, userId, userName)

import Aliases exposing (..)
import Browser.Navigation as Nav



-- TYPE


type Session
    = Guest Nav.Key
    | LoggedIn (Maybe RoomName) RandomId UserName Nav.Key



-- INIT


init : Nav.Key -> Session
init key =
    Guest key


navKey : Session -> Nav.Key
navKey session =
    case session of
        Guest key ->
            key

        LoggedIn _ _ _ key ->
            key


userName : Session -> UserName
userName session =
    case session of
        Guest _ ->
            ""

        LoggedIn _ _ name _ ->
            name


userId : Session -> RandomId
userId session =
    case session of
        Guest _ ->
            ""

        LoggedIn _ id _ _ ->
            id


roomName : Session -> RoomName
roomName session =
    case session of
        Guest _ ->
            "public"

        LoggedIn name _ _ _ ->
            case name of
                Nothing ->
                    "public"

                Just realName ->
                    realName
