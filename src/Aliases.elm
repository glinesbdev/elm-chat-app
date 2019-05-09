module Aliases exposing (Errors, Id, MessageBody, MessageId, PendingMessage, RandomId, Room, RoomId, RoomName, TimeSent, UserId, UserName)


type alias Room =
    { name : RoomName
    }


type alias Errors =
    List String


type alias Id =
    String


type alias PendingMessage =
    String


type alias RandomId =
    String


type alias RoomId =
    String


type alias RoomName =
    String


type alias TimeSent =
    Int


type alias MessageId =
    String


type alias MessageBody =
    String


type alias UserId =
    String


type alias UserName =
    String
