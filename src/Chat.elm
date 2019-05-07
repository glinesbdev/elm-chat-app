port module Chat exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Event
import Json.Decode as D
import Json.Encode as E
import Random
import Random.Char as RandomChar
import Random.String as RandomString
import Route
import Session
import Task
import Time



-- MODEL


type alias Model =
    { session : Session.Session
    , messages : List Message
    , pendingMessage : String
    , room : Room
    , userId : String
    , chatName : String
    , timeSent : Int
    }


type alias Message =
    { id : String
    , userId : String
    , body : String
    , roomId : String
    , chatName : String
    , timeSent : Int
    }


type alias Room =
    { id : String, name : String, userIds : List String }


type alias InitRoomChat =
    { roomName : String, chatName : String }



-- INIT


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( initialModel session, chatPageLoaded { roomName = roomName session, chatName = Session.chatName session } )


initialModel : Session.Session -> Model
initialModel session =
    { session = session
    , messages = []
    , pendingMessage = ""
    , userId = ""
    , chatName = Session.chatName session
    , room = Room "IIVY2snGny1UkBvgGqQB" "" []
    , timeSent = 0
    }



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Chat Room"
    , content =
        div [ Attr.class "flex flex-col h-screen" ]
            [ h1 [ Attr.class "text-center mt-8" ] [ text "Chat Room" ]
            , messageBox model
            , messageInput model
            ]
    }


messageBox : Model -> Html Msg
messageBox model =
    List.sortBy .timeSent model.messages
        |> List.map (\message -> displayedMessage model message)
        |> div [ Attr.class "bg-white px-2 -mb-12 overflow-y-scroll flex-1 mt-12 mx-4 message-box" ]


displayedMessage : Model -> Message -> Html Msg
displayedMessage model message =
    div
        [ Attr.classList
            [ ( "mt-4", True )
            , ( "text-right", not <| message.userId == model.userId )
            ]
        ]
        [ span [ Attr.class "font-bold" ] [ text (isHacker <| message.chatName) ]
        , p [ Attr.class "mt-0" ] [ text message.body ]
        ]


messageInput : Model -> Html Msg
messageInput model =
    form [ Event.onSubmit SubmitMessage, Attr.class "flex my-20" ]
        [ input
            [ Event.onInput EnterMessage
            , Attr.value model.pendingMessage
            , Attr.autofocus True
            , Attr.class "mx-4 flex-1 p-4 text-lg"
            ]
            []

        -- This is type_ "button" so the form isn't submitted twice
        , button
            [ Event.onClick SubmitMessage
            , Attr.type_ "button"
            , Attr.class "bg-green-lighter mr-4 p-4"
            ]
            [ img [ Attr.src buttonSvg, Attr.class "w-4" ] [] ]
        ]


buttonSvg : String
buttonSvg =
    "data:image/svg+xml;utf8,<svg aria-hidden=\"true\" focusable=\"false\" data-prefix=\"fas\" data-icon=\"paper-plane\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 512 512\"><path fill=\"rgb(96,111,123)\" d=\"M476 3.2L12.5 270.6c-18.1 10.4-15.8 35.6 2.2 43.2L121 358.4l287.3-253.2c5.5-4.9 13.3 2.6 8.6 8.3L176 407v80.5c0 23.6 28.5 32.9 42.5 15.8L282 426l124.6 52.2c14.2 6 30.4-2.9 33-18.2l72-432C515 7.8 493.3-6.8 476 3.2z\"></path></svg>"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ getMessage (\message -> GotMessage message)
        , getUserId (\userId -> GotUserId userId)
        , getAllMessages (\messages -> GotAllMessages messages)
        ]



-- PORTS
-- OUTGOING


port chatPageLoaded : InitRoomChat -> Cmd msg


port storeMessage : E.Value -> Cmd msg



-- INCOMING


port getMessage : (Message -> msg) -> Sub msg


port getAllMessages : (List Message -> msg) -> Sub msg


port getUserId : (String -> msg) -> Sub msg



-- UPDATE


type Msg
    = EnterMessage String
    | SubmitMessage
    | GotMessage Message
    | GotUserId String
    | GenerateRandomId String
    | GotAllMessages (List Message)
    | GotTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnterMessage message ->
            ( { model | pendingMessage = message }
            , Cmd.none
            )

        SubmitMessage ->
            ( model
            , getTime
            )

        GotTime time ->
            ( { model | timeSent = Time.posixToMillis time }
            , generateRandomId
            )

        GenerateRandomId id ->
            let
                message =
                    Message
                        id
                        model.userId
                        (String.trim model.pendingMessage)
                        model.room.id
                        (Session.chatName model.session)
                        model.timeSent
            in
            ( { model | pendingMessage = "" }
            , storeMessage (messageEncoder message)
            )

        GotUserId id ->
            ( { model | userId = id }
            , Cmd.none
            )

        GotMessage message ->
            ( { model | messages = message :: model.messages }
            , Cmd.none
            )

        GotAllMessages messages ->
            ( { model | messages = messages }
            , Cmd.none
            )



-- HELPERS


messageEncoder : Message -> E.Value
messageEncoder message =
    E.object
        [ ( "id", E.string message.id )
        , ( "userId", E.string message.userId )
        , ( "roomId", E.string message.roomId )
        , ( "chatName", E.string message.chatName )
        , ( "body", E.string message.body )
        , ( "timeSent", E.int message.timeSent )
        ]


chatName : List Message -> String
chatName messages =
    case List.head messages of
        Nothing ->
            ""

        Just message ->
            message.chatName


roomName : Session.Session -> String
roomName session =
    Maybe.withDefault "default" (Session.roomName session)


isHacker : String -> String
isHacker name =
    if List.any (\x -> String.contains x name) [ "<", ">" ] then
        "HAXXOR"

    else
        name


generateRandomId : Cmd Msg
generateRandomId =
    Random.generate GenerateRandomId <| RandomString.string 20 RandomChar.latin


getTime : Cmd Msg
getTime =
    Task.perform GotTime Time.now



-- EXPORT


toSession : Model -> Session.Session
toSession model =
    model.session
