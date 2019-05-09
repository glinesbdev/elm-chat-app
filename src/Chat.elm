port module Chat exposing (Model, Msg, init, subscriptions, toSession, update, view)

import Aliases exposing (..)
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
    , pendingMessage : PendingMessage
    , room : Room
    , user : User
    , timeSent : TimeSent
    , errors : Errors
    }


type alias User =
    { id : UserId
    , name : UserName
    }


type alias Message =
    { id : Id
    , body : MessageBody
    , user : User
    , timeSent : TimeSent
    , room : Room
    }


type alias InitialState =
    { messages : List Message
    , userId : UserId
    }


type alias RoomState =
    { room : Room
    , user : User
    }



-- INIT


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( initialModel session
    , chatPageLoaded { room = initialRoom session, user = initialUser session }
    )


initialModel : Session.Session -> Model
initialModel session =
    { session = session
    , messages = []
    , pendingMessage = ""
    , user = initialUser session
    , room = Room <| Session.roomName session
    , timeSent = 0
    , errors = []
    }


initialUser : Session.Session -> User
initialUser session =
    User (Session.userId session) (Session.userName session)


initialRoom : Session.Session -> Room
initialRoom session =
    Room (Session.roomName session)



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Chat Room"
    , content =
        div [ Attr.class "flex flex-col h-screen" ]
            [ h1 [ Attr.class "text-center mt-8" ] [ text "Chat Room" ]
            , disclaimer
            , viewErrors model.errors
            , h4 [ Attr.class "pl-4 mt-12 mb-4" ] [ text <| "Room: " ++ model.room.name ]
            , messageBox model
            , messageInput model
            ]
    }


disclaimer : Html Msg
disclaimer =
    div []
        [ p [ Attr.class "text-center text-grey-darker mb-0 mt-2" ] [ text "Thanks for trying out this chat application." ]
        , p [ Attr.class "text-center text-grey-darker" ] [ text "Please keep in mind that all messages and users are NOT permanent as this application is still in active development." ]
        , p [ Attr.class "text-center text-grey-darker" ]
            [ text "Please issue any pull requests or bug reports on the "
            , a
                [ Attr.href "https://github.com/glinesbdev/elm-chat-app"
                , Attr.class "inline-block"
                , Attr.target "_blank"
                ]
                [ text " github project page" ]
            ]
        ]


messageBox : Model -> Html Msg
messageBox model =
    List.sortBy .timeSent model.messages
        |> List.map (\message -> displayedMessage model message)
        |> div [ Attr.class "bg-white px-2 -mb-12 overflow-y-scroll flex-1 mx-4 message-box" ]


displayedMessage : Model -> Message -> Html Msg
displayedMessage model message =
    div
        [ Attr.classList
            [ ( "mt-4", True )
            , ( "text-right", not <| message.user.id == model.user.id )
            ]
        ]
        [ span [ Attr.class "font-bold" ] [ text (isHacker <| message.user.name) ]
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


viewErrors : Errors -> Html Msg
viewErrors errors =
    if List.length errors > 0 then
        List.map (\e -> li [] [ text e ]) errors
            |> ul [ Attr.class " text-center text-grey-lightest errors p-8 bg-red-light mt-4" ]

    else
        text ""


buttonSvg : String
buttonSvg =
    "data:image/svg+xml;utf8,<svg aria-hidden=\"true\" focusable=\"false\" data-prefix=\"fas\" data-icon=\"paper-plane\" role=\"img\" xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 512 512\"><path fill=\"rgb(96,111,123)\" d=\"M476 3.2L12.5 270.6c-18.1 10.4-15.8 35.6 2.2 43.2L121 358.4l287.3-253.2c5.5-4.9 13.3 2.6 8.6 8.3L176 407v80.5c0 23.6 28.5 32.9 42.5 15.8L282 426l124.6 52.2c14.2 6 30.4-2.9 33-18.2l72-432C515 7.8 493.3-6.8 476 3.2z\"></path></svg>"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ initialize (\value -> Initialize value)
        , getMessage (\message -> GotMessage message)
        , getUserId (\id -> GotUserId id)
        ]



-- PORTS
-- OUTGOING


port chatPageLoaded : RoomState -> Cmd msg


port storeMessage : E.Value -> Cmd msg



-- INCOMING


port initialize : (E.Value -> msg) -> Sub msg


port getMessage : (E.Value -> msg) -> Sub msg


port getUserId : (UserId -> msg) -> Sub msg



-- UPDATE


type Msg
    = Initialize E.Value
    | EnterMessage String
    | SubmitMessage
    | GotMessage E.Value
    | GotUserId UserId
    | GenerateRandomId RandomId
    | GotAllMessages (List Message)
    | GotTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Initialize state ->
            let
                stateVal =
                    D.decodeValue initialStateDecoder state
            in
            case stateVal of
                Ok value ->
                    ( { model | messages = value.messages, user = setUserId value.userId model.user }
                    , Cmd.none
                    )

                Err error ->
                    case error of
                        D.Field field err ->
                            let
                                detailedError =
                                    "Error with field '" ++ field ++ "': " ++ D.errorToString err
                            in
                            ( { model | errors = detailedError :: model.errors }
                            , Cmd.none
                            )

                        D.Failure err _ ->
                            ( { model | errors = err :: model.errors }
                            , Cmd.none
                            )

                        _ ->
                            let
                                genericError =
                                    "Something went wrong setting up the chat room"
                            in
                            ( { model | errors = genericError :: model.errors }
                            , Cmd.none
                            )

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
            ( { model | pendingMessage = "" }
            , { pendingMessage = model.pendingMessage, user = model.user, timeSent = model.timeSent, room = model.room }
                |> prepareMessage id
                |> messageEncoder
                |> storeMessage
            )

        GotUserId id ->
            ( { model | user = setUserId id model.user }
            , Cmd.none
            )

        GotMessage message ->
            let
                messageVal =
                    D.decodeValue messageDecoder message
            in
            case messageVal of
                Ok value ->
                    ( { model | messages = value :: model.messages }
                    , Cmd.none
                    )

                Err error ->
                    case error of
                        _ ->
                            ( { model | errors = "Couldn't display message. Please submit a bug report with the link above. If a bug report already exists, please check back again." :: model.errors }
                            , Cmd.none
                            )

        GotAllMessages messages ->
            ( { model | messages = messages }
            , Cmd.none
            )



-- HELPERS


prepareMessage :
    RandomId
    -> { pendingMessage : PendingMessage, user : User, timeSent : TimeSent, room : Room }
    -> Message
prepareMessage randId msgDetails =
    { id = randId
    , body = String.trim msgDetails.pendingMessage
    , user = msgDetails.user
    , timeSent = msgDetails.timeSent
    , room = msgDetails.room
    }


initialStateEncoder : InitialState -> E.Value
initialStateEncoder state =
    E.object
        [ ( "messages", E.list messageEncoder state.messages )
        , ( "userId", E.string state.userId )
        ]


initialStateDecoder : D.Decoder InitialState
initialStateDecoder =
    D.map2 InitialState
        (D.field "messages" <| D.list messageDecoder)
        (D.field "userId" D.string)


messageEncoder : Message -> E.Value
messageEncoder message =
    E.object
        [ ( "id", E.string message.id )
        , ( "body", E.string message.body )
        , ( "user", userEncoder message.user )
        , ( "timeSent", E.int message.timeSent )
        , ( "room", roomEncoder message.room )
        ]


messageDecoder : D.Decoder Message
messageDecoder =
    D.map5 Message
        (D.field "id" D.string)
        (D.field "body" D.string)
        (D.field "user" userDecoder)
        (D.field "timeSent" D.int)
        (D.field "room" roomDecoder)


userDecoder : D.Decoder User
userDecoder =
    D.map2 User
        (D.field "id" D.string)
        (D.field "name" D.string)


userEncoder : User -> E.Value
userEncoder user =
    E.object
        [ ( "id", E.string user.id )
        , ( "name", E.string user.name )
        ]


roomDecoder : D.Decoder Room
roomDecoder =
    D.map Room
        (D.field "name" D.string)


roomEncoder : Room -> E.Value
roomEncoder room =
    E.object
        [ ( "name", E.string room.name ) ]


setUserId : UserId -> User -> User
setUserId id user =
    { user | id = id }


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
