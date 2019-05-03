module Chat exposing (Model, Msg, init, toSession, update, view)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Event
import Random
import Random.Char as RandomChar
import Random.String as RandomString
import Session



-- MODEL


type alias Model =
    { session : Session.Session
    , messages : List String
    , pendingMessage : String
    , id : String
    }



-- INIT


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( Model session [] "" ""
    , generateRandomChatId
    )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Chat Room"
    , content =
        div []
            [ h1 [ Attr.class "chat-heading" ] [ text "Chat Room" ]
            , messageBox model
            , messageInput model
            ]
    }


messageBox : Model -> Html Msg
messageBox model =
    div [ Attr.class "message-box" ] <|
        List.map (\m -> displayedMessage model m) model.messages


displayedMessage : Model -> String -> Html Msg
displayedMessage model message =
    div
        [ Attr.classList
            [ ( "message", True )
            , ( "text-right", not <| model.id == chatId model.session )
            ]
        ]
        [ span [] [ text (chatName model.session) ]
        , p
            [ Attr.classList
                [ ( "mt0", True ), ( "text-light", True ) ]
            ]
            [ text message ]
        ]


chatName : Session.Session -> String
chatName session =
    Maybe.withDefault "" (Session.chatName session)


chatId : Session.Session -> String
chatId session =
    Maybe.withDefault "" (Session.chatId session)


messageInput : Model -> Html Msg
messageInput model =
    form [ Event.onSubmit SubmitMessage, Attr.class "message-input" ]
        [ input
            [ Event.onInput EnterMessage
            , Attr.value model.pendingMessage
            , Attr.autofocus True
            , Attr.class "message-input-box"
            ]
            []

        -- This is type_ "button" so the form isn't submitted twice
        , button
            [ Event.onClick SubmitMessage
            , Attr.type_ "button"
            , Attr.class "message-submit"
            ]
            [ text "Send" ]
        ]



-- UPDATE


type Msg
    = EnterMessage String
    | SubmitMessage
    | GenerateRandomId String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnterMessage message ->
            ( { model | pendingMessage = message }
            , Cmd.none
            )

        SubmitMessage ->
            ( { model
                | messages = model.pendingMessage :: model.messages
                , pendingMessage = ""
              }
            , Cmd.none
            )

        GenerateRandomId id ->
            ( { model
                | session = Session.LoggedIn (Just id) (chatName model.session) (Session.navKey model.session)
                , id = id
              }
            , Cmd.none
            )


generateRandomChatId : Cmd Msg
generateRandomChatId =
    Random.generate GenerateRandomId <| RandomString.string 10 RandomChar.latin



-- EXPORT


toSession : Model -> Session.Session
toSession model =
    model.session
