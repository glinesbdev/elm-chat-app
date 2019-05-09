port module Home exposing (Model, Msg, init, subscriptions, toSession, update, view)

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



-- INIT


type alias Model =
    { session : Session.Session
    , name : String
    , errors : Errors
    }


type alias SavedUser =
    { id : UserId
    , name : UserName
    , room : Room
    }


init : Session.Session -> Maybe Errors -> ( Model, Cmd Msg )
init session errors =
    ( initialModel session <| Maybe.withDefault [] errors
    , checkForUser ()
    )


initialModel : Session.Session -> Errors -> Model
initialModel session errors =
    { session = session
    , name = ""
    , errors = errors
    }



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content =
        div []
            [ errorList model.errors
            , h1 [ Attr.class "text-center mb-8 mt-20" ] [ text "Chatterbox" ]
            , inputForm model
            , formButton
            ]
    }


inputForm : Model -> Html Msg
inputForm model =
    form
        [ Event.onSubmit SubmitName
        , Attr.class "flex flex-row px-4"
        ]
        [ span [ Attr.class "bg-indigo p-4 text-grey-lighter rounded-bl-lg rounded-tl-lg" ] [ text "@" ]
        , input
            [ Attr.placeholder "Enter your name"
            , Attr.class "border-solid text-lg flex-1 p-4 border-indigo border-1 rounded-br-lg rounded-tr-lg"
            , Attr.value model.name
            , Attr.required True
            , Attr.autofocus True
            , Event.onInput EnterName
            ]
            []
        ]


formButton : Html Msg
formButton =
    div [ Attr.class "text-center" ]
        [ button
            [ Attr.class "border-solid bg-teal-dark hover:bg-teal-darker text-grey-lighter rounded-lg py-4 px-8 mt-8"
            , Attr.type_ "button"
            , Event.onClick SubmitName
            ]
            [ text "Enter" ]
        ]


errorList : Errors -> Html Msg
errorList errors =
    shouldRender
        (List.length errors > 0)
        (div
            []
            [ ul [ Attr.class " text-center text-grey-lightest errors p-8 bg-red-light" ] <| List.map listItem errors ]
        )


listItem : String -> Html Msg
listItem message =
    li [] [ text message ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    setUser (\user -> GotUser user)



-- PORTS
-- OUTGOING


port storeUser : E.Value -> Cmd msg


port checkForUser : () -> Cmd msg



-- INCOMING


port setUser : (E.Value -> msg) -> Sub msg



-- UPDATE


type Msg
    = EnterName String
    | SubmitName
    | GenerateRandomId String
    | GotUser E.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnterName name ->
            ( { model | name = name }, Cmd.none )

        SubmitName ->
            ( model
            , generateRandomId
            )

        GenerateRandomId id ->
            ( { model | session = Session.LoggedIn Nothing id model.name (Session.navKey model.session) }
            , Cmd.batch
                [ storeUser <| encodeUserData ( id, model.name )
                , Route.pushUrl (Session.navKey model.session) Route.Chat
                ]
            )

        GotUser user ->
            let
                userVal =
                    D.decodeValue userDecoder user
            in
            case userVal of
                Ok val ->
                    ( { model | session = Session.LoggedIn (Just val.room.name) val.id val.name (Session.navKey model.session) }
                    , Route.pushUrl (Session.navKey model.session) Route.Chat
                    )

                Err err ->
                    case err of
                        D.Failure error _ ->
                            ( { model | errors = error :: model.errors }
                            , Cmd.none
                            )

                        _ ->
                            ( model
                            , Cmd.none
                            )



-- HELPERS


userDecoder : D.Decoder SavedUser
userDecoder =
    D.map3 SavedUser
        (D.field "id" D.string)
        (D.field "name" D.string)
        (D.field "room" roomDecoder)


roomDecoder : D.Decoder Room
roomDecoder =
    D.map Room
        (D.field "name" D.string)


shouldRender : Bool -> Html Msg -> Html Msg
shouldRender cond html =
    if cond then
        html

    else
        text ""


generateRandomId : Cmd Msg
generateRandomId =
    Random.generate GenerateRandomId <| RandomString.string 20 RandomChar.latin


encodeUserData : ( String, String ) -> E.Value
encodeUserData ( id, name ) =
    E.object
        [ ( "id", E.string id )
        , ( "name", E.string name )
        ]



-- EXPORT


toSession : Model -> Session.Session
toSession model =
    model.session
