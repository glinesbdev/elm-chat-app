module Home exposing (Model, Msg, init, toSession, update, view)

import Html exposing (..)
import Session



-- INIT


type alias Model =
    { session : Session.Session
    , name : String
    }


init : Session.Session -> ( Model, Cmd Msg )
init session =
    ( Model session "", Cmd.none )



-- VIEW


view : Model -> { title : String, content : Html Msg }
view model =
    { title = "Home"
    , content =
        div []
            [ h1 [] [ text "Home Page" ]
            ]
    }



-- UPDATE


type Msg
    = EnterName String
    | SubmitName


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EnterName name ->
            ( { model | name = name }, Cmd.none )

        SubmitName ->
            ( model, Cmd.none )


toSession : Model -> Session.Session
toSession model =
    model.session
