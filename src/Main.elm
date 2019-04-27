module Main exposing (Model, Route(..))

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \m -> Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- INIT


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key url Home
    , Cmd.none
    )



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Chat"
    , body =
        [ div [] [ text "Hello" ]
        ]
    }



--UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested request ->
            case request of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key "home"
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model
            , Cmd.none
            )



-- ROUTING


type Route
    = Home
    | Chat
