module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Chat
import Home
import Html
import Page
import Route
import Session
import Url



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- INIT


type Model
    = Redirect Session.Session
    | Home Home.Model
    | Chat Chat.Model
    | NotFound Session.Session


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    resolveRoute (Route.fromUrl url) key (Redirect <| Session.Guest key)


resolveRoute : Route.Route -> Nav.Key -> Model -> ( Model, Cmd Msg )
resolveRoute route key model =
    let
        session =
            toSession model
    in
    case route of
        Route.Home ->
            Home.init (Session.init key) (Just [])
                |> resolveWith Home GoToHome model

        Route.Chat ->
            let
                userName =
                    Session.userName session
            in
            if validUserName userName then
                Chat.init (Session.LoggedIn Nothing (Session.userId session) userName key)
                    |> resolveWith Chat GoToChat model

            else
                Home.init (Session.init key) (Just [ "You must have a name to chat!" ])
                    |> resolveWith Home GoToHome model

        _ ->
            Home.init (Session.init key) (Just [])
                |> resolveWith Home GoToHome model


toSession : Model -> Session.Session
toSession model =
    case model of
        Redirect session ->
            session

        Home home ->
            Home.toSession home

        Chat chat ->
            Chat.toSession chat

        NotFound session ->
            session



-- MAIN VIEW


view : Model -> Browser.Document Msg
view model =
    let
        viewPage toMsg config =
            let
                { title, body } =
                    Page.viewer config
            in
            { title = title
            , body = List.map (Html.map toMsg) body
            }
    in
    case model of
        Redirect _ ->
            Page.blank

        NotFound _ ->
            Page.notFound

        Home home ->
            viewPage GoToHome (Home.view home)

        Chat chat ->
            viewPage GoToChat (Chat.view chat)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Home home ->
            Sub.map GoToHome (Home.subscriptions home)

        Chat chat ->
            Sub.map GoToChat (Chat.subscriptions chat)

        _ ->
            Sub.none



--UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | GoToHome Home.Msg
    | GoToChat Chat.Msg
    | GoToSession Session.Session


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( UrlRequested request, _ ) ->
            case request of
                Browser.Internal url ->
                    resolveRoute (Route.fromUrl url) (Session.navKey <| toSession model) model

                Browser.External url ->
                    ( model, Nav.load url )

        ( UrlChanged url, _ ) ->
            resolveRoute (Route.fromUrl url) (Session.navKey <| toSession model) model

        ( GoToHome homeMsg, Home home ) ->
            Home.update homeMsg home
                |> resolveWith Home GoToHome model

        ( GoToChat chatMsg, Chat chat ) ->
            Chat.update chatMsg chat
                |> resolveWith Chat GoToChat model

        ( GoToSession session, Redirect _ ) ->
            ( Redirect session
            , Route.changeUrl (Session.navKey session) Route.Home
            )

        ( _, _ ) ->
            -- Do nothing upon unkown command and return the current state of the app
            ( model, Cmd.none )


resolveWith :
    (subModel -> Model)
    -> (subMsg -> Msg)
    -> Model
    -> ( subModel, Cmd subMsg )
    -> ( Model, Cmd Msg )
resolveWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



-- HELPERS


validUserName : String -> Bool
validUserName name =
    not <| String.isEmpty name
