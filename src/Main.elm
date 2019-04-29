module Main exposing (Model, Msg(..), Route(..), chatPath, chatView, homeView, init, main, notFoundView, parser, rootPath, update, urlToRoute, view, viewContainer)

import Browser
import Browser.Navigation as Nav
import Css.CssGrid as Grid
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Event
import Url
import Url.Builder as Builder
import Url.Parser as Parser



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- INIT


type alias Model =
    { key : Nav.Key
    , route : Route
    , chatName : String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key (urlToRoute (Url.toString url)) ""
    , Nav.pushUrl key (Url.toString url)
    )



-- MAIN VIEW


view : Model -> Browser.Document Msg
view model =
    case model.route of
        Home ->
            viewContainer (homeView model)

        Chat ->
            viewContainer (chatView model)

        NotFound ->
            viewContainer notFoundView



-- VIEW CONTAINER


viewContainer : { title : String, content : Html Msg } -> Browser.Document Msg
viewContainer { title, content } =
    { title = title ++ " - Elm Chat"
    , body = [ content ]
    }



-- HOME VIEW


homeView : Model -> { title : String, content : Html Msg }
homeView model =
    { title = "Home"
    , content =
        homeGridContainer model
    }


homeGridContainer : Model -> Html Msg
homeGridContainer model =
    Grid.gridContainer homeGrid
        []
        [ Grid.gridItem mainContent h1 [ Attr.style "justify-self" "center" ] [ text (welcomeText model) ]
        , nameInput model
        ]


welcomeText : Model -> String
welcomeText model =
    let
        base =
            "Welcome"
    in
    case String.length model.chatName of
        0 ->
            base ++ "!"

        _ ->
            base ++ ", " ++ model.chatName ++ "!"


nameInput : Model -> Html Msg
nameInput model =
    Grid.gridItem nameInputItem
        form
        [ Attr.style "grid-area" "input"
        , Attr.style "justify-self" "center"
        , Attr.style "width" "40%"
        , Event.onSubmit NameSubmitted
        ]
        [ input
            [ Attr.style "width" "100%"
            , Attr.style "font-size" "18px"
            , Attr.style "padding" "0.5em"
            , Attr.placeholder "Enter Your Name"
            , Attr.value model.chatName
            , Event.onInput NameEntered
            ]
            []
        ]



-- HOME STYLES


homeGrid : Grid.Grid
homeGrid =
    Grid.makeTemplateAreaGrid
        "50px 1fr 50px"
        "50px 1fr 50px"
        (Grid.gridTemplateProperty ( [ "header", "main", "input" ], 3 ))


mainContent : Grid.GridItem
mainContent =
    Grid.makeAreaGridItem "main"


nameInputItem : Grid.GridItem
nameInputItem =
    Grid.blankGridItem



-- CHAT VIEW


chatView : Model -> { title : String, content : Html Msg }
chatView model =
    { title = "Chat Room"
    , content =
        div []
            [ h1 [] [ text "Chat Room" ]
            , p [] [ text (model.chatName ++ " has entered the room!") ]
            ]
    }



-- NOT FOUND VIEW


notFoundView : { title : String, content : Html Msg }
notFoundView =
    { title = "Not Found"
    , content =
        div []
            [ h1 [] [ text "404!" ]
            , p [] [ text "The page you're looking for doesn't exist." ]
            ]
    }



-- URL HELPERS


rootPath : String
rootPath =
    Builder.absolute [ "" ] []


chatPath : String
chatPath =
    Builder.absolute [ "chat" ] []



--UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | NameEntered String
    | NameSubmitted


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested request ->
            case request of
                Browser.Internal url ->
                    ( { model | route = urlToRoute (Url.toString url) }
                    , Nav.pushUrl model.key (Builder.absolute [ Url.toString url ] [])
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( { model
                | route = urlToRoute (Url.toString url)
                , chatName = clearChatName (urlToRoute <| Url.toString url) model.chatName
              }
            , Cmd.none
            )

        NameEntered name ->
            ( { model | chatName = name }
            , Cmd.none
            )

        NameSubmitted ->
            ( { model | route = Chat }
            , Nav.pushUrl model.key (Builder.absolute [ "chat" ] [])
            )


clearChatName : Route -> String -> String
clearChatName route currentName =
    case route of
        Chat ->
            currentName

        _ ->
            ""



-- ROUTING


type Route
    = Home
    | Chat
    | NotFound


parser : Parser.Parser (Route -> a) a
parser =
    Parser.oneOf
        [ Parser.map Home Parser.top
        , Parser.map Chat (Parser.s "chat")
        , Parser.map NotFound (Parser.s "not-found")
        ]


urlToRoute : String -> Route
urlToRoute url =
    case Url.fromString url of
        Nothing ->
            NotFound

        Just route ->
            Maybe.withDefault NotFound (Parser.parse parser route)
