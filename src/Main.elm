module Main exposing (Model, Msg(..), Route(..), chatPath, chatView, homeView, init, main, notFoundView, parser, rootPath, update, urlToRoute, view, viewContainer)

import Browser
import Browser.Navigation as Nav
import Css.CssGrid as Grid
import Html exposing (..)
import Html.Attributes exposing (href)
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
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key (urlToRoute (Url.toString url))
    , Nav.pushUrl key (Url.toString url)
    )



-- MAIN VIEW


view : Model -> Browser.Document Msg
view model =
    case model.route of
        Home ->
            viewContainer homeView

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


homeView : { title : String, content : Html Msg }
homeView =
    { title = "Home"
    , content =
        Grid.gridContainer homeGrid
            [ Grid.gridItem header h1 [] [ text "Welcome!" ]
            ]
    }


homeGrid : Grid.Grid
homeGrid =
    Grid.makeGrid "0.5fr 1fr 0.5fr" "0.25fr 1fr 0.25fr" [] [] "" "" ""


header : Grid.GridItem
header =
    Grid.makeGridItem "two" "three" "row1-end" "row2-end"



-- CHAT VIEW


chatView : Model -> { title : String, content : Html Msg }
chatView model =
    { title = "Chat Room"
    , content =
        div []
            [ h1 [] [ text "Chat Room" ] ]
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



-- VIEW HELPERS


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested request ->
            case request of
                Browser.Internal url ->
                    ( { model | route = Url.toString url |> urlToRoute }
                    , Nav.pushUrl model.key (Builder.relative [ Url.toString url ] [])
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( { model | route = Url.toString url |> urlToRoute }
            , Cmd.none
            )



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
