module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Css.CssGrid as Grid
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Event
import Regex
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
    , errors : List String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model key (urlToRoute (Url.toString url)) "" []
    , Nav.pushUrl key (Url.toString url)
    )



-- MAIN VIEW


view : Model -> Browser.Document Msg
view model =
    case model.route of
        Home ->
            viewContainer model (homeView model)

        Chat ->
            viewContainer model (chatView model)

        NotFound ->
            viewContainer model notFoundView



-- VIEW CONTAINER


viewContainer : Model -> { title : String, content : Html Msg } -> Browser.Document Msg
viewContainer model { title, content } =
    { title = title ++ " - Elm Chat"
    , body =
        [ viewErrors model.errors
        , content
        ]
    }



-- Errors


viewErrors : List String -> Html Msg
viewErrors errors =
    let
        actualErrors =
            List.filterMap maybeError errors
    in
    ul []
        (List.map (\e -> li [] [ text e ]) actualErrors)



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
        [ Attr.class "home-container" ]
        [ Grid.gridItem mainContent h1 [ Attr.class "heading" ] [ text (welcomeText model) ]
        , nameInput model
        ]


welcomeText : Model -> String
welcomeText model =
    let
        base =
            "Welcome"
    in
    if String.isEmpty model.chatName then
        base ++ "!"

    else
        base ++ ", " ++ model.chatName ++ "!"


nameInput : Model -> Html Msg
nameInput model =
    Grid.gridItem nameInputItem
        form
        [ Attr.class "name-input-form"
        , Event.onSubmit NameSubmitted
        ]
        [ usernameInput model <|
            input
                [ Attr.placeholder "Enter Your Name"
                , Attr.value model.chatName

                -- , Attr.required True
                , Event.onInput NameEntered
                ]
                []
        ]


usernameInput : Model -> Html Msg -> Html Msg
usernameInput model input =
    div [ Attr.class "icon-input" ]
        [ span [ Attr.class "icon" ] [ text "@" ]
        , input
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
                    , authChangeUrl
                        (canEnterChat model)
                        model.key
                        (Builder.absolute [ Url.toString url ] [])
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
            ( { model | chatName = name, errors = formErrors <| hasValidName name }
            , Cmd.none
            )

        NameSubmitted ->
            ( { model
                | route = authRoute (canEnterChat model) Chat
                , chatName =
                    clearChatName
                        (authRoute (canEnterChat model) Chat)
                        (String.trim model.chatName)
                , errors = formErrors <| canEnterChat model
              }
            , authChangeUrl (canEnterChat model) model.key chatPath
            )


canEnterChat : Model -> List ( Bool, String )
canEnterChat model =
    hasValidName model.chatName


hasValidName : String -> List ( Bool, String )
hasValidName name =
    [ startsWithChar name
    , nameHasCharacters (not <| String.isEmpty name)
    ]


authChangeUrl : List ( Bool, String ) -> Nav.Key -> String -> Cmd Msg
authChangeUrl authCheck key url =
    if allValidationsPass authCheck then
        Nav.pushUrl key (Builder.absolute [ url ] [])

    else
        Nav.pushUrl key (Builder.absolute [ rootPath ] [])


authRoute : List ( Bool, String ) -> Route -> Route
authRoute validations route =
    if allValidationsPass validations then
        route

    else
        Home


clearChatName : Route -> String -> String
clearChatName route currentName =
    case route of
        Chat ->
            currentName

        _ ->
            ""



-- FORM VALIDATION


startsWithDigit : Regex.Regex
startsWithDigit =
    Maybe.withDefault Regex.never <|
        Regex.fromString "^([^\\d+].*)?$"


startsWithChar : String -> ( Bool, String )
startsWithChar val =
    if not <| String.isEmpty val then
        ( Regex.contains startsWithDigit val, "Names cannot start with numbers." )

    else
        ( True, "" )


nameHasCharacters : Bool -> ( Bool, String )
nameHasCharacters valid =
    if valid then
        ( True, "" )

    else
        ( False, "Names cannot be blank." )


formErrors : List ( Bool, String ) -> List String
formErrors validations =
    if allValidationsPass validations then
        []

    else
        List.map (\v -> Tuple.second v) validations



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



-- GENERAL HELPERS


allValidationsPass : List ( Bool, String ) -> Bool
allValidationsPass items =
    case items of
        [] ->
            True

        [ x ] ->
            Tuple.first x

        x :: xs ->
            if Tuple.first x then
                allValidationsPass xs

            else
                False


maybeError : String -> Maybe String
maybeError error =
    case error of
        "" ->
            Nothing

        _ ->
            Just error
