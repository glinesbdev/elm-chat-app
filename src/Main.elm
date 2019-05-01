module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Css.CssGrid as Grid
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Event
import Random
import Random.Char as RandomChar
import Random.String as RandomString
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
    , chatter : ChatEntity
    , errors : List String
    , pendingMessage : String
    }


type alias ChatEntity =
    { id : String
    , name : String
    , messages : List String
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( initialModel key (urlToRoute <| urlPath url)
    , Cmd.batch
        [ Nav.pushUrl key (urlPath url)
        , generateRandomChatId
        ]
    )


initialModel : Nav.Key -> Route -> Model
initialModel key route =
    { key = key
    , route = route
    , chatter = ChatEntity "" "" []
    , errors = []
    , pendingMessage = ""
    }



-- CHATTER HELPERS


setName : String -> ChatEntity -> ChatEntity
setName name entity =
    { entity | name = name }


setMessages : List String -> ChatEntity -> ChatEntity
setMessages messages entity =
    { entity | messages = messages }


setId : String -> ChatEntity -> ChatEntity
setId id entity =
    { entity | id = id }



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
        [ shouldRender (not <| List.isEmpty model.errors) (viewErrors model.errors)
        , content
        ]
    }



-- ERRORS


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
    if String.isEmpty model.chatter.name then
        base ++ "!"

    else
        base ++ ", " ++ model.chatter.name ++ "!"


nameInput : Model -> Html Msg
nameInput model =
    Grid.gridItem nameInputItem
        form
        [ Attr.class "name-input-form"
        , Event.onSubmit NameSubmitted
        ]
        [ usernameInput model
        , button [ Attr.class "enter-chat-button" ] [ text "Enter" ]
        ]


usernameInput : Model -> Html Msg
usernameInput model =
    div [ Attr.class "icon-input" ]
        [ span [ Attr.class "icon" ] [ text "@" ]
        , input
            [ Attr.placeholder "Enter Your Name"
            , Attr.value model.chatter.name
            , Attr.required True
            , Attr.autofocus True
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
            [ h1 [ Attr.class "chat-heading" ] [ text "Chat Room" ]
            , messageBox model
            , messageInput model
            ]
    }


messageBox : Model -> Html Msg
messageBox model =
    div [ Attr.class "message-box" ] <|
        List.map (\m -> displayedMessage model m) model.chatter.messages


displayedMessage : Model -> String -> Html Msg
displayedMessage model message =
    div
        [ Attr.classList
            [ ( "message", True )
            , ( "text-right", not <| String.contains model.chatter.name model.chatter.id )
            ]
        ]
        [ span [] [ text model.chatter.name ]
        , p
            [ Attr.classList
                [ ( "mt0", True ), ( "text-light", True ) ]
            ]
            [ text message ]
        ]



-- messagePostedByUser


messageInput : Model -> Html Msg
messageInput model =
    form [ Event.onSubmit MessageSubmitted, Attr.class "message-input" ]
        [ input
            [ Event.onInput MessageEntered
            , Attr.value model.pendingMessage
            , Attr.autofocus True
            , Attr.class "message-input-box"
            ]
            []

        -- This is type_ "button" so the form isn't submitted twice
        , button
            [ Event.onClick MessageSubmitted
            , Attr.type_ "button"
            , Attr.class "message-submit"
            ]
            [ text "Send" ]
        ]



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



--UPDATE


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | NameEntered String
    | NameSubmitted
    | MessageEntered String
    | MessageSubmitted
    | GenerateRandomId String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested request ->
            case request of
                Browser.Internal url ->
                    ( { model
                        | route = urlToRoute <| Url.toString url
                        , errors = authErrors (canEnterChat model) (urlToRoute <| urlPath url) ++ model.errors
                      }
                    , authChangeUrl (canEnterChat model) model.key (urlPath url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            let
                route =
                    urlToRoute <| Url.toString url

                chatEntity =
                    setName (clearChatName route model.chatter.name) model.chatter
            in
            ( { model
                | route = route
                , errors = authErrors (canEnterChat model) route
                , chatter = chatEntity
              }
            , Cmd.none
            )

        NameEntered name ->
            let
                chatEntity =
                    setName name model.chatter
            in
            ( { model | chatter = chatEntity, errors = formErrors <| hasValidName name }
            , Cmd.none
            )

        NameSubmitted ->
            let
                route =
                    authRoute (canEnterChat model) Chat

                chatEntity =
                    setName (String.trim model.chatter.name |> clearChatName route) model.chatter
                        |> setMessages ((model.chatter.name ++ " has entered the room!") :: model.chatter.messages)
            in
            ( { model
                | route = route
                , chatter = chatEntity
                , errors = authErrors (canEnterChat model) route
              }
            , Cmd.batch
                [ authChangeUrl (canEnterChat model) model.key chatPath
                , generateRandomChatId
                ]
            )

        MessageEntered message ->
            ( { model | pendingMessage = message }
            , Cmd.none
            )

        MessageSubmitted ->
            let
                chatEntity =
                    setMessages (basicHtmlEscapeTrim model.pendingMessage :: model.chatter.messages) model.chatter
            in
            ( { model
                | chatter = chatEntity
                , pendingMessage = ""
              }
            , Cmd.none
            )

        GenerateRandomId id ->
            let
                chatEntity =
                    setId (String.concat [ model.chatter.name, "-", id ]) model.chatter
            in
            ( { model | chatter = chatEntity }
            , Cmd.none
            )


canEnterChat : Model -> List ( Bool, String )
canEnterChat model =
    hasValidName model.chatter.name


hasValidName : String -> List ( Bool, String )
hasValidName name =
    [ startsWithChar name
    , nameHasCharacters name
    ]


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


nameHasCharacters : String -> ( Bool, String )
nameHasCharacters name =
    if not <| String.isEmpty name then
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


authChangeUrl : List ( Bool, String ) -> Nav.Key -> String -> Cmd Msg
authChangeUrl authCheck key url =
    if allValidationsPass authCheck then
        Nav.pushUrl key url

    else
        Nav.pushUrl key rootPath


authRoute : List ( Bool, String ) -> Route -> Route
authRoute validations route =
    if allValidationsPass validations then
        route

    else
        Home


urlPath : Url.Url -> String
urlPath url =
    Url.toString url


rootPath : String
rootPath =
    Builder.relative [ "" ] []


chatPath : String
chatPath =
    Builder.relative [ "chat" ] []



-- GENERAL HELPERS


allValidationsPass : List ( Bool, String ) -> Bool
allValidationsPass items =
    List.all Tuple.first items



-- The code below was refeactored to the above.
-- Yay for trying to find a reason to use recursion 🤦‍♂️
-- case items of
--     [] ->
--         True
--     [ x ] ->
--         Tuple.first x
--     x :: xs ->
--         if Tuple.first x then
--             allValidationsPass xs
--         else
--             False


maybeError : String -> Maybe String
maybeError error =
    if String.isEmpty error then
        Nothing

    else
        Just error


basicHtmlEscapeTrim : String -> String
basicHtmlEscapeTrim =
    String.replace "<" "&gt;"
        >> String.replace ">" "&lt;"
        >> String.trim


shouldRender : Bool -> Html Msg -> Html Msg
shouldRender cond html =
    if cond then
        html

    else
        text ""


generateRandomChatId : Cmd Msg
generateRandomChatId =
    Random.generate GenerateRandomId <| RandomString.string 10 RandomChar.latin


authErrors : List ( Bool, String ) -> Route -> List String
authErrors validations route =
    case route of
        Chat ->
            if not <| allValidationsPass validations then
                [ "You're not authorized for the requested page." ]

            else
                []

        _ ->
            []



-- TODO remove if no longer needed


routeToString : Route -> String
routeToString route =
    case route of
        Home ->
            "home"

        Chat ->
            "chat"

        NotFound ->
            "not-found"
