module Home exposing (Model, Msg, init, toSession, update, view)

import Css.CssGrid as Grid
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Event
import Route
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
    if String.isEmpty model.name then
        base ++ "!"

    else
        base ++ ", " ++ model.name ++ "!"


nameInput : Model -> Html Msg
nameInput model =
    Grid.gridItem nameInputItem
        form
        [ Attr.class "name-input-form"
        , Event.onSubmit SubmitName
        ]
        [ chatNameInput model
        , button
            [ Attr.class "enter-chat-button"
            , Attr.type_ "button"
            , Event.onClick SubmitName
            ]
            [ text "Enter" ]
        ]


chatNameInput : Model -> Html Msg
chatNameInput model =
    div [ Attr.class "icon-input" ]
        [ span [ Attr.class "icon" ] [ text "@" ]
        , input
            [ Attr.placeholder "Enter your name"
            , Attr.value model.name
            , Attr.required True
            , Attr.autofocus True
            , Event.onInput EnterName
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
            ( model
            , Route.pushUrl (Session.navKey model.session) (Route.Chat (Just model.name))
            )



-- EXPORT


toSession : Model -> Session.Session
toSession model =
    model.session
