module Route exposing (Route(..), changeUrl, fromUrl, pushUrl)

import Browser.Navigation as Nav
import Url
import Url.Builder as Builder
import Url.Parser as Parser exposing ((<?>))
import Url.Parser.Query as Query


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


fromUrl : Url.Url -> Route
fromUrl url =
    Maybe.withDefault NotFound (Parser.parse parser url)


changeUrl : Nav.Key -> Route -> Cmd msg
changeUrl key route =
    Nav.replaceUrl key (toString route)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (toString route)


toString : Route -> String
toString route =
    case route of
        Home ->
            Builder.relative [ "/" ] []

        Chat ->
            Builder.relative [ "chat" ] []

        NotFound ->
            "not-found"
