module Page exposing (blank, notFound, viewer)

import Browser
import Html exposing (..)
import Html.Attributes as Attr



-- VIEWS


viewer : { title : String, content : Html msg } -> Browser.Document msg
viewer { title, content } =
    { title = title ++ " - Elm Chat"
    , body =
        [ main_ [] [ content ]
        , footer [ Attr.class "absolute pin-b-8 pin-x" ]
            [ p
                [ Attr.class "text-center text-grey-darker" ]
                [ text "Made with ♥️ using Elm © 2019" ]
            ]
        ]
    }


blank : Browser.Document msg
blank =
    { title = ""
    , body = []
    }


notFound : Browser.Document msg
notFound =
    { title = "Not Found"
    , body =
        [ h1 [] [ text "The page you're looking for doesn't exist." ]
        ]
    }
