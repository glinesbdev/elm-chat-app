module Page exposing (blank, notFound, viewer)

import Browser
import Html exposing (..)



-- VIEWS


viewer : { title : String, content : Html msg } -> Browser.Document msg
viewer { title, content } =
    { title = title ++ " - Elm Chat"
    , body = [ content ]
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
