module Css.CssGrid exposing (CssAttribute(..), Grid(..), GridItem(..), getGridAutoColumns, getGridAutoFlow, getGridAutoRows, getGridColumnEnd, getGridColumnStart, getGridRowEnd, getGridRowStart, getGridTemplate, getGridTemplateArea, getGridTemplateColumns, getGridTemplateRows, gridContainer, gridItem, makeGrid, makeGridItem, setAutoColumns, setAutoFlow, setAutoRows, setColumnEnd, setColumnStart, setRowEnd, setRowStart, setTemplate, setTemplateArea, setTemplateColumns, setTemplateRows)

import Html exposing (..)
import Html.Attributes exposing (..)



-- TYPES


type CssAttribute
    = None
    | Single String
    | Multiple (List String)


type Grid
    = Grid
        { gridTemplateColumns : String
        , gridTemplateRows : String
        , gridTemplateArea : List String
        , gridTemplate : List String
        , gridAutoFlow : String
        , gridAutoColumns : String
        , gridAutoRows : String
        }


type GridItem
    = GridItem
        { gridColumnStart : String
        , gridColumnEnd : String
        , gridRowStart : String
        , gridRowEnd : String
        }



-- ALIASES


type alias GridTemplateColumns =
    String


type alias GridTemplateRows =
    String


type alias GridTemplateArea =
    List String


type alias GridTemplate =
    List String


type alias GridAutoFlow =
    String


type alias GridAutoColumns =
    String


type alias GridAutoRows =
    String


type alias GridColumnStart =
    String


type alias GridColumnEnd =
    String


type alias GridRowStart =
    String


type alias GridRowEnd =
    String



-- CONSTRUCTORS


makeGrid :
    GridTemplateColumns
    -> GridTemplateRows
    -> GridTemplateArea
    -> GridTemplate
    -> GridAutoFlow
    -> GridAutoColumns
    -> GridAutoRows
    -> Grid
makeGrid tCols tRows tArea t aF aC aR =
    Grid
        { gridTemplateColumns = tCols
        , gridTemplateRows = tRows
        , gridTemplateArea = tArea
        , gridTemplate = t
        , gridAutoFlow = aF
        , gridAutoColumns = aC
        , gridAutoRows = aR
        }


makeGridItem :
    GridColumnStart
    -> GridColumnEnd
    -> GridRowStart
    -> GridRowEnd
    -> GridItem
makeGridItem gColS gColE gRowS gRowE =
    GridItem
        { gridColumnStart = gColS
        , gridColumnEnd = gColE
        , gridRowStart = gRowS
        , gridRowEnd = gRowE
        }



-- GRID BASICS


gridContainer : Grid -> List (Html msg) -> Html msg
gridContainer grid children =
    div (gridAttributes grid) children


gridItem :
    GridItem
    -> (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
gridItem item html attributes children =
    html (gridItemAttributes item ++ attributes) children



-- GRID GETTERS


getGridTemplateColumns : Grid -> CssAttribute
getGridTemplateColumns (Grid definition) =
    Single definition.gridTemplateColumns


getGridTemplateRows : Grid -> CssAttribute
getGridTemplateRows (Grid definition) =
    Single definition.gridTemplateRows


getGridTemplateArea : Grid -> CssAttribute
getGridTemplateArea (Grid definition) =
    Multiple definition.gridTemplateArea


getGridTemplate : Grid -> CssAttribute
getGridTemplate (Grid definition) =
    Multiple definition.gridTemplate


getGridAutoFlow : Grid -> CssAttribute
getGridAutoFlow (Grid definition) =
    Single definition.gridAutoFlow


getGridAutoColumns : Grid -> CssAttribute
getGridAutoColumns (Grid definition) =
    Single definition.gridAutoColumns


getGridAutoRows : Grid -> CssAttribute
getGridAutoRows (Grid definition) =
    Single definition.gridAutoRows



-- GRID SETTERS


setTemplateColumns : String -> Grid -> Grid
setTemplateColumns attribute (Grid definition) =
    Grid { definition | gridTemplateColumns = attribute }


setTemplateRows : String -> Grid -> Grid
setTemplateRows attribute (Grid definition) =
    Grid { definition | gridTemplateRows = attribute }


setTemplateArea : List String -> Grid -> Grid
setTemplateArea attribute (Grid definition) =
    Grid { definition | gridTemplateArea = attribute }


setTemplate : List String -> Grid -> Grid
setTemplate attribute (Grid definition) =
    Grid { definition | gridTemplate = attribute }


setAutoFlow : String -> Grid -> Grid
setAutoFlow attribute (Grid definition) =
    Grid { definition | gridAutoFlow = attribute }


setAutoColumns : String -> Grid -> Grid
setAutoColumns attribute (Grid definition) =
    Grid { definition | gridAutoColumns = attribute }


setAutoRows : String -> Grid -> Grid
setAutoRows attribute (Grid definition) =
    Grid { definition | gridAutoRows = attribute }



-- GRID ITEM GETTERS


getGridColumnStart : GridItem -> CssAttribute
getGridColumnStart (GridItem item) =
    Single item.gridColumnStart


getGridColumnEnd : GridItem -> CssAttribute
getGridColumnEnd (GridItem item) =
    Single item.gridColumnEnd


getGridRowStart : GridItem -> CssAttribute
getGridRowStart (GridItem item) =
    Single item.gridRowStart


getGridRowEnd : GridItem -> CssAttribute
getGridRowEnd (GridItem item) =
    Single item.gridRowEnd



-- GRID ITEM SETTERS


setColumnStart : String -> GridItem -> GridItem
setColumnStart attribute (GridItem item) =
    GridItem { item | gridColumnStart = attribute }


setColumnEnd : String -> GridItem -> GridItem
setColumnEnd attribute (GridItem item) =
    GridItem { item | gridColumnEnd = attribute }


setRowStart : String -> GridItem -> GridItem
setRowStart attribute (GridItem item) =
    GridItem { item | gridRowStart = attribute }


setRowEnd : String -> GridItem -> GridItem
setRowEnd attribute (GridItem item) =
    GridItem { item | gridRowEnd = attribute }



-- HELPERS


gridAttributes : Grid -> List (Attribute msg)
gridAttributes grid =
    List.filterMap identity
        [ Just <| style "display" "grid"
        , maybeCssAttribute "grid-template-columns" (getGridTemplateColumns grid)
        , maybeCssAttribute "grid-template-rows" (getGridTemplateRows grid)
        , maybeCssAttribute "grid-template-areas" (getGridTemplateArea grid)
        , maybeCssAttribute "grid-template" (getGridTemplate grid)
        , maybeCssAttribute "grid-auto-flow" (getGridAutoFlow grid)
        , maybeCssAttribute "grid-auto-columns" (getGridAutoColumns grid)
        , maybeCssAttribute "grid-auto-rows" (getGridAutoRows grid)
        ]


gridItemAttributes : GridItem -> List (Attribute msg)
gridItemAttributes item =
    List.filterMap identity
        [ maybeCssAttribute "grid-column-start" (getGridColumnStart item)
        , maybeCssAttribute "grid-column-end" (getGridColumnEnd item)
        , maybeCssAttribute "grid-row-start" (getGridRowStart item)
        , maybeCssAttribute "grid-row-end" (getGridRowEnd item)
        ]



-- UTILITIES


maybeCssAttribute : String -> CssAttribute -> Maybe (Attribute msg)
maybeCssAttribute name attribute =
    case attribute of
        Single attr ->
            Just <| style name attr

        Multiple list ->
            case list of
                [] ->
                    Nothing

                _ ->
                    Just <|
                        style name <|
                            (List.map (\attr -> "'" ++ attr ++ "'") list
                                |> String.concat
                            )

        None ->
            Nothing
