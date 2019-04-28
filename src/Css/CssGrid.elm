module CssGrid exposing (Grid, GridItem, getGridAutoColumns, getGridAutoFlow, getGridAutoRows, getGridColumnEnd, getGridColumnStart, getGridRowEnd, getGridRowStart, getGridTemplate, getGridTemplateArea, getGridTemplateColumns, getGridTemplateRows, gridAttributes, gridContainer, gridItem, gridItemAttributes, setAutoColumns, setAutoFlow, setAutoRows, setColumnEnd, setColumnStart, setRowEnd, setRowStart, setTemplate, setTemplateArea, setTemplateColumns, setTemplateRows)

import Html exposing (..)
import Html.Attributes exposing (..)



-- TYPES


type CssAttribute
    = None
    | Single String
    | Multiple (List String)


type Grid
    = GridDefinition
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



-- GRID BASICS


gridContainer : Grid -> List (Html msg) -> Html msg
gridContainer grid children =
    div (gridAttributes grid) children


gridItem : GridItem -> List (Html msg) -> Html msg
gridItem item children =
    div (gridItemAttributes item) children



-- GRID GETTERS


getGridTemplateColumns : Grid -> CssAttribute
getGridTemplateColumns (GridDefinition definition) =
    Single definition.gridTemplateColumns


getGridTemplateRows : Grid -> CssAttribute
getGridTemplateRows (GridDefinition definition) =
    Single definition.gridTemplateRows


getGridTemplateArea : Grid -> CssAttribute
getGridTemplateArea (GridDefinition definition) =
    Multiple definition.gridTemplateArea


getGridTemplate : Grid -> CssAttribute
getGridTemplate (GridDefinition definition) =
    Multiple definition.gridTemplate


getGridAutoFlow : Grid -> CssAttribute
getGridAutoFlow (GridDefinition definition) =
    Single definition.gridAutoFlow


getGridAutoColumns : Grid -> CssAttribute
getGridAutoColumns (GridDefinition definition) =
    Single definition.gridAutoColumns


getGridAutoRows : Grid -> CssAttribute
getGridAutoRows (GridDefinition definition) =
    Single definition.gridAutoRows



-- GRID SETTERS


setTemplateColumns : String -> Grid -> Grid
setTemplateColumns attribute (GridDefinition definition) =
    GridDefinition { definition | gridTemplateColumns = attribute }


setTemplateRows : String -> Grid -> Grid
setTemplateRows attribute (GridDefinition definition) =
    GridDefinition { definition | gridTemplateRows = attribute }


setTemplateArea : List String -> Grid -> Grid
setTemplateArea attribute (GridDefinition definition) =
    GridDefinition { definition | gridTemplateArea = attribute }


setTemplate : List String -> Grid -> Grid
setTemplate attribute (GridDefinition definition) =
    GridDefinition { definition | gridTemplate = attribute }


setAutoFlow : String -> Grid -> Grid
setAutoFlow attribute (GridDefinition definition) =
    GridDefinition { definition | gridAutoFlow = attribute }


setAutoColumns : String -> Grid -> Grid
setAutoColumns attribute (GridDefinition definition) =
    GridDefinition { definition | gridAutoColumns = attribute }


setAutoRows : String -> Grid -> Grid
setAutoRows attribute (GridDefinition definition) =
    GridDefinition { definition | gridAutoRows = attribute }



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



-- ULTILIES


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
