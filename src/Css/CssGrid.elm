module Css.CssGrid exposing (Grid, GridItem, blankGridItem, gridContainer, gridItem, gridTemplateProperty, makeAreaGridItem, makeGrid, makeGridItem, makeTemplateAreaGrid)

import Html exposing (..)
import Html.Attributes exposing (..)



{-
   This section outlines what is explosed by the module.
-}
-- TYPES


type Grid
    = Grid
        { gridTemplateColumns : GridTemplateColumns
        , gridTemplateRows : GridTemplateRows
        , gridTemplateArea : GridTemplateArea
        , gridTemplate : GridTemplate
        , gridAutoFlow : GridAutoFlow
        , gridAutoColumns : GridAutoColumns
        , gridAutoRows : GridAutoRows
        }


type GridItem
    = GridItem
        { gridColumnStart : GridColumnStart
        , gridColumnEnd : GridColumnEnd
        , gridRowStart : GridRowStart
        , gridRowEnd : GridRowEnd
        , gridArea : GridArea
        }



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


makeTemplateAreaGrid :
    GridTemplateColumns
    -> GridTemplateRows
    -> GridTemplateArea
    -> Grid
makeTemplateAreaGrid tCols tRows tArea =
    makeGrid tCols tRows tArea [] "" "" ""


makeGridItem :
    GridColumnStart
    -> GridColumnEnd
    -> GridRowStart
    -> GridRowEnd
    -> GridArea
    -> GridItem
makeGridItem gColS gColE gRowS gRowE gA =
    GridItem
        { gridColumnStart = gColS
        , gridColumnEnd = gColE
        , gridRowStart = gRowS
        , gridRowEnd = gRowE
        , gridArea = gA
        }


makeAreaGridItem : GridArea -> GridItem
makeAreaGridItem area =
    makeGridItem "" "" "" "" area



-- GRID BASICS


gridContainer :
    Grid
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
gridContainer grid attributes children =
    div (gridAttributes grid ++ attributes) children


gridItem :
    GridItem
    -> (List (Attribute msg) -> List (Html msg) -> Html msg)
    -> List (Attribute msg)
    -> List (Html msg)
    -> Html msg
gridItem item html attributes children =
    html (gridItemAttributes item ++ attributes) children



-- CONVENIENCE FUNCTIONS


gridTemplateProperty : ( List String, Int ) -> List String
gridTemplateProperty ( props, amount ) =
    List.foldl (::) (repeatTemplateStrings props amount) []


blankGridItem : GridItem
blankGridItem =
    makeGridItem "" "" "" "" ""



{-
   All functions past this point are not exposed by the module.
-}
-- TYPES


type CssAttribute
    = None
    | Single String
    | Multiple (List String)



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


type alias GridArea =
    String


type alias GridColumnStart =
    String


type alias GridColumnEnd =
    String


type alias GridRowStart =
    String


type alias GridRowEnd =
    String



-- DISPLAY FUNCTIONS


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
        , maybeCssAttribute "grid-area" (getGridArea item)
        ]



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


getGridArea : GridItem -> CssAttribute
getGridArea (GridItem item) =
    Single item.gridArea



-- UTILITIES


repeatTemplateStrings : List String -> Int -> List String
repeatTemplateStrings strings amount =
    List.map (\p -> String.repeat amount (p ++ " ")) strings


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
