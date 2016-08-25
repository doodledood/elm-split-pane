module SplitPane
    exposing
        ( view
        , Model
        , Msg
        , Orientation(..)
        , Px
        , Size(..)
        , splitterPosition
        , width
        , height
        , orientation
        , withFirstViewMinSize
        , withSecondViewMinSize
        , subscriptions
        , update
        , init
        , startAt
        , draggable
        , changeOrientationTo
        , changeWidth
        , changeHeight
        , moveSplitterTo
        )

{-|

This is a split pane view library. Can be used to split views into multiple parts with a splitter between them.

Check out the [examples][] to see how it works.

[examples]: https://github.com/doodledood/elm-split-pane/tree/master/examples

# View

@docs view

# Model

@docs Model, Msg, Orientation, Px, Size, splitterPosition, width, height, orientation

# Init

@docs init

# Update

@docs update

# Helper functions

Use these functions to update the state of the pane

@docs startAt, draggable, withFirstViewMinSize, withSecondViewMinSize, changeOrientationTo, changeWidth, changeHeight, moveSplitterTo

# Subscriptions

@docs subscriptions

-}

import Html exposing (Html, span, div, Attribute)
import Html.Attributes exposing (style)
import Html.Events exposing (on)
import Mouse
import Json.Decode as Json
import Maybe


-- MODEL


{-| Size in either pixels or percentage.
-}
type Size
    = Px Int
    | Percentage Float


{-| Px in pixels.
-}
type alias Px =
    Int


{-| Orientation of pane.
-}
type Orientation
    = Horizontal
    | Vertical


{-| Tracks state of pane.
-}
type Model
    = Model
        { splitterPosition : Px
        , draggable : Bool
        , firstViewMinSize : Size
        , secondViewMinSize : Size
        , paneWidth : Px
        , paneHeight : Px
        , dragPosition : Maybe Mouse.Position
        , orientation : Orientation
        }


{-| Used to track resizes.
-}
type Msg
    = ResizeStarted Mouse.Position
    | Resize Mouse.Position
    | ResizeEnded Mouse.Position


pxToCss : Px -> String
pxToCss px =
    toString px ++ "px"


sizeToCss : Size -> String
sizeToCss size =
    case size of
        Px n ->
            pxToCss n

        Percentage p ->
            toString (p * 100) ++ "%"



-- INIT


{-| Initialize a new model.

        init
            { paneWidth = 600
            , paneHeight = 600
            }
-}
init :
    { a
        | paneHeight : Px
        , paneWidth : Px
    }
    -> Model
init { paneWidth, paneHeight } =
    Model
        { splitterPosition = paneWidth // 2
        , draggable = True
        , firstViewMinSize = Px 0
        , secondViewMinSize = Px 0
        , paneWidth = paneWidth
        , paneHeight = paneHeight
        , dragPosition = Nothing
        , orientation = Horizontal
        }



-- HELPERS


{-| Sets the starting position for the splitter.

        init =
            SplitPane.init
                { paneWidth = 800
                , paneHeight = 600
                , orientation = Horizontal
                }
                |> startAt 300
-}
startAt : Size -> Model -> Model
startAt startingSplitterPosition (Model model) =
    let
        cappedPosition =
            capSplitterPosition startingSplitterPosition model
    in
        Model { model | splitterPosition = cappedPosition }

capSplitterPosition
    : Size
    -> { a | orientation : Orientation, paneWidth : Int, paneHeight : Int }
    -> Int
capSplitterPosition splitterPosition model =
    let
        pxSize =
            sizeToPx model splitterPosition

        cappedPosition =
            case model.orientation of
                Horizontal ->
                    min (max pxSize 0) model.paneWidth

                Vertical ->
                    min (max pxSize 0) model.paneHeight
    in
        cappedPosition


{-| Retrieves current splitter position in pixels from the model (relative to the edge of the pane).
-}
splitterPosition : Model -> Px
splitterPosition (Model model) =
    model.splitterPosition

{-| Retrieves current width of the pane from the model.
-}
width : Model -> Px
width (Model model) =
    model.paneWidth

{-| Retrieves current height of the pane from the model.
-}
height : Model -> Px
height (Model model) =
    model.paneHeight

{-| Retrieves current orientation of the pane from the model.
-}
orientation : Model -> Orientation
orientation (Model model) =
    model.orientation

{-| Moves the splitter to the requested location relative to the edge of the pane
-}
moveSplitterTo : Size -> Model -> Model
moveSplitterTo size (Model model) =
    let
        pxSize =
            sizeToPx model size
    in
        Model { model | splitterPosition = pxSize }


{-| Make pane splitter draggable or not

        init =
            SplitPane.init
                { paneWidth = 800
                , paneHeight = 600
                }
                |> draggable False
-}
draggable : Bool -> Model -> Model
draggable isDraggable (Model model) =
    Model { model | draggable = isDraggable }


{-| Set minimum size for the first view.
    When the pane is horizontal, this is the left view.
    When the pane is vertical, this is the top view.
-}
withFirstViewMinSize : Size -> Model -> Model
withFirstViewMinSize size (Model model) =
    Model { model | firstViewMinSize = size }


{-| Set minimum size for the second view.
    When the pane is horizontal, this is the right view.
    When the pane is vertical, this is the bottom view.
-}
withSecondViewMinSize : Size -> Model -> Model
withSecondViewMinSize size (Model model) =
    Model { model | secondViewMinSize = size }


{-| Set the orientation of the pane.
-}
changeOrientationTo : Orientation -> Model -> Model
changeOrientationTo o (Model model) =
    Model { model | orientation = o }


{-| Change the width of the pane.
-}
changeWidth : Px -> Model -> Model
changeWidth px (Model model) =
    Model { model | paneWidth = px }


{-| Change the height of the pane.
-}
changeHeight : Px -> Model -> Model
changeHeight px (Model model) =
    Model { model | paneHeight = px }


sizeToPx :
    { a | orientation : Orientation, paneHeight : Int, paneWidth : Int }
    -> Size
    -> Int
sizeToPx model size =
    case size of
        Px n ->
            n

        Percentage p ->
            case model.orientation of
                Horizontal ->
                    round <| p * toFloat model.paneWidth

                Vertical ->
                    round <| p * toFloat model.paneHeight



-- UPDATE


{-| Updates internal model.
-}
update : Msg -> Model -> Model
update msg (Model model) =
    if not model.draggable then
        Model model
    else
        case msg of
            ResizeStarted pos ->
                Model { model | dragPosition = Just pos }

            ResizeEnded _ ->
                Model { model | dragPosition = Nothing }

            Resize curr ->
                case model.orientation of
                    Horizontal ->
                        resize model curr (\{ x, y } -> x)

                    Vertical ->
                        resize model curr (\{ x, y } -> y)


resize :
    { dragPosition : Maybe Mouse.Position
    , draggable : Bool
    , firstViewMinSize : Size
    , paneHeight : Px
    , paneWidth : Px
    , secondViewMinSize : Size
    , splitterPosition : Px
    , orientation : Orientation
    }
    -> Mouse.Position
    -> ({ x : Int, y : Int } -> Int)
    -> Model
resize model newDragPosition diffProp =
    case model.dragPosition of
        Nothing ->
            Model model

        Just prev ->
            let
                calculateDiff x y =
                    diffProp x - diffProp y

                diff =
                    calculateDiff newDragPosition prev

                newSplitterPosition =
                    model.splitterPosition + diff
            in
                if isInLimits model newSplitterPosition then
                    Model
                        { model
                            | splitterPosition = newSplitterPosition
                            , dragPosition = Just newDragPosition
                        }
                else
                    Model model


isInLimits :
    { a
        | orientation : Orientation
        , paneHeight : Px
        , paneWidth : Px
        , firstViewMinSize : Size
        , secondViewMinSize : Size
    }
    -> Px
    -> Bool
isInLimits model newSplitterPosition =
    let
        ( minPx, maxPx ) =
            calculateMinAndMaxPositionsForSplitter model
    in
        newSplitterPosition >= minPx && newSplitterPosition <= maxPx


calculateMinAndMaxPositionsForSplitter :
    { a
        | firstViewMinSize : Size
        , orientation : Orientation
        , paneHeight : Px
        , paneWidth : Px
        , secondViewMinSize : Size
    }
    -> ( Int, Int )
calculateMinAndMaxPositionsForSplitter model =
    let
        totalLength =
            case model.orientation of
                Horizontal ->
                    model.paneWidth

                Vertical ->
                    model.paneHeight

        minPx =
            case model.firstViewMinSize of
                Px n ->
                    n

                Percentage p ->
                    round <| toFloat totalLength * p

        maxPx =
            case model.secondViewMinSize of
                Px n ->
                    totalLength - n

                Percentage p ->
                    totalLength - (round <| toFloat totalLength * p)
    in
        ( minPx, maxPx )



-- VIEW


defaultSplitter : (Msg -> msg) -> Bool -> Orientation -> Html msg
defaultSplitter toMsg draggable orientation =
    let
        baseStyles =
            [ ( "width", "100%" )
            , ( "background", "#000" )
            , ( "boxSizing", "border-box" )
            , ( "opacity", ".2" )
            , ( "zIndex", "1" )
            , ( "webkitUserSelect", "none" )
            , ( "mozUserSelect", "none" )
            , ( "userSelect", "none" )
            , ( "backgroundClip", "padding-box" )
            ]

        createSpanWith styles =
            span
                [ onMouseDown toMsg
                , style (baseStyles ++ styles)
                ]
                []
    in
        case orientation of
            Horizontal ->
                createSpanWith <|
                    [ ( "width", "11px" )
                    , ( "margin", "0 -5px" )
                    , ( "borderLeft", "5px solid rgba(255, 255, 255, 0)" )
                    , ( "borderRight", "5px solid rgba(255, 255, 255, 0)" )
                    ]
                        ++ if draggable then
                            [ ( "cursor", "col-resize" ) ]
                           else
                            []

            Vertical ->
                createSpanWith <|
                    [ ( "height", "11px" )
                    , ( "width", "100%" )
                    , ( "margin", "-5px 0" )
                    , ( "borderTop", "5px solid rgba(255, 255, 255, 0)" )
                    , ( "borderBottom", "5px solid rgba(255, 255, 255, 0)" )
                    ]
                        ++ if draggable then
                            [ ( "cursor", "row-resize" ) ]
                           else
                            []


{-| Default pane with two views

        view : SplitPane.Model -> Html SplitPane.Msg
        view model =
            SplitPane.view identity model firstView secondView


        firstView : Html a
        firstView =
            img [ src "http://4.bp.blogspot.com/-s3sIvuCfg4o/VP-82RkCOGI/AAAAAAAALSY/509obByLvNw/s1600/baby-cat-wallpaper.jpg" ] []


        secondView : Html a
        secondView =
            img [ src "http://2.bp.blogspot.com/-pATX0YgNSFs/VP-82AQKcuI/AAAAAAAALSU/Vet9e7Qsjjw/s1600/Cat-hd-wallpapers.jpg" ] []
-}
view : (Msg -> msg) -> Model -> Html msg -> Html msg -> Html msg
view toMsg (Model model) firstView secondView =
    case model.orientation of
        Horizontal ->
            div
                [ style
                    [ ( "overflow", "hidden" )
                    , ( "display", "flex" )
                    , ( "flex", "1 1 0%" )
                    , ( "flexDirection", "row" )
                    , ( "width", pxToCss model.paneWidth )
                    , ( "height", pxToCss model.paneHeight )
                    , ( "boxSizing", "border-box" )
                    ]
                ]
                [ div
                    [ style
                        [ ( "width", pxToCss model.splitterPosition )
                        , ( "minWidth", sizeToCss model.firstViewMinSize )
                        , ( "overflow", "hidden" )
                        , ( "boxSizing", "border-box" )
                        , ( "position", "relative" )
                        ]
                    ]
                    [ firstView ]
                , defaultSplitter toMsg model.draggable model.orientation
                , div
                    [ style
                        [ ( "width", pxToCss <| model.paneWidth - model.splitterPosition )
                        , ( "minWidth", sizeToCss model.secondViewMinSize )
                        , ( "overflow", "hidden" )
                        , ( "boxSizing", "border-box" )
                        , ( "position", "relative" )
                        ]
                    ]
                    [ secondView ]
                ]

        Vertical ->
            div
                [ style
                    [ ( "overflow", "hidden" )
                    , ( "display", "flex" )
                    , ( "flex", "1 1 0%" )
                    , ( "flexDirection", "column" )
                    , ( "width", pxToCss model.paneWidth )
                    , ( "height", pxToCss model.paneHeight )
                    , ( "boxSizing", "border-box" )
                    ]
                ]
                [ div
                    [ style
                        [ ( "height", pxToCss model.splitterPosition )
                        , ( "minHeight", sizeToCss model.firstViewMinSize )
                        , ( "overflow", "hidden" )
                        , ( "boxSizing", "border-box" )
                        , ( "position", "relative" )
                        ]
                    ]
                    [ firstView ]
                , defaultSplitter toMsg model.draggable model.orientation
                , div
                    [ style
                        [ ( "height", pxToCss <| model.paneHeight - model.splitterPosition )
                        , ( "minHeight", sizeToCss model.secondViewMinSize )
                        , ( "overflow", "hidden" )
                        , ( "boxSizing", "border-box" )
                        , ( "position", "relative" )
                        ]
                    ]
                    [ secondView ]
                ]


onMouseDown : (Msg -> msg) -> Attribute msg
onMouseDown toMsg =
    on "mousedown" <| Json.map (toMsg << ResizeStarted) Mouse.position



-- SUBSCRIPTIONS


{-| Subscribes to relevant events for resizing
-}
subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    if not model.draggable then
        Sub.none
    else
        case model.dragPosition of
            Just _ ->
                Sub.batch
                    [ Mouse.moves Resize
                    , Mouse.ups ResizeEnded
                    ]

            Nothing ->
                Sub.none
