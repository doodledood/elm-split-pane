module SplitPane exposing (view, Model, Msg, Orientation(..), Size, splitterPosition, subscriptions, update, init, startAt, draggable)

{-|

Lel

# View
@docs view

# Model
@docs Model, Msg, Orientation, Size, splitterPosition

# Init
@docs init, startAt, draggable

# Update
@docs update

# Subscriptions
@docs subscriptions

-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on)
import Mouse
import Json.Decode as Json
import Maybe


-- MODEL


{-| Size in pixels.
-}
type alias Size =
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
        { splitterPosition : Size
        , draggable : Bool
        , paneWidth : Size
        , paneHeight : Size
        , dragPosition : Maybe Mouse.Position
        , orientation : Orientation
        }


{-| Used to track resizes.
-}
type Msg
    = ResizeStarted Mouse.Position
    | Resize Mouse.Position
    | ResizeEnded Mouse.Position


{-| Retrieves current size from the model.
-}
splitterPosition : Model -> Size
splitterPosition (Model model) =
    model.splitterPosition


toCss : Size -> String
toCss size =
    toString size ++ "px"



-- INIT


{-| Initialize a new model.

        init
            { splitterPosition = 200
            , paneWidth = 600
            , paneHeight = 600
            , orientation = Horizontal
            }
-}
init :
    { a
        | orientation : Orientation
        , paneHeight : Size
        , paneWidth : Size
    }
    -> Model
init { paneWidth, paneHeight, orientation } =
    let
        startingSize =
            case orientation of
                Horizontal ->
                    paneWidth // 2

                Vertical ->
                    paneHeight // 2
    in
        Model
            { splitterPosition = startingSize
            , draggable = True
            , paneWidth = paneWidth
            , paneHeight = paneHeight
            , dragPosition = Nothing
            , orientation = orientation
            }


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
    Model { model | splitterPosition = startingSplitterPosition }


{-| Make pane splitter draggable or not

        init =
            SplitPane.init
                { paneWidth = 800
                , paneHeight = 600
                , orientation = Horizontal
                }
                |> draggable False
-}
draggable : Bool -> Model -> Model
draggable isDraggable (Model model) =
    Model { model | draggable = isDraggable }



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
    { splitterPosition : Size
    , dragPosition : Maybe Mouse.Position
    , orientation : Orientation
    , draggable : Bool
    , paneHeight : Size
    , paneWidth : Size
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

                newSize =
                    model.splitterPosition + diff
            in
                if isInLimits model newSize then
                    Model
                        { model
                            | splitterPosition = newSize
                            , dragPosition = Just newDragPosition
                        }
                else
                    Model model


isInLimits :
    { a
        | orientation : Orientation
        , paneHeight : Size
        , paneWidth : Size
    }
    -> Size
    -> Bool
isInLimits model newSize =
    case model.orientation of
        Horizontal ->
            newSize >= 0 && newSize <= model.paneWidth

        Vertical ->
            newSize >= 0 && newSize <= model.paneHeight



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
                    , ( "width", toCss model.paneWidth )
                    , ( "height", toCss model.paneHeight )
                    , ( "boxSizing", "border-box" )
                    ]
                ]
                [ div
                    [ style
                        [ ( "width", toCss model.splitterPosition )
                        , ( "overflow", "hidden" )
                        , ( "boxSizing", "border-box" )
                        , ( "position", "relative" )
                        ]
                    ]
                    [ firstView ]
                , defaultSplitter toMsg model.draggable model.orientation
                , div
                    [ style
                        [ ( "width", toCss <| model.paneWidth - model.splitterPosition )
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
                    , ( "width", toCss model.paneWidth )
                    , ( "height", toCss model.paneHeight )
                    , ( "boxSizing", "border-box" )
                    ]
                ]
                [ div
                    [ style
                        [ ( "height", toCss model.splitterPosition )
                        , ( "overflow", "hidden" )
                        , ( "boxSizing", "border-box" )
                        , ( "position", "relative" )
                        ]
                    ]
                    [ firstView ]
                , defaultSplitter toMsg model.draggable model.orientation
                , div
                    [ style
                        [ ( "height", toCss <| model.paneHeight - model.splitterPosition )
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
