module SplitPane
    exposing
        ( view
        , ViewConfig
        , createViewConfig
        , createCustomSplitter
        , CustomSplitter
        , HtmlDetails
        , State
        , Msg
        , Orientation(..)
        , Percentage
        , subscriptions
        , update
        , customUpdate
        , UpdateConfig
        , createUpdateConfig
        , init
        , withResizeLimits
        , withSplitterAt
        , orientation
        , draggable
        )

{-|

This is a split pane view library. Can be used to split views into multiple parts with a splitter between them.

Check out the [examples][] to see how it works.

[examples]: https://github.com/doodledood/elm-split-pane/tree/master/examples

# View

@docs view, createViewConfig

# Update

@docs update, subscriptions

# State

@docs State, init, withSplitterAt, withResizeLimits, orientation, draggable

# Definitions

@docs Msg, Orientation, Percentage, ViewConfig, UpdateConfig, CustomSplitter, HtmlDetails

# Customization

@docs customUpdate, createUpdateConfig, createCustomSplitter
-}

import Html exposing (Html, span, div, Attribute)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onWithOptions)
import Mouse
import Json.Decode as Json exposing (Decoder, (:=), at)
import Maybe
import Styles exposing (paneContainerStyle, childViewStyle, defaultHorizontalSplitterStyle, defaultVerticalSplitterStyle)


-- MODEL


{-| A percentage value between 0.0 and 1.0
-}
type alias Percentage =
    Float


{-| Orientation of pane.
-}
type Orientation
    = Horizontal
    | Vertical


{-| Tracks state of pane.
-}
type State
    = State
        { dragPosition : Maybe Position
        , draggable : Bool
        , orientation : Orientation
        , splitterPosition : Percentage
        , resizeLimits : ( Percentage, Percentage )
        , paneWidth : Maybe Int
        , paneHeight : Maybe Int
        }


{-| Internal messages.
-}
type Msg
    = SplitterClick DOMInfo
    | SplitterMove Position
    | SplitterLeftAlone Position


type alias Position =
    { x : Int
    , y : Int
    }


{-| Sets whether the pane is draggable or not
-}
draggable : Bool -> State -> State
draggable isDraggable (State state) =
    State { state | draggable = isDraggable }


{-| Changes orientation of the pane.
-}
orientation : Orientation -> State -> State
orientation o (State state) =
    State { state | orientation = o }


{-| Changes the splitter position
-}
withSplitterAt : Percentage -> State -> State
withSplitterAt newPosition (State state) =
    State { state | splitterPosition = min 1.0 <| max newPosition 0.0 }


{-| Changes resizes limits
-}
withResizeLimits : Percentage -> Percentage -> State -> State
withResizeLimits minLimit maxLimit (State state) =
    State { state | resizeLimits = ( minLimit, maxLimit ) }



-- INIT


{-| Initialize a new model.

        init Horizontal
-}
init : Orientation -> State
init orientation =
    State
        { dragPosition = Nothing
        , draggable = True
        , orientation = orientation
        , splitterPosition = 0.5
        , resizeLimits = ( 0.0, 1.0 )
        , paneWidth = Nothing
        , paneHeight = Nothing
        }



-- UPDATE


domInfoToPosition : DOMInfo -> Position
domInfoToPosition { x, y, touchX, touchY, parentWidth, parentHeight } =
    case ( x, y, touchX, touchY ) of
        ( _, _, Just posX, Just posY ) ->
            { x = posX, y = posY }

        ( Just posX, Just posY, _, _ ) ->
            { x = posX, y = posY }

        _ ->
            { x = 0, y = 0 }


{-| Configuration for updates.
-}
type UpdateConfig msg
    = UpdateConfig
        { onResize : Percentage -> Maybe msg
        , onResizeStarted : Maybe msg
        , onResizeEnded : Maybe msg
        }


{-| Creates the update configuration.
    Gives you the option to respond to various things that happen.

    For example:
    - Draw a different view when the pane is resized:

        updateConfig
            { onResize (\p -> Just (SwitchViews p))
            , onResizeStarted Nothing
            , onResizeEnded Nothing
            }
-}
createUpdateConfig :
    { onResize : Percentage -> Maybe msg
    , onResizeStarted : Maybe msg
    , onResizeEnded : Maybe msg
    }
    -> UpdateConfig msg
createUpdateConfig config =
    UpdateConfig config


{-| Updates internal model.
-}
update : Msg -> State -> State
update msg model =
    let
        ( updatedModel, _ ) =
            customUpdate
                (createUpdateConfig
                    { onResize = \_ -> Nothing
                    , onResizeStarted = Nothing
                    , onResizeEnded = Nothing
                    }
                )
                msg
                model
    in
        updatedModel


{-| Updates internal model using custom configuration.
-}
customUpdate : UpdateConfig msg -> Msg -> State -> ( State, Maybe msg )
customUpdate (UpdateConfig updateConfig) msg (State state) =
    if not state.draggable then
        ( State state, Nothing )
    else
        case msg of
            SplitterClick pos ->
                ( State
                    { state
                        | dragPosition = Just <| domInfoToPosition pos
                        , paneWidth = Just pos.parentWidth
                        , paneHeight = Just pos.parentHeight
                    }
                , updateConfig.onResizeStarted
                )

            SplitterLeftAlone _ ->
                ( State { state | dragPosition = Nothing }
                , updateConfig.onResizeEnded
                )

            SplitterMove curr ->
                case state.dragPosition of
                    Nothing ->
                        ( State state, Nothing )

                    Just dragPos ->
                        let
                            ( minLimit, maxLimit ) =
                                state.resizeLimits

                            ( newSplitterPosition, newPosition ) =
                                resize state.orientation state.splitterPosition curr dragPos state.paneWidth state.paneHeight minLimit maxLimit
                        in
                            ( State
                                { state
                                    | dragPosition = Just newPosition
                                    , splitterPosition = newSplitterPosition
                                }
                            , updateConfig.onResize newSplitterPosition
                            )


resize : Orientation -> Percentage -> Position -> Position -> Maybe Int -> Maybe Int -> Percentage -> Percentage -> ( Percentage, Position )
resize orientation splitterPosition newPosition prevPosition paneWidth paneHeight minLimit maxLimit =
    case ( paneWidth, paneHeight ) of
        ( Just width, Just height ) ->
            case orientation of
                Horizontal ->
                    let
                        newSplitterPosition =
                            splitterPosition + toFloat (newPosition.x - prevPosition.x) / toFloat width
                    in
                        if newSplitterPosition < minLimit then
                            ( minLimit, { x = round <| toFloat width * minLimit, y = newPosition.y } )
                        else if newSplitterPosition > maxLimit then
                            ( maxLimit, { x = round <| toFloat width * maxLimit, y = newPosition.y } )
                        else
                            ( newSplitterPosition, newPosition )

                Vertical ->
                    let
                        newSplitterPosition =
                            splitterPosition + toFloat (newPosition.y - prevPosition.y) / toFloat height
                    in
                        if newSplitterPosition < minLimit then
                            ( minLimit, { x = newPosition.x, y = round <| toFloat height * minLimit } )
                        else if newSplitterPosition > maxLimit then
                            ( maxLimit, { x = newPosition.x, y = round <| toFloat height * maxLimit } )
                        else
                            ( newSplitterPosition, newPosition )

        ( _, _ ) ->
            ( splitterPosition, newPosition )



-- VIEW


{-| Lets you specify attributes such as style and children for the splitter element
-}
type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


{-| Decribes a custom splitter
-}
type CustomSplitter msg
    = CustomSplitter (Html msg)


createDefaultSplitterDetails : Orientation -> Bool -> HtmlDetails msg
createDefaultSplitterDetails orientation draggable =
    case orientation of
        Horizontal ->
            { attributes =
                [ defaultHorizontalSplitterStyle draggable
                ]
            , children = []
            }

        Vertical ->
            { attributes =
                [ defaultVerticalSplitterStyle draggable
                ]
            , children = []
            }


{-| Creates a custom splitter.

        myCustomSplitter : CustomSplitter Msg
        myCustomSplitter =
            customSplitter PaneMsg
                { attributes =
                    [ style
                        [ ( "width", "20px" )
                        , ( "height", "20px" )
                        ]
                    ]
                , children =
                    []
                }
-}
createCustomSplitter :
    (Msg -> msg)
    -> HtmlDetails msg
    -> CustomSplitter msg
createCustomSplitter toMsg details =
    CustomSplitter <|
        span
            (onMouseDown toMsg :: onTouchStart toMsg :: onTouchEnd toMsg :: onTouchMove toMsg :: onTouchCancel toMsg :: details.attributes)
            details.children


{-| Configuration for the view.
-}
type ViewConfig msg
    = ViewConfig
        { toMsg : Msg -> msg
        , splitter : Maybe (CustomSplitter msg)
        }


{-| Creates a configuration for the view.
-}
createViewConfig :
    { toMsg : Msg -> msg
    , customSplitter : Maybe (CustomSplitter msg)
    }
    -> ViewConfig msg
createViewConfig { toMsg, customSplitter } =
    ViewConfig
        { toMsg = toMsg
        , splitter = customSplitter
        }


{-| A pane with custom splitter.

        view : Model -> Html Msg
        view =
            SplitPane.view viewConfig firstView secondView


        viewConfig : ViewConfig Msg
        viewConfig =
            createViewConfig
                { toMsg = PaneMsg
                , customSplitter = Nothing
                }


        myCustomSplitter : CustomSplitter Msg
        myCustomSplitter =
            customSplitter PaneMsg
                { attributes =
                    [ style
                        [ ( "width", "20px" )
                        , ( "height", "20px" )
                        ]
                    ]
                , children =
                    []
                }


        firstView : Html a
        firstView =
            img [ src "http://4.bp.blogspot.com/-s3sIvuCfg4o/VP-82RkCOGI/AAAAAAAALSY/509obByLvNw/s1600/baby-cat-wallpaper.jpg" ] []


        secondView : Html a
        secondView =
            img [ src "http://2.bp.blogspot.com/-pATX0YgNSFs/VP-82AQKcuI/AAAAAAAALSU/Vet9e7Qsjjw/s1600/Cat-hd-wallpapers.jpg" ] []
-}
view : ViewConfig msg -> Html msg -> Html msg -> State -> Html msg
view (ViewConfig viewConfig) firstView secondView (State state) =
    div
        [ class "pane-container"
        , paneContainerStyle <| state.orientation == Horizontal
        ]
        [ div
            [ class "pane-first-view"
            , childViewStyle state.splitterPosition
            ]
            [ firstView ]
        , getConcreteSplitter viewConfig state.orientation state.draggable
        , div
            [ class "pane-second-view"
            , childViewStyle <| 1 - state.splitterPosition
            ]
            [ secondView ]
        ]


getConcreteSplitter :
    { toMsg : Msg -> msg
    , splitter : Maybe (CustomSplitter msg)
    }
    -> Orientation
    -> Bool
    -> Html msg
getConcreteSplitter viewConfig orientation draggable =
    case viewConfig.splitter of
        Just (CustomSplitter splitter) ->
            splitter

        Nothing ->
            case createCustomSplitter viewConfig.toMsg <| createDefaultSplitterDetails orientation draggable of
                CustomSplitter defaultSplitter ->
                    defaultSplitter


onMouseDown : (Msg -> msg) -> Attribute msg
onMouseDown toMsg =
    onWithOptions "mousedown" { preventDefault = True, stopPropagation = False } <| Json.map (toMsg << SplitterClick) domInfo


onTouchStart : (Msg -> msg) -> Attribute msg
onTouchStart toMsg =
    onWithOptions "touchstart" { preventDefault = True, stopPropagation = True } <| Json.map (toMsg << SplitterClick) domInfo


onTouchEnd : (Msg -> msg) -> Attribute msg
onTouchEnd toMsg =
    onWithOptions "touchend" { preventDefault = True, stopPropagation = True } <| Json.map (toMsg << SplitterLeftAlone << domInfoToPosition) domInfo


onTouchCancel : (Msg -> msg) -> Attribute msg
onTouchCancel toMsg =
    onWithOptions "touchcancel" { preventDefault = True, stopPropagation = True } <| Json.map (toMsg << SplitterLeftAlone << domInfoToPosition) domInfo


onTouchMove : (Msg -> msg) -> Attribute msg
onTouchMove toMsg =
    onWithOptions "touchmove" { preventDefault = True, stopPropagation = True } <| Json.map (toMsg << SplitterMove << domInfoToPosition) domInfo


{-| The position of the touch relative to the whole document. So if you are
scrolled down a bunch, you are still getting a coordinate relative to the
very top left corner of the *whole* document.
-}
type alias DOMInfo =
    { x : Maybe Int
    , y : Maybe Int
    , touchX : Maybe Int
    , touchY : Maybe Int
    , parentWidth : Int
    , parentHeight : Int
    }


{-| The decoder used to extract a `Position` from a JavaScript touch event.
-}
domInfo : Json.Decoder DOMInfo
domInfo =
    Json.object6 DOMInfo
        (Json.maybe ("clientX" := Json.int))
        (Json.maybe ("clientY" := Json.int))
        (Json.maybe (at [ "touches", "0", "clientX" ] Json.int))
        (Json.maybe (at [ "touches", "0", "clientY" ] Json.int))
        (at [ "target", "parentElement", "clientWidth" ] Json.int)
        (at [ "target", "parentElement", "clientHeight" ] Json.int)



-- SUBSCRIPTIONS


{-| Subscribes to relevant events for resizing
-}
subscriptions : State -> Sub Msg
subscriptions (State state) =
    if not state.draggable then
        Sub.none
    else
        case state.dragPosition of
            Just _ ->
                Sub.batch
                    [ Mouse.moves SplitterMove
                    , Mouse.ups SplitterLeftAlone
                    ]

            Nothing ->
                Sub.none
