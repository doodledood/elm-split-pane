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
        , percentage
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

@docs State, init, withSplitterAt, withResizeLimits, orientation, draggable, percentage

# Definitions

@docs Msg, Orientation, Percentage, ViewConfig, UpdateConfig, CustomSplitter, HtmlDetails

# Customization

@docs customUpdate, createUpdateConfig, createCustomSplitter
-}

import Html exposing (Html, span, div, Attribute)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onWithOptions)
import Mouse
import Json.Decode as Json exposing (Decoder, field, at)
import Maybe
import Bound exposing (Bound, Bounded, putValue, putBound, createBound, createBounded)
import Styles exposing (paneContainerStyle, childViewStyle, defaultHorizontalSplitterStyle, defaultVerticalSplitterStyle)


-- MODEL


{-| A percentage value between 0.0 and 1.0
-}
type alias Percentage =
    Bounded Float


{-| Orientation of pane.
-}
type Orientation
    = Horizontal
    | Vertical


type alias PaneDOMInfo =
    { width : Int
    , height : Int
    }


type alias DragInfo =
    { paneInfo : PaneDOMInfo
    , anchor : Position
    }


type DragState
    = Draggable (Maybe DragInfo)
    | NotDraggable


{-| Tracks state of pane.
-}
type State
    = State
        { orientation : Orientation
        , splitterPosition : Bounded Percentage
        , dragState : DragState
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
    State
        { state
            | dragState =
                if isDraggable then
                    Draggable Nothing
                else
                    NotDraggable
        }


{-| Changes orientation of the pane.
-}
orientation : Orientation -> State -> State
orientation o (State state) =
    State { state | orientation = o }


{-| Changes the splitter position
-}
withSplitterAt : Percentage -> State -> State
withSplitterAt newPosition (State state) =
    State
        { state
            | splitterPosition = putValue state.splitterPosition newPosition
        }


{-| Changes resizes limits
-}
withResizeLimits : Bound Percentage -> State -> State
withResizeLimits newResizeLimit (State state) =
    State
        { state
            | splitterPosition =
                putBound state.splitterPosition newResizeLimit
        }


{-| Creates a percentage from a float
-}
percentage : Float -> Percentage
percentage x =
    createBounded x <| createBound 0.0 1.0


getPercentage : Bounded Percentage -> Float
getPercentage =
    Tuple.first << Tuple.first



-- INIT


setPercentage : Float -> Bounded Percentage -> Bounded Percentage
setPercentage x ( p, bound ) =
    createBounded (putValue p x) bound


{-| Initialize a new model.

        init Horizontal
-}
init : Orientation -> State
init orientation =
    State
        { orientation = orientation
        , splitterPosition = ( percentage 0.5, createBound (percentage 0.0) (percentage 1.0) )
        , dragState = Draggable Nothing
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
        { onResize : Float -> Maybe msg
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
    { onResize : Float -> Maybe msg
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
    case ( state.dragState, msg ) of
        ( Draggable Nothing, SplitterClick pos ) ->
            ( State
                { state
                    | dragState =
                        Draggable <|
                            Just
                                { paneInfo =
                                    { width = pos.parentWidth
                                    , height = pos.parentHeight
                                    }
                                , anchor =
                                    { x = Maybe.withDefault 0 pos.x
                                    , y = Maybe.withDefault 0 pos.y
                                    }
                                }
                }
            , updateConfig.onResizeStarted
            )

        ( Draggable (Just _), SplitterLeftAlone _ ) ->
            ( State { state | dragState = Draggable Nothing }
            , updateConfig.onResizeEnded
            )

        ( Draggable (Just { paneInfo, anchor }), SplitterMove newRequestedPosition ) ->
            let
                step =
                    { x = newRequestedPosition.x - anchor.x
                    , y = newRequestedPosition.y - anchor.y
                    }

                newSplitterPosition =
                    resize state.orientation state.splitterPosition step paneInfo.width paneInfo.height
            in
                ( State
                    { state
                        | splitterPosition = newSplitterPosition
                        , dragState =
                            Draggable <|
                                Just
                                    { paneInfo =
                                        { width = paneInfo.width
                                        , height = paneInfo.height
                                        }
                                    , anchor =
                                        { x = newRequestedPosition.x
                                        , y = newRequestedPosition.y
                                        }
                                    }
                    }
                , updateConfig.onResize <| getPercentage newSplitterPosition
                )

        _ ->
            ( State state, Nothing )


resize : Orientation -> Bounded Percentage -> Position -> Int -> Int -> Bounded Percentage
resize orientation splitterPosition step paneWidth paneHeight =
    case orientation of
        Horizontal ->
            let
                newSplitterValue =
                    getPercentage splitterPosition + toFloat step.x / toFloat paneWidth
            in
                setPercentage newSplitterValue splitterPosition

        Vertical ->
            let
                newSplitterValue =
                    getPercentage splitterPosition + toFloat step.y / toFloat paneHeight
            in
                setPercentage newSplitterValue splitterPosition



-- VIEW


{-| Lets you specify attributes such as style and children for the splitter element
-}
type alias HtmlDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


{-| Describes a custom splitter
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
            createCustomSplitter PaneMsg
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


{-| Creates a view.

        view : Model -> Html Msg
        view =
            SplitPane.view viewConfig firstView secondView


        viewConfig : ViewConfig Msg
        viewConfig =
            createViewConfig
                { toMsg = PaneMsg
                , customSplitter = Nothing
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
    let
        splitter =
            case state.dragState of
                Draggable _ ->
                    getConcreteSplitter viewConfig state.orientation True

                NotDraggable ->
                    getConcreteSplitter viewConfig state.orientation False
    in
        div
            [ class "pane-container"
            , paneContainerStyle <| state.orientation == Horizontal
            ]
            [ div
                [ class "pane-first-view"
                , childViewStyle <| getPercentage state.splitterPosition
                ]
                [ firstView ]
            , splitter
            , div
                [ class "pane-second-view"
                , childViewStyle <| 1 - getPercentage state.splitterPosition
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


{-| The decoder used to extract a `DOMInfo` from a JavaScript touch event.
-}
domInfo : Json.Decoder DOMInfo
domInfo =
    Json.map6 DOMInfo
        (Json.maybe (field "clientX" Json.int))
        (Json.maybe (field "clientY" Json.int))
        (Json.maybe (at [ "touches", "0", "clientX" ] Json.int))
        (Json.maybe (at [ "touches", "0", "clientY" ] Json.int))
        (at [ "currentTarget", "parentElement", "clientWidth" ] Json.int)
        (at [ "currentTarget", "parentElement", "clientHeight" ] Json.int)



-- SUBSCRIPTIONS


{-| Subscribes to relevant events for resizing
-}
subscriptions : State -> Sub Msg
subscriptions (State state) =
    case state.dragState of
        Draggable (Just _) ->
            Sub.batch
                [ Mouse.moves SplitterMove
                , Mouse.ups SplitterLeftAlone
                ]

        _ ->
            Sub.none
