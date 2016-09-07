module SplitPane
    exposing
        ( view
        , ViewConfig
        , createViewConfig
        , createCustomSplitter
        , CustomSplitter
        , HtmlDetails
        , Model
        , Orientation(..)
        , Percentage
        , draggable
        , withResizeLimits
        , withSplitterAt
        , orientation
        , subscriptions
        , init
        )

{-|

This is a split pane view library. Can be used to split views into multiple parts with a splitter between them.

Check out the [examples][] to see how it works.

[examples]: https://github.com/doodledood/elm-split-pane/tree/master/examples

@docs view, ViewConfig, createViewConfig, createCustomSplitter, CustomSplitter, HtmlDetails, Model, Orientation, Percentage, draggable, withResizeLimits, withSplitterAt, orientation, subscriptions, init
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
type Model
    = Model
        { dragPosition : Maybe Position
        , draggable : Bool
        , orientation : Orientation
        , splitterPosition : Percentage
        , resizeLimits : ( Percentage, Percentage )
        , paneWidth : Maybe Int
        , paneHeight : Maybe Int
        }


type alias Position =
    { x : Int
    , y : Int
    }


{-| Sets whether the pane is draggable or not
-}
draggable : Bool -> Model -> Model
draggable isDraggable (Model model) =
    Model { model | draggable = isDraggable }


{-| Changes orientation of the pane.
-}
orientation : Orientation -> Model -> Model
orientation o (Model model) =
    Model { model | orientation = o }


{-| Changes the splitter position
-}
withSplitterAt : Percentage -> Model -> Model
withSplitterAt newPosition (Model model) =
    Model
        { model | splitterPosition = min 1.0 <| max newPosition 0.0 }


{-| Changes resizes limits
-}
withResizeLimits : Percentage -> Percentage -> Model -> Model
withResizeLimits minLimit maxLimit (Model model) =
    Model
        { model | resizeLimits = ( minLimit, maxLimit ) }



-- INIT


{-| Initialize a new model.

        init
            { paneWidth = 600
            , paneHeight = 600
            }
-}
init : Orientation -> Model
init orientation =
    Model
        { dragPosition = Nothing
        , draggable = True
        , orientation = orientation
        , splitterPosition = 0.5
        , resizeLimits = ( 0.0, 1.0 )
        , paneWidth = Nothing
        , paneHeight = Nothing
        }


resize : Orientation -> Percentage -> Position -> Position -> Maybe Int -> Maybe Int -> Percentage -> Percentage -> ( Bool, Percentage )
resize orientation splitterPosition newPosition prevPosition paneWidth paneHeight minLimit maxLimit =
    case ( paneWidth, paneHeight ) of
        ( Just width, Just height ) ->
            case orientation of
                Horizontal ->
                    let
                        newSplitterPosition =
                            splitterPosition + toFloat (newPosition.x - prevPosition.x) / toFloat width
                    in
                        checkLimits newSplitterPosition minLimit maxLimit

                Vertical ->
                    let
                        newSplitterPosition =
                            splitterPosition + toFloat (newPosition.y - prevPosition.y) / toFloat height
                    in
                        checkLimits newSplitterPosition minLimit maxLimit

        ( _, _ ) ->
            ( False, splitterPosition )


checkLimits : Percentage -> Float -> Float -> ( Bool, Percentage )
checkLimits newSplitterPosition minLimit maxLimit =
    if newSplitterPosition > maxLimit || newSplitterPosition < minLimit then
        ( True, min maxLimit <| max minLimit newSplitterPosition )
    else
        ( False, newSplitterPosition )



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
    = CustomSplitter (Model -> Html msg)


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
    (Model -> msg)
    -> HtmlDetails msg
    -> CustomSplitter msg
createCustomSplitter toMsg details =
    CustomSplitter <|
        \model ->
            span
                (onMouseDown toMsg
                    model
                    :: onTouchStart toMsg model
                    :: onMouseUp toMsg model
                    :: onTouchEnd toMsg model
                    :: onTouchMove toMsg model
                    :: onTouchCancel toMsg model
                    :: details.attributes
                )
                details.children


{-| Configuration for the view.
-}
type ViewConfig msg
    = ViewConfig
        { toMsg : Model -> msg
        , splitter : Maybe (CustomSplitter msg)
        }


{-| Creates a configuration for the view.

-}
createViewConfig :
    { toMsg : Model -> msg
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
            SplitPane.viewWithCustomSplitter myCustomSplitter firstView secondView


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
view : ViewConfig msg -> Html msg -> Html msg -> Model -> Html msg
view (ViewConfig viewConfig) firstView secondView ((Model model) as m) =
    div
        [ class "pane-container"
        , onMouseLeave viewConfig.toMsg m
        , paneContainerStyle <| model.orientation == Horizontal
        ]
        [ div
            [ class "pane-first-view"
            , childViewStyle model.splitterPosition
            ]
            [ firstView ]
        , getConcreteSplitter viewConfig m
        , div
            [ class "pane-second-view"
            , childViewStyle <| 1 - model.splitterPosition
            ]
            [ secondView ]
        ]


getConcreteSplitter :
    { toMsg : Model -> msg
    , splitter : Maybe (CustomSplitter msg)
    }
    -> Model
    -> Html msg
getConcreteSplitter viewConfig ((Model model) as m) =
    case viewConfig.splitter of
        Just (CustomSplitter splitter) ->
            splitter m

        Nothing ->
            case createCustomSplitter viewConfig.toMsg <| createDefaultSplitterDetails model.orientation model.draggable of
                CustomSplitter defaultSplitter ->
                    defaultSplitter m


onMouseDown : (Model -> msg) -> Model -> Attribute msg
onMouseDown toMsg model =
    onWithOptions "mousedown" { preventDefault = True, stopPropagation = False } <| Json.map (toMsg << startDrag model) domInfo


onTouchStart : (Model -> msg) -> Model -> Attribute msg
onTouchStart toMsg model =
    onWithOptions "touchstart" { preventDefault = True, stopPropagation = False } <| Json.map (toMsg << startDrag model) domInfo


startDrag : Model -> DOMInfo -> Model
startDrag (Model model) domInfo =
    Model
        { model
            | dragPosition = Just <| domInfoToPosition domInfo
            , paneWidth = Just domInfo.parentWidth
            , paneHeight = Just domInfo.parentHeight
        }


{-| BUG: This is here because there is a weird behavior regarding move events on a global document.
         It causes both move and up events to fire together when the mouse is released causing the up event to not register
         and therefore not stop the resize process.
         This event handler is the solution meanwhile, as dragin outside the pane is now not allowed....

         See progress on [Github Issue][]
         [Github Issue]: https://github.com/elm-lang/mouse/issues/2
-}
onMouseLeave : (Model -> msg) -> Model -> Attribute msg
onMouseLeave toMsg model =
    onWithOptions "mouseleave" { preventDefault = True, stopPropagation = False } <| Json.map (toMsg << endDrag model << domInfoToPosition) domInfo


onMouseUp : (Model -> msg) -> Model -> Attribute msg
onMouseUp toMsg model =
    onWithOptions "mouseup" { preventDefault = True, stopPropagation = False } <| Json.map (toMsg << endDrag model << domInfoToPosition) domInfo


onTouchEnd : (Model -> msg) -> Model -> Attribute msg
onTouchEnd toMsg model =
    onWithOptions "touchend" { preventDefault = True, stopPropagation = False } <| Json.map (toMsg << endDrag model << domInfoToPosition) domInfo


onTouchCancel : (Model -> msg) -> Model -> Attribute msg
onTouchCancel toMsg model =
    onWithOptions "touchcancel" { preventDefault = True, stopPropagation = False } <| Json.map (toMsg << endDrag model << domInfoToPosition) domInfo


endDrag : Model -> Position -> Model
endDrag (Model model) _ =
    Model { model | dragPosition = Nothing }


onTouchMove : (Model -> msg) -> Model -> Attribute msg
onTouchMove toMsg model =
    onWithOptions "touchmove" { preventDefault = True, stopPropagation = False } <| Json.map (toMsg << moveSplitter model << domInfoToPosition) domInfo


moveSplitter : Model -> Position -> Model
moveSplitter (Model model) curr =
    case model.dragPosition of
        Nothing ->
            Model model

        Just dragPos ->
            let
                ( minLimit, maxLimit ) =
                    model.resizeLimits

                ( shouldStopDrag, newSplitterPosition ) =
                    resize model.orientation model.splitterPosition curr dragPos model.paneWidth model.paneHeight minLimit maxLimit
            in
                Model
                    { model
                        | dragPosition =
                            if shouldStopDrag then
                                Nothing
                            else
                                Just curr
                        , splitterPosition = newSplitterPosition
                    }


domInfoToPosition : DOMInfo -> Position
domInfoToPosition { x, y, touchX, touchY, parentWidth, parentHeight } =
    case ( x, y, touchX, touchY ) of
        ( _, _, Just posX, Just posY ) ->
            { x = posX, y = posY }

        ( Just posX, Just posY, _, _ ) ->
            { x = posX, y = posY }

        _ ->
            { x = 0, y = 0 }


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
subscriptions : (Model -> msg) -> Model -> Sub msg
subscriptions toMsg ((Model model) as m) =
    if not model.draggable then
        Sub.none
    else
        case model.dragPosition of
            Just _ ->
                Sub.batch
                    [ Mouse.ups <| toMsg << endDrag m
                    , Mouse.moves <| toMsg << moveSplitter m
                    ]

            Nothing ->
                Sub.none
