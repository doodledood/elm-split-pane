module SplitPane
    exposing
        ( view
        , viewWithCustomSplitter
        , customSplitter
        , CustomSplitter
        , HtmlDetails
        , Model
        , Msg
        , WhatHappened(..)
        , Orientation(..)
        , Px
        , Size(..)
        , subscriptions
        , update
        , init
        )

{-|

This is a split pane view library. Can be used to split views into multiple parts with a splitter between them.

Check out the [examples][] to see how it works.

[examples]: https://github.com/doodledood/elm-split-pane/tree/master/examples

# View

@docs view

# Model

@docs Model, Orientation, Px, Size

# Init

@docs init

# Update

@docs update, WhatHappened, Msg

# Subscriptions

@docs subscriptions

# Customization

Apart for the simple view, there is a way to provide your own custom splitter:

@docs viewWithCustomSplitter, customSplitter, CustomSplitter, HtmlDetails

-}

import Html exposing (Html, span, div, Attribute)
import Html.Attributes exposing (style, class)
import Html.Events exposing (onWithOptions)
import Mouse
import Json.Decode as Json exposing (Decoder, (:=), at)
import Maybe
import Styles exposing (paneContainerStyle, childViewStyle)


-- MODEL


{-| Size in either pixels or percentage.
-}
type Size
    = Px Int
    | Percentage Float


{-| Size in pixels.
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
        { splitterPosition : Float
        , draggable : Bool
        , firstViewMinSize : Size
        , secondViewMinSize : Size
        , paneWidth : Maybe Px
        , paneHeight : Maybe Px
        , dragPosition : Maybe Position
        , orientation : Orientation
        }


{-| Used to track SplitterMoves.
-}
type Msg
    = SplitterClick DOMInfo
    | SplitterMove Position
    | SplitterLeftAlone Position


{-| Describes what happened. (after update)
-}
type WhatHappened
    = ResizeStarted
    | Resized Float
    | ResizeEnded


type alias Position =
    { x : Int
    , y : Int
    }



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
        { splitterPosition = 0.5
        , draggable = True
        , firstViewMinSize = Px 0
        , secondViewMinSize = Px 0
        , paneWidth = Nothing
        , paneHeight = Nothing
        , dragPosition = Nothing
        , orientation = orientation
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


{-| Updates internal model.
-}
update : Msg -> Model -> ( Model, Maybe WhatHappened )
update msg (Model model) =
    if not model.draggable then
        ( Model model, Nothing )
    else
        case msg of
            SplitterClick pos ->
                ( Model
                    { model
                        | dragPosition = Just <| domInfoToPosition pos
                        , paneWidth = Just pos.parentWidth
                        , paneHeight = Just pos.parentHeight
                    }
                , Just ResizeStarted
                )

            SplitterLeftAlone _ ->
                ( Model { model | dragPosition = Nothing }, Just ResizeEnded )

            SplitterMove curr ->
                case model.dragPosition of
                    Nothing ->
                        ( Model model, Nothing )

                    Just dragPos ->
                        let
                            newSplitterPosition =
                                resize model.orientation model.splitterPosition curr dragPos model.paneWidth model.paneHeight
                        in
                            ( Model
                                { model
                                    | dragPosition = Just curr
                                    , splitterPosition = newSplitterPosition
                                }
                            , Just <| Resized newSplitterPosition
                            )


resize : Orientation -> Float -> Position -> Position -> Maybe Int -> Maybe Int -> Float
resize orientation splitterPosition newPosition prevPosition paneWidth paneHeight =
    case ( paneWidth, paneHeight ) of
        ( Just width, Just height ) ->
            case orientation of
                Horizontal ->
                    splitterPosition + toFloat (newPosition.x - prevPosition.x) / toFloat width

                Vertical ->
                    splitterPosition + toFloat (newPosition.y - prevPosition.y) / toFloat height

        ( _, _ ) ->
            splitterPosition



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


createDefaultSplitterDetails : Model -> HtmlDetails msg
createDefaultSplitterDetails (Model model) =
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
    in
        case model.orientation of
            Horizontal ->
                { attributes =
                    [ style
                        (baseStyles
                            ++ [ ( "width", "11px" )
                               , ( "height", "100%" )
                               , ( "margin", "0 -5px" )
                               , ( "borderLeft", "5px solid rgba(255, 255, 255, 0)" )
                               , ( "borderRight", "5px solid rgba(255, 255, 255, 0)" )
                               ]
                            ++ if model.draggable then
                                [ ( "cursor", "col-resize" ) ]
                               else
                                []
                        )
                    ]
                , children = []
                }

            Vertical ->
                { attributes =
                    [ style
                        (baseStyles
                            ++ [ ( "height", "11px" )
                               , ( "width", "100%" )
                               , ( "margin", "-5px 0" )
                               , ( "borderTop", "5px solid rgba(255, 255, 255, 0)" )
                               , ( "borderBottom", "5px solid rgba(255, 255, 255, 0)" )
                               ]
                            ++ if model.draggable then
                                [ ( "cursor", "row-resize" ) ]
                               else
                                []
                        )
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
customSplitter :
    (Msg -> msg)
    -> (Model -> HtmlDetails msg)
    -> CustomSplitter msg
customSplitter toMsg createDetails =
    CustomSplitter <|
        (\model ->
            let
                details =
                    createDetails model
            in
                span
                    (onMouseDown toMsg :: onTouchStart toMsg :: onTouchEnd toMsg :: onTouchMove toMsg :: onTouchCancel toMsg :: details.attributes)
                    details.children
        )


{-| Default pane with two views

        view : Model -> Html Msg
        view model =
            SplitPane.view PaneMsg firstView secondView model.pane


        firstView : Html a
        firstView =
            img [ src "http://4.bp.blogspot.com/-s3sIvuCfg4o/VP-82RkCOGI/AAAAAAAALSY/509obByLvNw/s1600/baby-cat-wallpaper.jpg" ] []


        secondView : Html a
        secondView =
            img [ src "http://2.bp.blogspot.com/-pATX0YgNSFs/VP-82AQKcuI/AAAAAAAALSU/Vet9e7Qsjjw/s1600/Cat-hd-wallpapers.jpg" ] []
-}
view : (Msg -> msg) -> Html msg -> Html msg -> Model -> Html msg
view toMsg firstView secondView model =
    let
        defaultSplitter =
            customSplitter toMsg createDefaultSplitterDetails
    in
        viewWithCustomSplitter defaultSplitter firstView secondView model


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
viewWithCustomSplitter : CustomSplitter msg -> Html msg -> Html msg -> Model -> Html msg
viewWithCustomSplitter (CustomSplitter createSplitter) firstView secondView ((Model model) as m) =
    div
        [ class "pane-container"
        , paneContainerStyle <| model.orientation == Horizontal
        ]
        [ div
            [ class "pane-first-view"
            , childViewStyle model.splitterPosition
            ]
            [ firstView ]
        , createSplitter m
        , div
            [ class "pane-second-view"
            , childViewStyle <| 1 - model.splitterPosition
            ]
            [ secondView ]
        ]


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
subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    if not model.draggable then
        Sub.none
    else
        case model.dragPosition of
            Just _ ->
                Sub.batch
                    [ Mouse.moves SplitterMove
                    , Mouse.ups SplitterLeftAlone
                    ]

            Nothing ->
                Sub.none
