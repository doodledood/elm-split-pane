module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.App exposing (program)
import SplitPane exposing (Px, WhatHappened(..))


main : Program Never
main =
    program
        { update = update
        , init = init
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type ViewSize
    = Small
    | Medium
    | Large


type alias Model =
    { pane : SplitPane.Model
    , leftViewSize : ViewSize
    , rightViewSize : ViewSize
    }


type Msg
    = PaneMsg SplitPane.Msg



-- INIT


init : ( Model, Cmd a )
init =
    ( { pane =
            SplitPane.init
                { paneWidth = 800
                , paneHeight = 600
                }
      , leftViewSize = Medium
      , rightViewSize = Medium
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        PaneMsg paneMsg ->
            let
                ( updatedPane, whatHappened ) =
                    SplitPane.update paneMsg model.pane
            in
                case whatHappened of
                    Just (Resized newPosition) ->
                        let
                            ( leftViewNewSize, rightViewNewSize ) =
                                chooseViewSizesBasedOnSplitterPosition newPosition
                        in
                            ( { model
                                | pane = updatedPane
                                , leftViewSize = leftViewNewSize
                                , rightViewSize = rightViewNewSize
                              }
                            , Cmd.none
                            )

                    _ ->
                        ( { model | pane = updatedPane }, Cmd.none )


chooseViewSizesBasedOnSplitterPosition : Px -> ( ViewSize, ViewSize )
chooseViewSizesBasedOnSplitterPosition splitterPosition =
    if splitterPosition < 200 then
        ( Small, Large )
    else if splitterPosition < 600 then
        ( Medium, Medium )
    else
        ( Large, Small )



-- VIEW


view : Model -> Html Msg
view model =
    let
        ( firstView, secondView ) =
            ( toView model.leftViewSize, toView model.rightViewSize )
    in
        SplitPane.view PaneMsg firstView secondView model.pane


toView : ViewSize -> Html a
toView size =
    case size of
        Small ->
            smallView

        Medium ->
            mediumView

        Large ->
            largeView


smallView : Html a
smallView =
    div
        [ style
            [ ( "background", "lightblue" ) ]
        ]
        [ text "small" ]


mediumView : Html a
mediumView =
    div
        [ style
            [ ( "background", "lightgreen" ) ]
        ]
        [ text "medium" ]


largeView : Html a
largeView =
    div
        [ style
            [ ( "background", "lightcoral" ) ]
        ]
        [ text "large" ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PaneMsg <| SplitPane.subscriptions model.pane
