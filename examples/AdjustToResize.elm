module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.App exposing (program)
import SplitPane exposing (Px, splitterPosition)


main : Program Never
main =
    program
        { update = update
        , init = init
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : SplitPane.Model -> Sub SplitPane.Msg
subscriptions =
    SplitPane.subscriptions


init : ( SplitPane.Model, Cmd a )
init =
    ( SplitPane.init
        { paneWidth = 800
        , paneHeight = 600
        }
    , Cmd.none
    )


update : SplitPane.Msg -> SplitPane.Model -> ( SplitPane.Model, Cmd a )
update msg model =
    ( SplitPane.update msg model, Cmd.none )


view : SplitPane.Model -> Html SplitPane.Msg
view model =
    let
        ( firstView, secondView ) =
            chooseViewsBasedOnSplitterPosition <| splitterPosition model
    in
        SplitPane.view identity firstView secondView model


chooseViewsBasedOnSplitterPosition : Px -> ( Html a, Html a )
chooseViewsBasedOnSplitterPosition splitterPosition =
    if splitterPosition < 200 then
        ( smallView, largeView )
    else if splitterPosition < 600 then
        ( mediumView, mediumView )
    else
        ( largeView, smallView )


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
