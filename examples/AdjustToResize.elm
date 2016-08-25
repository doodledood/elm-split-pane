module Main exposing (..)

import Html exposing (..)
import Html.App exposing (program)
import SplitPane exposing (Px, splitterPosition)


main : Program Never
main =
    program
        { update = update
        , init = init
        , subscriptions = SplitPane.subscriptions
        , view = view
        }

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
    let (firstView, secondView) = chooseViewsBasedOnSplitterPosition <| splitterPosition model
    in SplitPane.view identity model firstView secondView

chooseViewsBasedOnSplitterPosition : Px -> ( Html a, Html b )
chooseViewsBasedOnSplitterPosition splitterPosition =
    if splitterPosition < 200 then (smallView, largeView)
    else if splitterPosition < 600 then (mediumView, mediumView)
    else (largeView, smallView)

smallView : Html a
smallView =
    text "small"

mediumView : Html a
mediumView =
    text "medium"

largeView : Html a
largeView =
    text "large"
