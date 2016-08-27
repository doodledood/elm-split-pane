# Split Pane

A split pane for Elm.

Embed two views beside each other with a resizable splitter in between.

## Basic Usage

Use it just like any other TEA component.

**Don't forget to register subscriptions, or dragging on desktop won't work.**

```elm
module Main exposing (..)

import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (src, style)
import SplitPane


main : Program Never
main =
    program
        { update = update
        , init = init
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { pane : SplitPane.Model
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
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        PaneMsg paneMsg ->
            let
                ( updatedPane, _ ) =
                    SplitPane.update paneMsg model.pane
            in
                ( { model | pane = updatedPane }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    SplitPane.view PaneMsg firstView secondView model.pane


firstView : Html a
firstView =
    text "first view"


secondView : Html a
secondView =
    text "second view"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PaneMsg <| SplitPane.subscriptions model.pane

```


## Examples

1. [Simple](https://doodledood.github.io/elm-split-pane/simple.html)
2. [Nested](https://doodledood.github.io/elm-split-pane/nested.html)
3. [Custom splitter](https://doodledood.github.io/elm-split-pane/customSplitter.html)
4. [Adjusting to resizes](https://doodledood.github.io/elm-split-pane/adjustToResize.html)

[Examples code](https://github.com/doodledood/elm-split-pane/tree/master/examples)
