# Split Pane

A split pane for Elm.

Embed two views beside each other with a resizable splitter in between.

## Basic Usage

Use it just like any other TEA components.

**Don't forget to register subscriptions, or dragging won't work.**

```elm
import Html exposing (..)
import Html.App exposing (program)
import SplitPane


main : Program Never
main =
    program
        { update = update
        , init = init
        , subscriptions = subscriptions
        , view = view
        }

subscriptions : SplitPane.Model -> Sub SplitPane.Msg
subscriptions = SplitPane.subscriptions

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
view =
    SplitPane.view identity firstView secondView


firstView : Html a
firstView =
    text "first view"


secondView : Html a
secondView =
    text "second view"
```


## Usage Examples

[See examples for more complicated uses](https://github.com/doodledood/elm-split-pane/tree/master/examples)
