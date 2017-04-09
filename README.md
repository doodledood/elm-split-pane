# Split Pane

A split pane for Elm.

Embed two views beside each other with a resizable splitter in between.

## Usage Rules

  - Always put `SplitPane.State` in your model.
  - Never put _any_ `Config` in your model.
  - Don't forget to register subscriptions for dragging to work.
  - To control the pane's size place the pane inside a container and give the container a size

Design inspired by [elm-sortable-table](https://github.com/evancz/elm-sortable-table/).

Read about why these usage rules are good rules [here](https://github.com/evancz/elm-sortable-table/tree/1.0.0#usage-rules).

## Installation

```
elm package install doodledood/elm-split-pane
```

## Examples

1. [Simple](https://doodledood.github.io/elm-split-pane/simple.html)
2. [Nested](https://doodledood.github.io/elm-split-pane/nested.html)
3. [Custom splitter](https://doodledood.github.io/elm-split-pane/customSplitter.html)
4. [Adjusting to resizes](https://doodledood.github.io/elm-split-pane/adjustToResize.html)

[Examples code](https://github.com/doodledood/elm-split-pane/tree/master/examples)

## Basic Usage

Use it just like any other TEA component.

```elm

module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src, style)
import SplitPane exposing (Orientation(..), ViewConfig, createViewConfig)


main : Program Never Model Msg
main =
    program
        { update = update
        , init = init
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { pane : SplitPane.State
    }


type Msg
    = PaneMsg SplitPane.Msg



-- INIT


init : ( Model, Cmd a )
init =
    ( { pane = SplitPane.init Horizontal
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        PaneMsg paneMsg ->
            ( { model | pane = SplitPane.update paneMsg model.pane }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "width", "800px" )
            , ( "height", "600px" )
            ]
        ]
        [ SplitPane.view viewConfig firstView secondView model.pane ]


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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PaneMsg <| SplitPane.subscriptions model.pane

```