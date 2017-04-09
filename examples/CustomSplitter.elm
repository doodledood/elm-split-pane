module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)
import Maybe
import SplitPane exposing (Orientation(..), CustomSplitter, createCustomSplitter, ViewConfig, createViewConfig)


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
    , message : Maybe String
    }


type Msg
    = PaneMsg SplitPane.Msg
    | CustomSplitterButtonClick



-- INIT


init : ( Model, Cmd a )
init =
    ( { pane =
            SplitPane.init Horizontal
      , message = Nothing
      }
    , Cmd.none
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        PaneMsg paneMsg ->
            ( { model | pane = SplitPane.update paneMsg model.pane }, Cmd.none )

        CustomSplitterButtonClick ->
            ( { model | message = Just "clicked a button inside the custom splitter." }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div
            [ style
                [ ( "width", "800px" )
                , ( "height", "600px" )
                ]
            ]
            [ SplitPane.view viewConfig firstView secondView model.pane ]
        , text <| Maybe.withDefault "" model.message
        ]


myCustomSplitter : CustomSplitter Msg
myCustomSplitter =
    createCustomSplitter PaneMsg
        { attributes =
            [ style
                [ ( "width", "40px" )
                , ( "height", "600px" )
                , ( "background", "lightcoral" )
                , ( "cursor", "col-resize" )
                ]
            ]
        , children =
            [ button [ onClick CustomSplitterButtonClick ] [ text "click me" ] ]
        }


firstView : Html a
firstView =
    img [ src "http://4.bp.blogspot.com/-s3sIvuCfg4o/VP-82RkCOGI/AAAAAAAALSY/509obByLvNw/s1600/baby-cat-wallpaper.jpg" ] []


secondView : Html a
secondView =
    img [ src "http://2.bp.blogspot.com/-pATX0YgNSFs/VP-82AQKcuI/AAAAAAAALSU/Vet9e7Qsjjw/s1600/Cat-hd-wallpapers.jpg" ] []


viewConfig : ViewConfig Msg
viewConfig =
    createViewConfig
        { toMsg = PaneMsg
        , customSplitter = Just myCustomSplitter
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PaneMsg <| SplitPane.subscriptions model.pane
