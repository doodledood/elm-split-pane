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
    img [ src "http://4.bp.blogspot.com/-s3sIvuCfg4o/VP-82RkCOGI/AAAAAAAALSY/509obByLvNw/s1600/baby-cat-wallpaper.jpg" ] []


secondView : Html a
secondView =
    img [ src "http://2.bp.blogspot.com/-pATX0YgNSFs/VP-82AQKcuI/AAAAAAAALSU/Vet9e7Qsjjw/s1600/Cat-hd-wallpapers.jpg" ] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map PaneMsg <| SplitPane.subscriptions model.pane
