module Main exposing (..)

import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (src, style)
import SplitPane exposing (Orientation(..), Size, startAt)


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
        , orientation = Horizontal
        }
        |> startAt 200
    , Cmd.none
    )


update : SplitPane.Msg -> SplitPane.Model -> ( SplitPane.Model, Cmd a )
update msg model =
    ( SplitPane.update msg model, Cmd.none )


view : SplitPane.Model -> Html SplitPane.Msg
view model =
    SplitPane.view identity model firstView secondView


firstView : Html a
firstView =
    unselectableImg "http://4.bp.blogspot.com/-s3sIvuCfg4o/VP-82RkCOGI/AAAAAAAALSY/509obByLvNw/s1600/baby-cat-wallpaper.jpg"


secondView : Html a
secondView =
    unselectableImg "http://2.bp.blogspot.com/-pATX0YgNSFs/VP-82AQKcuI/AAAAAAAALSU/Vet9e7Qsjjw/s1600/Cat-hd-wallpapers.jpg"


unselectableImg : String -> Html a
unselectableImg url =
    img
        [ src url
        , style
            [ ( "userSelect", "none" )
            , ( "webkitUserSelect", "none" )
            , ( "mozUserSelect", "none" )
            , ( "msUserSelect", "none" )
            , ( "pointerEvents", "none" )
            ]
        ]
        []
