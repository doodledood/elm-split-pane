module Main exposing (..)

import Html exposing (..)
import Html.App exposing (program)
import Html.Attributes exposing (src, style)
import SplitPane
    exposing
        ( Orientation(..)
        , draggable
        , startAt
        , withFirstViewMinSize
        , withSecondViewMinSize
        , Size(..)
        )


main : Program Never
main =
    program
        { update = update
        , init = init
        , subscriptions = subscriptions
        , view = view
        }


type Msg
    = Outer SplitPane.Msg
    | Inner SplitPane.Msg


type alias Model =
    { outer : SplitPane.Model
    , inner : SplitPane.Model
    }


init : ( Model, Cmd a )
init =
    { outer =
        SplitPane.init
            { paneWidth = 800
            , paneHeight = 600
            , orientation = Horizontal
            }
            |> draggable False
            |> startAt 200
    , inner =
        SplitPane.init
            { paneWidth = 600
            , paneHeight = 600
            , orientation = Vertical
            }
            |> withFirstViewMinSize (Percentage 0.33)
            |> withSecondViewMinSize (Px 200)
    }
        ! []


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        Outer m ->
            ( { model
                | outer = SplitPane.update m model.outer
              }
            , Cmd.none
            )

        Inner m ->
            ( { model
                | inner = SplitPane.update m model.inner
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    SplitPane.view Outer model.outer thirdView <| secondView model.inner


firstView : Html a
firstView =
    unselectableImg "http://4.bp.blogspot.com/-s3sIvuCfg4o/VP-82RkCOGI/AAAAAAAALSY/509obByLvNw/s1600/baby-cat-wallpaper.jpg"


secondView : SplitPane.Model -> Html Msg
secondView model =
    SplitPane.view Inner model firstView <|
        unselectableImg "http://2.bp.blogspot.com/-pATX0YgNSFs/VP-82AQKcuI/AAAAAAAALSU/Vet9e7Qsjjw/s1600/Cat-hd-wallpapers.jpg"


thirdView : Html a
thirdView =
    unselectableImg "https://pbs.twimg.com/profile_images/378800000532546226/dbe5f0727b69487016ffd67a6689e75a.jpeg"


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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Outer <| SplitPane.subscriptions model.outer
        , Sub.map Inner <| SplitPane.subscriptions model.inner
        ]
