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
        , changeOrientationTo
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



-- MODEL


type Msg
    = Outer SplitPane.Msg
    | Inner SplitPane.Msg


type alias Model =
    { outer : SplitPane.Model
    , inner : SplitPane.Model
    }



-- INIT


init : ( Model, Cmd a )
init =
    { outer =
        SplitPane.init
            { paneWidth = 800
            , paneHeight = 600
            }
            |> startAt (Percentage 0.2)
    , inner =
        SplitPane.init
            { paneWidth = 600
            , paneHeight = 600
            }
            |> changeOrientationTo Vertical
            |> startAt (Px 400)
            |> withFirstViewMinSize (Percentage 0.2)
            |> withSecondViewMinSize (Px 200)
    }
        ! []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        Outer m ->
            let
                ( newOuterModel, _ ) =
                    SplitPane.update m model.outer
            in
                ( { model
                    | outer = newOuterModel
                  }
                , Cmd.none
                )

        Inner m ->
            let
                ( newInnerModel, _ ) =
                    SplitPane.update m model.inner
            in
                ( { model
                    | inner = newInnerModel
                  }
                , Cmd.none
                )



-- VIEW


view : Model -> Html Msg
view model =
    SplitPane.view Outer leftView (rightView model.inner) model.outer


leftView : Html a
leftView =
    img [ src "http://4.bp.blogspot.com/-s3sIvuCfg4o/VP-82RkCOGI/AAAAAAAALSY/509obByLvNw/s1600/baby-cat-wallpaper.jpg" ] []


rightView : SplitPane.Model -> Html Msg
rightView =
    SplitPane.view Inner
        (img [ src "https://pbs.twimg.com/profile_images/378800000532546226/dbe5f0727b69487016ffd67a6689e75a.jpeg" ] [])
        (img [ src "http://2.bp.blogspot.com/-pATX0YgNSFs/VP-82AQKcuI/AAAAAAAALSU/Vet9e7Qsjjw/s1600/Cat-hd-wallpapers.jpg" ] [])



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Outer <| SplitPane.subscriptions model.outer
        , Sub.map Inner <| SplitPane.subscriptions model.inner
        ]
