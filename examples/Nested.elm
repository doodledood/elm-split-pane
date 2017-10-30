module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (src, style)
import SplitPane
    exposing
        ( Orientation(..)
        , SizeUnit(..)
        , ViewConfig
        , createViewConfig
        , configureSplitter
        , percentage
        , px
        )


main : Program Never Model Msg
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
    { outer : SplitPane.State
    , inner : SplitPane.State
    }



-- INIT


init : ( Model, Cmd a )
init =
    { outer =
        SplitPane.init Horizontal
            |> configureSplitter (percentage 0.5 <| Just ( 0.2, 0.8 ))
        -- |> configureSplitter (px 300 <| Just ( 200, 700 ))
    , inner =
        SplitPane.init Vertical
            |> configureSplitter (px 450 <| Just ( 0, 800 ))
        -- |> configureSplitter (percentage 0.75 Nothing)
    }
        ! []



-- UPDATE


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        Outer m ->
            { model
                | outer = SplitPane.update m model.outer
            }
                ! []

        Inner m ->
            { model
                | inner = SplitPane.update m model.inner
            }
                ! []



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style
            [ ( "width", "800px" )
            , ( "height", "600px" )
            ]
        ]
        [ SplitPane.view outerViewConfig leftView (rightView model.inner) model.outer ]


leftView : Html a
leftView =
    div [ containerStyle ]
        [ img [ src "http://4.bp.blogspot.com/-s3sIvuCfg4o/VP-82RkCOGI/AAAAAAAALSY/509obByLvNw/s1600/baby-cat-wallpaper.jpg" ] [] ]


rightView : SplitPane.State -> Html Msg
rightView =
    SplitPane.view innerViewConfig
        (div [ containerStyle ] [ img [ src "https://pbs.twimg.com/profile_images/378800000532546226/dbe5f0727b69487016ffd67a6689e75a.jpeg" ] [] ])
        (div [ containerStyle ] [ img [ src "http://2.bp.blogspot.com/-pATX0YgNSFs/VP-82AQKcuI/AAAAAAAALSU/Vet9e7Qsjjw/s1600/Cat-hd-wallpapers.jpg" ] [] ])


outerViewConfig : ViewConfig Msg
outerViewConfig =
    createViewConfig
        { toMsg = Outer
        , customSplitter = Nothing
        }


innerViewConfig : ViewConfig Msg
innerViewConfig =
    createViewConfig
        { toMsg = Inner
        , customSplitter = Nothing
        }


containerStyle : Attribute a
containerStyle =
    style
        [ ( "width", "100%" )
        , ( "height", "100%" )
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Outer <| SplitPane.subscriptions model.outer
        , Sub.map Inner <| SplitPane.subscriptions model.inner
        ]
