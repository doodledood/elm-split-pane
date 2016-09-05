module Styles exposing (paneContainerStyle, childViewStyle)

import Html exposing (Attribute)
import Html.Attributes exposing (style)


paneContainerStyle : Bool -> Attribute a
paneContainerStyle isHorizontal =
    style
        [ ( "overflow", "hidden" )
        , ( "display", "flex" )
        , ( "flexDirection"
          , if isHorizontal then
                "row"
            else
                "column"
          )
        , ( "justifyContent", "center" )
        , ( "alignItems", "center" )
        , ( "width", "100%" )
        , ( "height", "100%" )
        , ( "boxSizing", "border-box" )
        ]


childViewStyle : Float -> Attribute a
childViewStyle fraction =
    style
        [ ( "flex", toString fraction )
        , ( "width", "100%" )
        , ( "height", "100%" )
        , ( "overflow", "hidden" )
        , ( "boxSizing", "border-box" )
        , ( "position", "relative" )
        ]
