module Styles exposing (paneContainerStyle, childViewStyle, defaultVerticalSplitterStyle, defaultHorizontalSplitterStyle)

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


defaultVerticalSplitterStyle : Bool -> Attribute a
defaultVerticalSplitterStyle isDraggable =
    style
        (baseDefaultSplitterStyles
            ++ [ ( "height", "11px" )
               , ( "width", "100%" )
               , ( "margin", "-5px 0" )
               , ( "borderTop", "5px solid rgba(255, 255, 255, 0)" )
               , ( "borderBottom", "5px solid rgba(255, 255, 255, 0)" )
               ]
            ++ if isDraggable then
                [ ( "cursor", "row-resize" ) ]
               else
                []
        )


defaultHorizontalSplitterStyle : Bool -> Attribute a
defaultHorizontalSplitterStyle isDraggable =
    style
        (baseDefaultSplitterStyles
            ++ [ ( "width", "11px" )
               , ( "height", "100%" )
               , ( "margin", "0 -5px" )
               , ( "borderLeft", "5px solid rgba(255, 255, 255, 0)" )
               , ( "borderRight", "5px solid rgba(255, 255, 255, 0)" )
               ]
            ++ if isDraggable then
                [ ( "cursor", "col-resize" ) ]
               else
                []
        )


baseDefaultSplitterStyles : List ( String, String )
baseDefaultSplitterStyles =
    [ ( "width", "100%" )
    , ( "background", "#000" )
    , ( "boxSizing", "border-box" )
    , ( "opacity", ".2" )
    , ( "zIndex", "1" )
    , ( "webkitUserSelect", "none" )
    , ( "mozUserSelect", "none" )
    , ( "userSelect", "none" )
    , ( "backgroundClip", "padding-box" )
    ]
