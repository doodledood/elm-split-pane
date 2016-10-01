module Bound exposing (Bound, Bounded, putValue, putBound, createBound)


type alias Bound a =
    ( a, a )


type alias Bounded a =
    { value : a
    , bound : Bound a
    }


putValue : Bounded comparable -> comparable -> Bounded comparable
putValue { bound } value =
    Bounded (boundTo bound value) bound


putBound : Bounded comparable -> Bound comparable -> Bounded comparable
putBound { value } bound =
    putValue (Bounded value bound) value


createBound : comparable -> comparable -> Bound comparable
createBound a b =
    ( min a b, max a b )


boundTo : Bound comparable -> comparable -> comparable
boundTo ( a, b ) x =
    min b <| max a x
