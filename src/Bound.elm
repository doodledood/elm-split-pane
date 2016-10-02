module Bound exposing (Bound, Bounded, putValue, putBound, createBound, createBounded)


type alias Bound a =
    ( a, a )


type alias Bounded a =
    ( a, Bound a )


createBounded : comparable -> Bound comparable -> Bounded comparable
createBounded value bound =
    putValue ( value, bound ) value


putValue : Bounded comparable -> comparable -> Bounded comparable
putValue ( _, bound ) value =
    ( boundTo bound value, bound )


putBound : Bounded comparable -> Bound comparable -> Bounded comparable
putBound ( value, _ ) bound =
    ( value, bound )


createBound : comparable -> comparable -> Bound comparable
createBound a b =
    ( min a b, max a b )


boundTo : Bound comparable -> comparable -> comparable
boundTo ( a, b ) =
    min b << max a
