effect module Touch where { subscription = MySub } exposing
  ( Position, position
  , starts
  , moves
  , ends
  , cancels
  )

{-| This library lets you listen to global touch events. This is useful
for a couple tricky scenarios including:

  - Detecting a "touch" outside the current component.
  - Supporting drag-and-drop interactions.

# Touch Position
@docs Position, position

# Subscriptions
@docs starts, moves, ends, cancels

-}

import Dict
import Dom.LowLevel as Dom
import Json.Decode as Json exposing ((:=), at)
import Process
import Task exposing (Task)
import Maybe


-- POSITIONS


{-| The position of the touch relative to the whole document. So if you are
scrolled down a bunch, you are still getting a coordinate relative to the
very top left corner of the *whole* document.
-}
type alias Position =
  { x : Maybe Int
  , y : Maybe Int
  }


{-| The decoder used to extract a `Position` from a JavaScript touch event.
-}
position : Json.Decoder Position
position =
  Json.object2 Position 
    (Json.maybe (at ["touches", "0", "clientX"] Json.int)) 
    (Json.maybe (at ["touches", "0", "clientY"] Json.int))


-- Touch EVENTS


{-| Subscribe to touch starts anywhere on screen.
-}
starts : (Position -> msg) -> Sub msg
starts tagger =
  subscription (MySub "touchstart" tagger)


{-| Subscribe to touche moves anywhere on screen. It is best to unsubscribe if
you do not need these events. Otherwise you will handle a bunch of events for
no benefit.
-}
moves : (Position -> msg) -> Sub msg
moves tagger =
  subscription (MySub "touchmove" tagger)


{-| Get a position whenever the user *lifts* his finger.
-}
ends : (Position -> msg) -> Sub msg
ends tagger =
  subscription (MySub "touchend" tagger)

{-| Get a position whenever the touch ended for some other reason.
-}
cancels : (Position -> msg) -> Sub msg
cancels tagger =
  subscription (MySub "touchcancel" tagger)



-- SUBSCRIPTIONS


type MySub msg
  = MySub String (Position -> msg)


subMap : (a -> b) -> MySub a -> MySub b
subMap func (MySub category tagger) =
  MySub category (tagger >> func)



-- EFFECT MANAGER STATE


type alias State msg =
  Dict.Dict String (Watcher msg)


type alias Watcher msg =
  { taggers : List (Position -> msg)
  , pid : Process.Id
  }



-- CATEGORIZE SUBSCRIPTIONS


type alias SubDict msg =
  Dict.Dict String (List (Position -> msg))


categorize : List (MySub msg) -> SubDict msg
categorize subs =
  categorizeHelp subs Dict.empty


categorizeHelp : List (MySub msg) -> SubDict msg -> SubDict msg
categorizeHelp subs subDict =
  case subs of
    [] ->
      subDict

    MySub category tagger :: rest ->
      categorizeHelp rest <|
        Dict.update category (categorizeHelpHelp tagger) subDict


categorizeHelpHelp : a -> Maybe (List a) -> Maybe (List a)
categorizeHelpHelp value maybeValues =
  case maybeValues of
    Nothing ->
      Just [value]

    Just values ->
      Just (value :: values)



-- EFFECT MANAGER


init : Task Never (State msg)
init =
  Task.succeed Dict.empty


type alias Msg =
  { category : String
  , position : Position
  }


(&>) t1 t2 = t1 `Task.andThen` \_ -> t2


onEffects : Platform.Router msg Msg -> List (MySub msg) -> State msg -> Task Never (State msg)
onEffects router newSubs oldState =
  let
    leftStep category {pid} task =
      Process.kill pid &> task

    bothStep category {pid} taggers task =
      task
        `Task.andThen` \state ->

      Task.succeed
        (Dict.insert category (Watcher taggers pid) state)

    rightStep category taggers task =
      task
        `Task.andThen` \state ->

      Process.spawn (Dom.onDocument category position (Platform.sendToSelf router << Msg category))
        `Task.andThen` \pid ->

      Task.succeed
        (Dict.insert category (Watcher taggers pid) state)
  in
    Dict.merge
      leftStep
      bothStep
      rightStep
      oldState
      (categorize newSubs)
      (Task.succeed Dict.empty)


onSelfMsg : Platform.Router msg Msg -> Msg -> State msg -> Task Never (State msg)
onSelfMsg router {category,position} state =
  case Dict.get category state of
    Nothing ->
      Task.succeed state

    Just {taggers} ->
      let
        send tagger =
          Platform.sendToApp router (tagger position)
      in
        Task.sequence (List.map send taggers)
          `Task.andThen` \_ ->

        Task.succeed state