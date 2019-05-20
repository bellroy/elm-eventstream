module EventStream exposing
    ( Error
    , EventStream
    , Matcher
    , addEvent
    , errorToString
    , getEvents
    , init
    )

import Dict as Dict exposing (Dict, fromList, get, member)
import Json.Decode as Decode exposing (Decoder, Error, Value, andThen, decodeValue, errorToString, fail, field, string, succeed)
import Json.Encode as Encode exposing (Value)
import List as List exposing (filterMap)


type Error
    = UnknownEvent String
    | DecodeError Decode.Error


type EventStream
    = EventStream IncomingEventMatchers Triggers ListOfIncomingEvents


type IncomingEventMatchers
    = IncomingEventMatchers (Dict.Dict EventName IncomingEventMatcher)


type alias IncomingEventMatcher =
    Matcher -> Decode.Decoder Bool


type Triggers
    = Triggers (List ( Matcher, OutgoingEventDecoder ))


type alias OutgoingEventDecoder =
    Matcher -> Decode.Value -> EventStream -> Result Error Encode.Value


type alias ListOfIncomingEvents =
    List Event


type alias EventName =
    String


type alias Matcher =
    String


type Event
    = Event EventName RawIncomingEvent


type alias RawIncomingEvent =
    Decode.Value


type alias RawOutgoingEvent =
    Encode.Value


{-| Create an EventStream
-}
init : List ( String, IncomingEventMatcher ) -> List ( Matcher, OutgoingEventDecoder ) -> EventStream
init listOfIncomingEventMatchers listOfTriggers =
    EventStream
        (IncomingEventMatchers <| Dict.fromList listOfIncomingEventMatchers)
        (Triggers listOfTriggers)
        []


{-| Attempt to add a new event to the EventStream

The minimal expected structure of an event needs to look like;

```json
{ "eventName": "YourEventName",
  "eventData": "Data for which you supply a decoder that confirms it's validity and match"
}
```

-}
addEvent : RawIncomingEvent -> EventStream -> Result Error ( EventStream, List RawOutgoingEvent )
addEvent rawIncomingEvent ((EventStream incomingEventsMatcher outgoingEventsEncoders listOfEvents) as eventStream) =
    case getEventNameAndMatcher rawIncomingEvent eventStream of
        Err error ->
            Err error

        Ok ( eventName, decoder ) ->
            case decodeValue (decoder eventName) rawIncomingEvent of
                Err decodeError ->
                    Err <| DecodeError decodeError

                Ok _ ->
                    let
                        event =
                            Event eventName rawIncomingEvent

                        updatedEventStream =
                            EventStream
                                incomingEventsMatcher
                                outgoingEventsEncoders
                                (event :: listOfEvents)
                    in
                    case triggerOutgoingEvents event updatedEventStream of
                        Err error ->
                            Err error

                        Ok maybeListOfOutgoingEvents ->
                            Ok <|
                                ( updatedEventStream
                                , maybeListOfOutgoingEvents
                                )


{-| Get RawIncomingEvents that match query from the eventStream
-}
getEvents : Matcher -> EventStream -> List RawIncomingEvent
getEvents matcher ((EventStream _ _ listOfEvents) as eventStream) =
    List.filterMap
        (\(Event eventName rawIncomingEvent) ->
            if eventName == matcher then
                Just rawIncomingEvent

            else
                case getEventNameAndMatcher rawIncomingEvent eventStream of
                    Err _ ->
                        Nothing

                    Ok ( _, incomingEventMatcher ) ->
                        case Decode.decodeValue (incomingEventMatcher matcher) rawIncomingEvent of
                            Ok True ->
                                Just rawIncomingEvent

                            Ok False ->
                                Nothing

                            Err _ ->
                                Nothing
        )
        listOfEvents


{-| Convert an EventStream error into a String that is nice for debugging.
-}
errorToString : Error -> String
errorToString error =
    case error of
        UnknownEvent string ->
            "UnknownEvent: " ++ string

        DecodeError decodeError ->
            Decode.errorToString decodeError



{- INTERNALS -}


{-| Takes the eventStream and for given event returns a list of triggered outgoing events
-}
triggerOutgoingEvents : Event -> EventStream -> Result Error (List RawOutgoingEvent)
triggerOutgoingEvents ((Event eventName rawIncomingEvent) as event) ((EventStream _ (Triggers outgoingEventEncoders) _) as eventStream) =
    let
        triggerOutgoingEvent ( matcher, outgoingEventEncoder ) =
            if eventName == matcher then
                Just <| outgoingEventEncoder matcher rawIncomingEvent eventStream

            else
                case getEventNameAndMatcher rawIncomingEvent eventStream of
                    Err _ ->
                        Nothing

                    Ok ( _, incomingEventMatcher ) ->
                        case Decode.decodeValue (incomingEventMatcher matcher) rawIncomingEvent of
                            Ok True ->
                                Just <| outgoingEventEncoder matcher rawIncomingEvent eventStream

                            Ok False ->
                                Nothing

                            Err _ ->
                                Nothing

        triggeredOutgoingEvents =
            List.filterMap triggerOutgoingEvent outgoingEventEncoders
    in
    case firstErrorInList triggeredOutgoingEvents of
        Just error ->
            Err error

        Nothing ->
            Ok <| List.filterMap Result.toMaybe triggeredOutgoingEvents



{- A convenience constant that represents the eventName field name on a JSON event object -}


fieldNameEventName : String
fieldNameEventName =
    "eventName"



{- A convenience constant that represents the eventData field name on a JSON event object -}


fieldNameEventData : String
fieldNameEventData =
    "eventData"



{- Attempt to find the eventName and validity decoder for given RawIncomingEvent -}


getEventNameAndMatcher : RawIncomingEvent -> EventStream -> Result Error ( EventName, IncomingEventMatcher )
getEventNameAndMatcher rawIncomingEvent (EventStream (IncomingEventMatchers incomingEventsMatcher) _ _) =
    case Decode.decodeValue (Decode.field fieldNameEventName Decode.string) rawIncomingEvent of
        Err decodeError ->
            Err (DecodeError decodeError)

        Ok eventName ->
            Dict.get eventName incomingEventsMatcher
                |> Maybe.map (\decoder -> Ok <| ( eventName, decoder ))
                |> Maybe.withDefault (Err <| UnknownEvent eventName)



{- Attempt to find the eventName for given RawIncomingEvent -}


getEventName : RawIncomingEvent -> EventStream -> Maybe EventName
getEventName rawIncomingEvent (EventStream (IncomingEventMatchers incomingEventsMatcher) _ _) =
    case Decode.decodeValue (Decode.field fieldNameEventName Decode.string) rawIncomingEvent of
        Err decodeError ->
            Nothing

        Ok eventName ->
            if Dict.member eventName incomingEventsMatcher then
                Just eventName

            else
                Nothing



{- Return the first error in given list -}


firstErrorInList : List (Result x a) -> Maybe x
firstErrorInList =
    List.head
        << List.filterMap
            (\a ->
                case a of
                    Err x ->
                        Just x

                    Ok _ ->
                        Nothing
            )
