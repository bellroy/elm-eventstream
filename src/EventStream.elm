module EventStream exposing
    ( Error
    , EventStream
    , addEvent
    , errorToString
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
    = EventStream IncomingEventDecoders Triggers ListOfIncomingEvents


type IncomingEventDecoders
    = IncomingEventDecoders (Dict.Dict EventName (Decode.Decoder Bool))


type Triggers
    = Triggers (List ( Matcher, OutgoingEventDecoder ))


type alias OutgoingEventDecoder =
    Matcher -> Decode.Value -> Result Error Encode.Value


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
init : List ( String, Decode.Decoder Bool ) -> List ( Matcher, OutgoingEventDecoder ) -> EventStream
init listOfIncomingEventDecoders listOfTriggers =
    EventStream
        (IncomingEventDecoders <| Dict.fromList listOfIncomingEventDecoders)
        (Triggers listOfTriggers)
        []


{-| Attempt to add a new event to the EventStream

The minimal expected structure of an event needs to look like;

```json
{ "eventName": "YourEventName",
  "eventData": "Data for which you supply a decoder that confirms it's validity"
}
```

-}
addEvent : EventStream -> RawIncomingEvent -> Result Error ( EventStream, List RawOutgoingEvent )
addEvent ((EventStream incomingEventsDecoders outgoingEventsEncoders listOfEvents) as eventStream) rawIncomingEvent =
    case getEventNameAndDecoder eventStream rawIncomingEvent of
        Err error ->
            Err error

        Ok ( eventName, decoder ) ->
            case decodeValue decoder rawIncomingEvent of
                Err decodeError ->
                    Err <| DecodeError decodeError

                Ok decodedEvent ->
                    let
                        event =
                            Event eventName rawIncomingEvent

                        updatedEventStream =
                            EventStream
                                incomingEventsDecoders
                                outgoingEventsEncoders
                                (event :: listOfEvents)
                    in
                    case triggerOutgoingEvents updatedEventStream event of
                        Err error ->
                            Err error

                        Ok maybeListOfOutgoingEvents ->
                            Ok <|
                                ( updatedEventStream
                                , maybeListOfOutgoingEvents
                                )


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
triggerOutgoingEvents : EventStream -> Event -> Result Error (List RawOutgoingEvent)
triggerOutgoingEvents ((EventStream _ (Triggers outgoingEventEncoders) _) as eventStream) ((Event eventName rawIncomingEvent) as event) =
    let
        triggerOutgoingEvent ( matcher, outgoingEventEncoder ) =
            if String.contains eventName matcher then
                Just <| outgoingEventEncoder matcher rawIncomingEvent

            else
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


getEventNameAndDecoder : EventStream -> RawIncomingEvent -> Result Error ( EventName, Decoder Bool )
getEventNameAndDecoder (EventStream (IncomingEventDecoders incomingEventsDecoders) _ _) rawIncomingEvent =
    case Decode.decodeValue (Decode.field fieldNameEventName Decode.string) rawIncomingEvent of
        Err decodeError ->
            Err (DecodeError decodeError)

        Ok eventName ->
            Dict.get eventName incomingEventsDecoders
                |> Maybe.map (\decoder -> Ok <| ( eventName, decoder ))
                |> Maybe.withDefault (Err <| UnknownEvent eventName)



{- Attempt to find the eventName for given RawIncomingEvent -}


getEventName : EventStream -> RawIncomingEvent -> Maybe EventName
getEventName (EventStream (IncomingEventDecoders incomingEventsDecoders) _ _) rawIncomingEvent =
    case Decode.decodeValue (Decode.field fieldNameEventName Decode.string) rawIncomingEvent of
        Err decodeError ->
            Nothing

        Ok eventName ->
            if Dict.member eventName incomingEventsDecoders then
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
