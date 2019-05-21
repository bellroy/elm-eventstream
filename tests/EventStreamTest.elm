module EventStreamTest exposing (suite)

import EventStream exposing (Error, EventStream, Matcher, addEvent, addTrigger, errorToString, getEvents, init)
import Expect as Expect exposing (equal, fail, pass)
import Json.Decode as Decode exposing (Decoder, Error, andThen, decodeValue, errorToString, field, string, succeed)
import Json.Encode as Encode exposing (Value, null, object, string)
import Test as Test exposing (Test, describe, test)


mockTestEventMatcher : Matcher -> Decode.Decoder Bool
mockTestEventMatcher _ =
    Decode.succeed False


mockAnotherTestEventMatcher : Matcher -> Decode.Decoder Bool
mockAnotherTestEventMatcher matcher =
    Decode.field "eventData"
        (Decode.field "someField" Decode.string
            |> Decode.andThen
                (\someFieldValue ->
                    if matcher == ("AnotherTestEvent." ++ someFieldValue) then
                        Decode.succeed True

                    else
                        Decode.succeed False
                )
        )


mockTestEventEncoder : Matcher -> Decode.Value -> EventStream -> Result Decode.Error Encode.Value
mockTestEventEncoder matcher rawEvent eventStream =
    Ok <| Encode.string "outgoing"


mockRawTestEvent : Encode.Value
mockRawTestEvent =
    Encode.object
        [ ( "eventName", Encode.string "TestEvent" )
        , ( "eventData", Encode.null )
        ]


mockRawAnotherTestEvent : String -> Encode.Value
mockRawAnotherTestEvent fieldValue =
    Encode.object
        [ ( "eventName", Encode.string "AnotherTestEvent" )
        , ( "eventData"
          , Encode.object
                [ ( "someField", Encode.string fieldValue )
                ]
          )
        ]


mockRawNonExistingEvent : Encode.Value
mockRawNonExistingEvent =
    Encode.object
        [ ( "eventName", Encode.string "NonExistingEvent" )
        , ( "eventData", Encode.null )
        ]


mockEventStream : EventStream
mockEventStream =
    EventStream.init
        [ ( "TestEvent", mockTestEventMatcher )
        , ( "AnotherTestEvent", mockAnotherTestEventMatcher )
        ]
        [ ( "TestEvent", mockTestEventEncoder ) ]


suite : Test
suite =
    Test.describe "EventStream"
        [ Test.test "Add a valid TestEvent"
            (\() ->
                case EventStream.addEvent mockRawTestEvent mockEventStream of
                    Ok ( updatedEventStream, _ ) ->
                        Expect.pass

                    Err error ->
                        Expect.fail <| EventStream.errorToString error
            )
        , Test.test "Add an non existing event"
            (\() ->
                case EventStream.addEvent mockRawNonExistingEvent mockEventStream of
                    Ok ( updatedEventStream, _ ) ->
                        Expect.fail "Expected to fail"

                    Err error ->
                        Expect.equal (EventStream.errorToString error) "UnknownEvent: NonExistingEvent"
            )
        , Test.test "Receive an outgoingEvent"
            (\() ->
                case EventStream.addEvent mockRawTestEvent mockEventStream of
                    Ok ( updatedEventStream, [ outgoingEvent ] ) ->
                        case Decode.decodeValue Decode.string outgoingEvent of
                            Ok string ->
                                Expect.equal string "outgoing"

                            Err error ->
                                Expect.fail <| Decode.errorToString error

                    Ok ( updatedEventStream, _ ) ->
                        Expect.fail "Expected to receive one outgoingEvent"

                    Err error ->
                        Expect.fail <| EventStream.errorToString error
            )
        , Test.test "Set a trigger after initializing the eventStream"
            (\() ->
                let
                    result =
                        EventStream.addTrigger "AnotherTestEvent" mockTestEventEncoder mockEventStream
                            |> EventStream.addEvent (mockRawAnotherTestEvent "someValue")
                in
                case result of
                    Ok ( updatedEventStream, [ outgoingEvent ] ) ->
                        case Decode.decodeValue Decode.string outgoingEvent of
                            Ok string ->
                                Expect.equal string "outgoing"

                            Err error ->
                                Expect.fail <| Decode.errorToString error

                    Ok ( updatedEventStream, _ ) ->
                        Expect.fail "Expected to receive one outgoingEvent"

                    Err error ->
                        Expect.fail <| EventStream.errorToString error
            )
        , Test.test "An eventStream can have multiple triggers set with the same Matcher"
            (\() ->
                let
                    useEventStream =
                        EventStream.init
                            [ ( "TestEvent", mockTestEventMatcher )
                            ]
                            [ ( "TestEvent", mockTestEventEncoder )
                            , ( "TestEvent", mockTestEventEncoder )
                            ]
                in
                case EventStream.addEvent mockRawTestEvent useEventStream of
                    Ok ( updatedEventStream, [ outgoingEvent, _ ] ) ->
                        case Decode.decodeValue Decode.string outgoingEvent of
                            Ok string ->
                                Expect.equal string "outgoing"

                            Err error ->
                                Expect.fail <| Decode.errorToString error

                    Ok ( updatedEventStream, listOutgoingEvent ) ->
                        Expect.fail ("Expected to receive two outgoingEvents, got " ++ (String.fromInt <| List.length listOutgoingEvent))

                    Err error ->
                        Expect.fail <| EventStream.errorToString error
            )
        , Test.test "You can set a trigger for a specific event using matchers"
            (\() ->
                let
                    useEventStream =
                        EventStream.init
                            [ ( "AnotherTestEvent", mockAnotherTestEventMatcher )
                            ]
                            [ ( "AnotherTestEvent.triggerMe", mockTestEventEncoder )
                            ]
                in
                case EventStream.addEvent (mockRawAnotherTestEvent "triggerMe") useEventStream of
                    Ok ( updatedEventStream, [ outgoingEvent ] ) ->
                        case Decode.decodeValue Decode.string outgoingEvent of
                            Ok string ->
                                Expect.equal string "outgoing"

                            Err error ->
                                Expect.fail <| Decode.errorToString error

                    Ok ( updatedEventStream, listOutgoingEvent ) ->
                        Expect.fail ("Expected to receive a single outgoingEvent, got " ++ (String.fromInt <| List.length listOutgoingEvent))

                    Err error ->
                        Expect.fail <| EventStream.errorToString error
            )
        , Test.test "Get past occurrences of an event"
            (\() ->
                let
                    resultUpdatedEventStream =
                        EventStream.addEvent (mockRawAnotherTestEvent "someValue") mockEventStream
                            |> Result.andThen (EventStream.addEvent mockRawTestEvent << Tuple.first)
                            |> Result.andThen (EventStream.addEvent (mockRawAnotherTestEvent "someOtherValue") << Tuple.first)
                in
                case resultUpdatedEventStream of
                    Err error ->
                        Expect.fail <| EventStream.errorToString error

                    Ok ( updatedEventStream, _ ) ->
                        Expect.equal (List.length <| EventStream.getEvents "AnotherTestEvent" updatedEventStream) 2
            )
        , Test.test "Get past occurrences of a specific event (using matcher)"
            (\() ->
                let
                    resultUpdatedEventStream =
                        EventStream.addEvent (mockRawAnotherTestEvent "someValue") mockEventStream
                            |> Result.andThen (EventStream.addEvent mockRawTestEvent << Tuple.first)
                            |> Result.andThen (EventStream.addEvent (mockRawAnotherTestEvent "someOtherValue") << Tuple.first)
                in
                case resultUpdatedEventStream of
                    Err error ->
                        Expect.fail <| EventStream.errorToString error

                    Ok ( updatedEventStream, _ ) ->
                        Expect.equal (List.length <| EventStream.getEvents "AnotherTestEvent.someOtherValue" updatedEventStream) 1
            )
        ]
