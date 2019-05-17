module EventStreamTest exposing (suite)

import EventStream exposing (Error, EventStream, addEvent, errorToString, init)
import Expect as Expect exposing (equal, fail, pass)
import Json.Decode as Decode exposing (Decoder, decodeValue, errorToString, string, succeed)
import Json.Encode as Encode exposing (Value, null, object, string)
import Test as Test exposing (Test, describe, test)


mockTestEventDecoder : Decode.Decoder Bool
mockTestEventDecoder =
    Decode.succeed True


mockTestEventEncoder : String -> Decode.Value -> Result EventStream.Error Encode.Value
mockTestEventEncoder matcher rawEvent =
    Ok <| Encode.string "outgoing"


mockRawTestEvent : Encode.Value
mockRawTestEvent =
    Encode.object
        [ ( "eventName", Encode.string "TestEvent" )
        , ( "eventData", Encode.null )
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
        [ ( "TestEvent", mockTestEventDecoder )
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
        , Test.test "An eventStream can have multiple triggers set with the same Matcher"
            (\() ->
                let
                    useEventStream =
                        EventStream.init
                            [ ( "TestEvent", mockTestEventDecoder )
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

                    Ok ( updatedEventStream, _ ) ->
                        Expect.fail "Expected to receive two outgoingEvents"

                    Err error ->
                        Expect.fail <| EventStream.errorToString error
            )
        ]
