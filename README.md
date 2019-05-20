# Elm EventStream

Allows you to setup a "stream" of Events.
For every event you are required to setup a name and decoder to validate it's contents.
Triggers can be placed to listen to events and consolidate the data in to a new outgoing event.

## Usage

### Creating a new EventStream

```elm
eventMatcher : Matcher -> Decode.Decoder Bool
eventMatcher matcher =
    Decode.field "eventData"
        (Decode.field "someField" Decode.string
            |> Decode.andThen
                (\someFieldValue ->
                    if matcher == ("TestEvent." ++ someFieldValue) then
                        Decode.succeed True

                    else
                        Decode.succeed False
                )
        )

testEventEncoder : Matcher -> Decode.Value -> Result EventStream.Error Encode.Value
testEventEncoder matcher rawEvent =
    Ok <| Encode.string "outgoing"

eventStream =
    EventStream.init
        [ ( "TestEvent", eventMatcher )
        ]
        [ ( "TestEvent", testEventEncoder ) ]

```

### Adding a new Event to the EventStream

```elm
(updatedEventStream, listOfOutgoingEvents) =
    EventStream.addEvent anEncodedValue eventStream

```
