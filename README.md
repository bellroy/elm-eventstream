# Elm EventStream

Allows you to setup a "stream" of Events.
For every event you are required to setup a name and decoder to validate it's contents.
Triggers can be placed to listen to events and consolidate the data in to a new outgoing event.

## Usage

### Creating a new EventStream

```elm
eventStream =
    EventStream.init
        [ ( "TestEvent", aDecoderThatConfirmsTheEventsValidity )
        ]
        [ ( "TestEvent", anEncoderThatResultsInAOutgoingEvent ) ]

```

### Adding a new Event to the EventStream

```elm
(updatedEventStream, listOfOutgoingEvents) =
    EventStream.addEvent eventStream anEncodedValue

```
