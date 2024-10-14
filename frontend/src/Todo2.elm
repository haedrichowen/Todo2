module Todo2 exposing (..)

import Array
import Browser
import Browser.Events
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decoder
import Json.Encode as Encoder
import List
import Platform.Cmd as Cmd
import String
import Task
import Time
import Browser.Dom exposing (setViewportOf)

-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Todo =
    { content : String
    , startTime : Int
    , length : Int
    }


type alias Note =
    { content : String
    , position : Vector2
    , createdTime : Int
    }

type alias FloatingNote =
    { note : Note
    , moving : Bool
    }
type alias Vector2 =
    { x : Int
    , y : Int
    }

vectorAdd : Vector2 -> Vector2 -> Vector2
vectorAdd v1 v2 = { x = v1.x + v2.x, y = v1.y + v2.y}

vectorDiff : Vector2 -> Vector2 -> Vector2
vectorDiff v1 v2 = { x = v1.x - v2.x, y = v1.y - v2.y}

type alias TimeModel =
    { zone : Time.Zone
    , now : Time.Posix
    }


type alias TimeBlock =
    { startTime : Int
    , endTime : Int
    }


type alias Model =
    { newTodo : Todo
    , todoList : List Todo
    , noteArray : Array.Array FloatingNote
    , time : TimeModel
    , timeSlotCount : Int
    , pointer : { position : Vector2, velocity : Vector2 }
    }


defaultTimeSlotLength : Int
defaultTimeSlotLength =
    15 * 60 * 1000


defaultTodo : Todo
defaultTodo =
    { content = "Make a Todo!", startTime = 0, length = defaultTimeSlotLength }


initializeTime : Cmd Msg
initializeTime =
    Cmd.batch [ Task.perform GetNewTime Time.now, Task.perform GetTimeZone Time.here ]


init : () -> ( Model, Cmd Msg )
init _ =
    let
        blankModel =
            { newTodo = { defaultTodo | content = "" }
            , todoList = []
            , noteArray = Array.fromList []
            , time = { now = Time.millisToPosix 0, zone = Time.utc }
            , timeSlotCount = 9
            , pointer = { velocity = { x = 0, y = 0 }, position = { x = 0, y = 0 } }
            }
    in
    ( blankModel, Cmd.batch [ initializeTime, getTodoList, getNoteArray ] )



-- UPDATE


type Msg
    = NewTodoContent String
    | NewTodoStartTime Int
    | NewTodoLength Int
    | SubmitNewTodo Todo
    | RemoveTodo Todo
    | UpdateTodoList (List Todo)
    | SpawnNote Vector2
    | MoveNote Vector2
    | UpdateNoteArray (Array.Array {note : Note, moving : Bool})
    | SyncTodos (Result Http.Error Decoder.Value)
    | SyncNotes (Result Http.Error Decoder.Value)
    | GetNewTime Time.Posix
    | GetTimeZone Time.Zone
    | ModifyDOM (Result Browser.Dom.Error ())

encodeTodoList : List Todo -> Encoder.Value
encodeTodoList todoList =
    Encoder.list todoEncoder todoList


encodeNoteArray : Array.Array Note -> Encoder.Value
encodeNoteArray noteArray =
    Encoder.array noteEncoder noteArray


todoEncoder : Todo -> Encoder.Value
todoEncoder todo =
    Encoder.object
        [ ( "content", Encoder.string todo.content )
        , ( "startTime", Encoder.int todo.startTime )
        , ( "length", Encoder.int todo.length )
        ]


noteEncoder : Note -> Encoder.Value
noteEncoder note =
    let
        encodedVector2 =
            Encoder.object
                [ ( "x", Encoder.int note.position.x )
                , ( "y", Encoder.int note.position.y )
                ]
    in
    Encoder.object
        [ ( "content", Encoder.string note.content )
        , ( "position", encodedVector2 )
        , ( "createdTime", Encoder.int note.createdTime )
        ]


todoDecoder : Decoder.Decoder Todo
todoDecoder =
    Decoder.map3 Todo
        (Decoder.field "content" Decoder.string)
        (Decoder.field "startTime" Decoder.int)
        (Decoder.field "length" Decoder.int)


noteDecoder : Decoder.Decoder Note
noteDecoder =
    Decoder.map3 Note
        (Decoder.field "content" Decoder.string)
        (Decoder.field "position"
            (Decoder.map2 Vector2
                (Decoder.field "x" Decoder.int)
                (Decoder.field "y" Decoder.int)
            )
        )
        (Decoder.field "createdTime" Decoder.int)


positionDecoder : Decoder.Decoder Vector2
positionDecoder =
    Decoder.map2 Vector2
        (Decoder.field "clientX" Decoder.int)
        (Decoder.field "clientY" Decoder.int)

resetScrollPosition : String -> Cmd Msg
resetScrollPosition id = 
    Task.attempt ModifyDOM (setViewportOf id 0 0)

postTodoList : List Todo -> Cmd Msg
postTodoList todoList =
    Http.post
        { url = "http://localhost:7999/sync/todo"
        , body = Http.jsonBody (encodeTodoList todoList)
        , expect = Http.expectJson SyncTodos Decoder.value
        }


getTodoList : Cmd Msg
getTodoList =
    Http.get
        { url = "http://localhost:7999/sync/todo"
        , expect = Http.expectJson SyncTodos Decoder.value
        }


postNoteArray : Array.Array Note -> Cmd Msg
postNoteArray noteArray =
    Http.post
        { url = "http://localhost:7999/sync/notes"
        , body = Http.jsonBody (encodeNoteArray noteArray)
        , expect = Http.expectJson SyncNotes Decoder.value
        }


getNoteArray : Cmd Msg
getNoteArray =
    Http.get
        { url = "http://localhost:7999/sync/notes"
        , expect = Http.expectJson SyncNotes Decoder.value
        }


todoDecoderResultsHandler : Result Decoder.Error (List Todo) -> List Todo
todoDecoderResultsHandler result =
    case result of
        Err error ->
            let
                _ =
                    Debug.log "Todo error is " error
            in
            []

        Ok todoList ->
            todoList


noteDecoderResultsHandler : Result Decoder.Error (Array.Array Note) -> Array.Array Note
noteDecoderResultsHandler result =
    case result of
        Err error ->
            let
                _ =
                    Debug.log "Notes error is " error
            in
            Array.fromList []

        Ok noteArray ->
            noteArray


cleanNewTodo : Todo -> Model -> Todo
cleanNewTodo todo model =
    let
        roundedTimeMillis =
            (Time.posixToMillis model.time.now // model.newTodo.length) * model.newTodo.length
    in
    if todo.startTime == 0 then
        { todo | startTime = roundedTimeMillis }

    else
        todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTodoContent newContent ->
            ( { model | newTodo = { content = newContent, startTime = model.newTodo.startTime, length = model.newTodo.length } }
            , Cmd.none
            )

        NewTodoStartTime selectedTime ->
            ( { model | newTodo = { content = model.newTodo.content, startTime = selectedTime, length = model.newTodo.length } }
            , Cmd.none
            )

        NewTodoLength newLength ->
            ( { model | newTodo = { content = model.newTodo.content, startTime = model.newTodo.startTime, length = newLength } }
            , resetScrollPosition "timeSelection" 
            )

        SubmitNewTodo newTodo ->
            let
                newModel =
                    { model | todoList = List.sortBy .startTime (cleanNewTodo newTodo model :: model.todoList) }
            in
            ( { newModel
                | newTodo = { content = "", startTime = getNextFreeTime newModel, length = model.newTodo.length }
              }
            , postTodoList newModel.todoList
            )

        RemoveTodo todo ->
            let
                newTodoList =
                    List.filter (\x -> x /= todo) model.todoList

                newModel = { model | todoList = newTodoList }
            in
            ( { newModel | newTodo = { content = "", startTime = getNextFreeTime newModel, length = model.newTodo.length } }, postTodoList newModel.todoList )

        UpdateTodoList newTodoList ->
            let
                newModel =
                    { model | todoList = List.sortBy .startTime newTodoList }
            in
            ( newModel
            , postTodoList newTodoList
            )

        SpawnNote position ->
            let
                newNote =
                    { content = "", position = position, createdTime = Time.posixToMillis model.time.now }

                newModel =
                    { model | noteArray = Array.append model.noteArray (Array.fromList [ { note = newNote, moving = False } ]) }
            in
            ( newModel, postNoteArray <| Array.map (\x -> x.note) newModel.noteArray )

        MoveNote newPosition -> 
            let newModel = {model | pointer = { position = newPosition, velocity = vectorDiff newPosition model.pointer.position }}
            in
            ( {newModel | noteArray = Array.map (\x -> if x.moving then {x | note = {content = x.note.content, position = vectorAdd x.note.position newModel.pointer.velocity, createdTime = x.note.createdTime}} else x) newModel.noteArray }
            , Cmd.none )

        UpdateNoteArray newNoteArray ->
            let
                newModel =
                    { model | noteArray = newNoteArray }
            in
            ( newModel
            , postNoteArray (Array.map (\x -> x.note) newModel.noteArray)
            )

        SyncTodos result ->
            case result of
                Ok todoListJson ->
                    ( { model | todoList = List.sortBy .startTime (todoDecoderResultsHandler (Decoder.decodeValue (Decoder.list todoDecoder) todoListJson)) }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )

        SyncNotes result ->
            case result of
                Ok noteArrayJson ->
                    ( { model | noteArray = Array.map (\x -> {note = x, moving = False}) (noteDecoderResultsHandler (Decoder.decodeValue (Decoder.array noteDecoder) noteArrayJson)) }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )
        
        GetNewTime newTime ->
            ( { model
                | time = { now = newTime, zone = model.time.zone }
              }
            , Cmd.none
            )

        GetTimeZone timeZone ->
            ( { model | time = { now = model.time.now, zone = timeZone } }
            , Cmd.none
            )

        ModifyDOM _ -> (model, Cmd.none)

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Time.every 1000 GetNewTime, Browser.Events.onMouseMove (Decoder.map MoveNote positionDecoder) ]
    
    
    -- VIEW


timeLeadingZero : Int -> String
timeLeadingZero input =
    if input < 10 then
        "0" ++ String.fromInt input

    else
        String.fromInt input


timeStringGenerator : Model -> Time.Posix -> String
timeStringGenerator model timePosix =
    let
        timeModel =
            model.time

        roundedTimeMillis =
            (Time.posixToMillis timeModel.now // model.newTodo.length) * model.newTodo.length
    in
    if roundedTimeMillis >= Time.posixToMillis timePosix then
        "Now"
    else
        let
            hour =
                Time.toHour timeModel.zone timePosix

            minute =
                Time.toMinute timeModel.zone timePosix
        in
        if hour > 12 then
            timeLeadingZero (hour - 12) ++ ":" ++ timeLeadingZero minute

        else if hour == 0 then
            "12:" ++ timeLeadingZero minute

        else
            timeLeadingZero hour ++ ":" ++ timeLeadingZero minute


countdownStringGenerator : Int -> String
countdownStringGenerator millisRemaining =
    let
        secondsRemaining =
            millisRemaining // 1000

        minute =
            timeLeadingZero (secondsRemaining // 60)

        second =
            timeLeadingZero (modBy 60 secondsRemaining)
    in
    minute ++ ":" ++ second



lengthSelectionGenerator : Model -> Html Msg
lengthSelectionGenerator model =
    let
        selectableLengths = List.map (\x -> x*defaultTodo.length) (List.range 1 9)
        
        selectableLengthsHtml =
            List.map (\newLength -> div [onMouseOver (NewTodoLength newLength) ] [div [] [ input [ checked (model.newTodo.length == newLength), name "lengthSelector", type_ "radio"] [], text (countdownStringGenerator newLength) ]]) selectableLengths
    in
    span [ id "lengthSelection", style "display" "inline-block", style "width" "100%", style "height" "1.3em", style "white-space" "nowrap", style "overflow" "scroll", style "scrollbar-width" "none" ] selectableLengthsHtml

timeSelectionGenerator : Model -> Html Msg
timeSelectionGenerator model =
    let
        selectableTimes =
            getSelectableTimes model
    in
    let
        selectableTimesHtml =
            List.map (\newStartTime -> div [ onMouseOver (NewTodoStartTime newStartTime) ] [ input [ checked (model.newTodo.startTime == newStartTime), name "timeSelector", type_ "radio"] [], text (timeStringGenerator model (Time.millisToPosix newStartTime)) ]) selectableTimes
    in
    span [ id "timeSelection", style "display" "inline-block", style "width" "100%", style "height" "1.3em", style "white-space" "nowrap", style "overflow" "scroll", style "scrollbar-width" "none" ] selectableTimesHtml


getSelectableTimes : Model -> List Int
getSelectableTimes model =
    let
        roundedTime =
            (Time.posixToMillis model.time.now // model.newTodo.length) * model.newTodo.length

        occupiedTimeBlocks =
            List.map (\todo -> { startTime = todo.startTime, endTime = todo.startTime + todo.length }) model.todoList

        timeGaps =
            List.filter (\x -> (x.startTime < x.endTime) && (x.startTime > roundedTime))
                (List.indexedMap
                    (\i timeBlock ->
                        { startTime = timeBlock.endTime
                        , endTime =
                            case Array.get (i + 1) (Array.fromList occupiedTimeBlocks) of
                                Just t ->
                                    t.startTime

                                Nothing ->
                                    0
                        }
                    )
                    occupiedTimeBlocks
                )

        endOfOccupiedTime =
            Basics.max roundedTime (Maybe.withDefault roundedTime (List.maximum (List.map (\x -> x.endTime) occupiedTimeBlocks)))

        selectableGaps =
            Array.toList <| List.foldr Array.append (Array.fromList []) (List.map (\x -> Array.initialize ((x.endTime - x.startTime) // model.newTodo.length) (\i -> x.startTime + i * model.newTodo.length)) timeGaps)

        remainingTimes =
            Array.toList <| Array.initialize (model.timeSlotCount - List.length selectableGaps) (\i -> endOfOccupiedTime + i * model.newTodo.length)

        selectableTimes =
            selectableGaps ++ remainingTimes
    in
    selectableTimes


getNextFreeTime : Model -> Int
getNextFreeTime model =
    let
        roundedTime =
            (Time.posixToMillis model.time.now // model.newTodo.length) * model.newTodo.length

        selectableTimes =
            getSelectableTimes model
    in
    Maybe.withDefault roundedTime (List.head selectableTimes)


todoGenerator : ( Todo, Model ) -> Html Msg
todoGenerator ( todo, model ) =
    let
        time =
            model.time

        todoEndTime =
            todo.startTime + todo.length

        millisRemaining =
            todoEndTime - Time.posixToMillis time.now

        countdownHtml =
            if millisRemaining < todo.length then
                div [] [ text (countdownStringGenerator millisRemaining) ]

            else
                div [] [ text ("Do by: " ++ timeStringGenerator model (Time.millisToPosix todoEndTime)) ]
    in
    li [class "todo"]
        [ table []
            [ tr []
                [ td []
                    [ textarea [ onInput <| todoEditor todo model ] [ text todo.content ]
                    , div [] [ text ("Start by: " ++ timeStringGenerator model (Time.millisToPosix todo.startTime)) ]
                    ]
                , td [] [ countdownHtml ]
                , td [] [ input [ attribute "type" "checkbox" ] [] ]
                , td [] [ button [ onClick (RemoveTodo todo) ] [ text "done" ] ]
                ]
            ]
        ]


todoEditor : Todo -> Model -> String -> Msg
todoEditor todo model newContent =
    let
        reducedTodoList =
            List.filter (\oldTodo -> oldTodo /= todo) model.todoList

        editedTodoList =
            { todo | content = newContent } :: reducedTodoList
    in
    UpdateTodoList editedTodoList


noteEditor : Note -> Model -> String -> Msg
noteEditor note model newContent =
    let
        newNote =
            { note | content = newContent }

        noteIndex =
            List.foldl (+) 0 (List.map (\( i, n ) -> i) (List.filter (\( i, n ) -> n == note) (Array.toIndexedList <| Array.map (\x -> x.note) model.noteArray)))

        editedNoteArray =
            Array.set noteIndex { note = newNote, moving = False } model.noteArray
    in
    UpdateNoteArray editedNoteArray


overdueGenerator : Todo -> Html Msg
overdueGenerator todo =
    li [class "todo", class "overdue"]
        [ table []
            [ tr []
                [ td []
                    [ span [] [ text todo.content ] ]
                , td [] [ text "overdue" ]
                , td [] [ button [ onClick (RemoveTodo todo) ] [ text "done" ] ]
                ]
            ]
        ]


todoListGenerator : Model -> Html Msg
todoListGenerator model =
    if List.length model.todoList == 0 then 
        div [] [text "All done!"]
    else
        let
            overDueList =
                List.filter (\overdueTodo -> (overdueTodo.startTime + overdueTodo.length) < Time.posixToMillis model.time.now) model.todoList

            futureList =
                List.filter (\overdueTodo -> not <| List.member overdueTodo overDueList) model.todoList
        in
        div []
            [ ol [] (List.map overdueGenerator overDueList)
            , ol [] (List.map todoGenerator (List.map (\todo -> ( todo, model )) futureList))
            ]


noteGenerator : Note -> Model -> Html Msg
noteGenerator note model =
    let
        x =
            String.fromInt note.position.x ++ "px"

        y =
            String.fromInt note.position.y ++ "px"
    in
    div
        [ style "position" "fixed"
        , style "left" x
        , style "top" y
        ]
        [ div [class "note"]
            [ div []
                [ button [ onClick (UpdateNoteArray (removeNote note model)) ] [ text "x" ]
                , button
                    [ onMouseDown (UpdateNoteArray (setMovingNote note True model))
                    , onMouseUp (UpdateNoteArray (setMovingNote note False model))
                    , onDoubleClick (UpdateNoteArray (setMovingNote note True model))]
                    [ text "move" ]
                ]
            , textarea [ onInput <| noteEditor note model ] [ text note.content ]
            ]
        ]


noteArrayGenerator : Model -> Html Msg
noteArrayGenerator model =
    div [ style "z-index" "5", style "position" "fixed" ] (Array.toList (Array.map (\x -> x model) (Array.map noteGenerator (Array.map (\x -> x.note) model.noteArray))))


removeNote : Note -> Model -> Array.Array FloatingNote
removeNote note model =
    Array.filter (\x -> x.note /= note) model.noteArray


setMovingNote : Note -> Bool -> Model -> Array.Array FloatingNote
setMovingNote selectedNote state model =
    Array.append (Array.fromList [ { note = selectedNote, moving = state }]) (Array.filter (\x -> x.note /= selectedNote) model.noteArray)

view : Model -> Browser.Document Msg
view model =
    let
        whiteboard =
            span [ on "click" (Decoder.map SpawnNote positionDecoder), class "whiteboard", style "position" "fixed", style "top" "0", style "width" "100vw", style "height" "100vh", style "z-index" "1" ] []

        newTodo =
            cleanNewTodo model.newTodo model
    in
    { title = "Todo2 "
    , body =
        [ div [ style "display" "flex", style "justify-content" "center", style "width" "100vw", style "height" "100vh" ]
            [ whiteboard
            , article [ class "todoList", style "z-index" "3", style "position" "fixed" ]
                [ header []
                    [ h1 [] [ text ("Todo : " ++ timeStringGenerator model (Time.millisToPosix newTodo.startTime))] ]
                , main_ []
                    [ div []
                        [ span [] [ text "do " ]
                        , input [ value model.newTodo.content, onInput NewTodoContent ] []
                        , span [] [ span [] [ text "for " ], span [ style "display" "inline-block" ] [ lengthSelectionGenerator model ] ]
                        , span []
                            [ span [] [ text "at " ]
                            , span [ style "display" "inline-block" ] [ timeSelectionGenerator model ]
                            ]
                        , button [ onClick (SubmitNewTodo newTodo) ] [ text "+" ]
                    
                    ]
                    , hr [] []
                    ]
                , div []
                    [ todoListGenerator model ]
                ]
            , noteArrayGenerator model
            ]
        ]
    }
