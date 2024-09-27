module Todo2 exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decoder
import Json.Encode as Encoder
import List
import Array
import Platform.Cmd as Cmd
import String
import Task
import Time


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
    , position : MousePosition
    , createdTime : Int
    }


type alias MousePosition =
    { x : Int
    , y : Int
    }


type alias TimeModel =
    { zone : Time.Zone
    , now : Time.Posix
    }

type alias TimeBlock = 
    { startTime : Int
    , endTime : Int }


type alias Model =
    { newTodo : Todo
    , todoList : List Todo
    , noteArray : Array.Array Note
    , time : TimeModel
    , timeSlotCount : Int
    }


defaultTimeSlotLength : Int
defaultTimeSlotLength =
    15 * 60 * 1000

defaultTodo : Todo
defaultTodo = { content = "Make a Todo!", startTime = 0, length = defaultTimeSlotLength}

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
            }
    in
    ( blankModel, Cmd.batch [ initializeTime, getTodoList, getNoteArray ])



-- UPDATE


type Msg
    = NewTodoContent String
    | NewTodoStartTime Int
    | NewTodoLength Int
    | SubmitNewTodo Todo
    | RemoveTodo Todo
    | UpdateTodoList (List Todo)
    | SpawnNote MousePosition
    | UpdateNoteArray (Array.Array Note)
    | GetNewTime Time.Posix
    | GetTimeZone Time.Zone
    | SyncTodos (Result Http.Error Decoder.Value)
    | SyncNotes (Result Http.Error Decoder.Value)


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
            encodedPosition = Encoder.object 
                [ ("x", Encoder.int note.position.x)
                , ("y", Encoder.int note.position.y)
                ]
        in
        Encoder.object
            [ ("content", Encoder.string note.content) 
            , ("position", encodedPosition)
            , ("createdTime", Encoder.int note.createdTime)
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
            (Decoder.map2 MousePosition 
                (Decoder.field "x" Decoder.int)
                (Decoder.field "y" Decoder.int)
            )
        )
        (Decoder.field "createdTime" Decoder.int)


mousePositionDecoder : Decoder.Decoder MousePosition
mousePositionDecoder =
    Decoder.map2 MousePosition
        (Decoder.field "x" Decoder.int)
        (Decoder.field "y" Decoder.int)


decodeMousePosition : Decoder.Value -> MousePosition
decodeMousePosition encodedPosition =
    mousePositionDecoderResultsHandler (Decoder.decodeValue mousePositionDecoder encodedPosition)


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


mousePositionDecoderResultsHandler : Result Decoder.Error MousePosition -> MousePosition
mousePositionDecoderResultsHandler result =
    case result of
        Err error ->
            let
                _ =
                    Debug.log "Error is " error
            in
            { x = 0, y = 0 }

        Ok position ->
            position


cleanNewTodo : Todo -> Model -> Todo
cleanNewTodo todo model =
    let roundedTimeMillis =
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

        NewTodoLength newLength -> ( { model | newTodo = { content = model.newTodo.content, startTime = model.newTodo.startTime, length = newLength } }
            , Cmd.none
            )

        SubmitNewTodo newTodo -> 
            let 
                newModel = { model | todoList = List.sortBy .startTime (cleanNewTodo newTodo model :: model.todoList) }
            in
            ({ newModel
                | newTodo = { content = "", startTime = getNextFreeTime newModel, length = model.newTodo.length}}
            , postTodoList newModel.todoList)

        RemoveTodo todo ->
            let
                newTodoList = List.filter (\x -> x /= todo) model.todoList
                newModel = 
                    if List.isEmpty newTodoList then
                        { model | todoList = [{defaultTodo | startTime = getNextFreeTime {model | todoList = newTodoList}}] }
                    else
                        { model | todoList = newTodoList }                
            in
            ( {newModel | newTodo = { content = "", startTime = getNextFreeTime newModel, length = model.newTodo.length}}, postTodoList newModel.todoList )
            

        UpdateTodoList newTodoList -> 
            let
                newModel = {model | todoList = List.sortBy .startTime newTodoList}
            in
            (  newModel
            
            , postTodoList newTodoList )

        SpawnNote position ->
            let
                newNote =
                    { content = "", position = position, createdTime = Time.posixToMillis model.time.now }
                newModel = { model | noteArray = Array.append model.noteArray (Array.fromList [newNote]) }
            in 
            ( newModel, postNoteArray newModel.noteArray )
        UpdateNoteArray newNoteArray -> 
            let
                newModel = { model | noteArray = newNoteArray }
            in
            ( newModel
            , postNoteArray newModel.noteArray)

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
                    ( { model | noteArray = noteDecoderResultsHandler (Decoder.decodeValue (Decoder.array noteDecoder) noteArrayJson) }
                    , Cmd.none
                    )

                Err _ ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 GetNewTime



-- VIEW


timeLeadingZero : Int -> String
timeLeadingZero input =
    if input < 10 then
        "0" ++ String.fromInt input

    else
        String.fromInt input


timeStringGenerator : Model -> Time.Posix -> String
timeStringGenerator model timePosix =
    let timeModel = model.time
        roundedTimeMillis =
            (Time.posixToMillis timeModel.now // model.newTodo.length) * model.newTodo.length
    in
    if roundedTimeMillis >= Time.posixToMillis timePosix then
        "Now"
    else if roundedTimeMillis + model.newTodo.length >= Time.posixToMillis timePosix then
        "Next"
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

timeSelectorGenerator : Model -> Html Msg
timeSelectorGenerator model =
    let
        selectableTimes =
            getSelectableTimes model
    in
    let
        selectableTimesHtml =
           List.map (\( newEndTime, selected ) -> span [] [ input [ checked selected, name "TimeSelector", type_ "radio", onCheck (\check -> NewTodoStartTime newEndTime) ] [], span [] [ text (timeStringGenerator model (Time.millisToPosix newEndTime)) ] ]) (applySelectedTime selectableTimes model)
    in
    span [ style "display" "inline-block", style "width" "100%", style "height" "1.3em", style "white-space" "nowrap", style "overflow" "scroll", style "scrollbar-width" "none" ] selectableTimesHtml

getSelectableTimes : Model -> List Int
getSelectableTimes model =
    let roundedTime = ((Time.posixToMillis model.time.now) // model.newTodo.length) * model.newTodo.length
        occupiedTimeBlocks = List.map (\todo -> {startTime = todo.startTime, endTime = todo.startTime + todo.length}) model.todoList
        timeGaps = 
            List.filter (\x -> x.startTime < x.endTime)
                (List.indexedMap 
                    (\i timeBlock -> 
                        {startTime = timeBlock.endTime
                        , endTime = 
                            case (Array.get (i + 1) (Array.fromList occupiedTimeBlocks)) of 
                                Just t -> t.startTime     
                                Nothing -> 0
                        }) 
            occupiedTimeBlocks)
        endOfOccupiedTime = Basics.max roundedTime (Maybe.withDefault roundedTime (List.maximum (List.map (\x -> x.endTime) occupiedTimeBlocks)))
        selectableGaps = Array.toList <| List.foldr (Array.append) (Array.fromList []) (List.map (\x -> Array.initialize ((x.endTime - x.startTime)//model.newTodo.length) (\i -> x.startTime + i * model.newTodo.length)) timeGaps)
        remainingTimes = Array.toList <| Array.initialize (model.timeSlotCount - List.length selectableGaps) (\i -> endOfOccupiedTime + i * model.newTodo.length)
        selectableTimes = selectableGaps ++ remainingTimes
    in selectableTimes

applySelectedTime : List Int -> Model -> List ( Int, Bool )
applySelectedTime selectableTimes model = 
    let
        userSelectedTime = model.newTodo.startTime
    in
    if userSelectedTime == 0 then
        List.indexedMap (\i t -> (t, i == 0)) selectableTimes
    else
        List.map (\t -> (t, userSelectedTime == t)) selectableTimes 

getNextFreeTime : Model -> Int
getNextFreeTime model =
    let
        roundedTime = (Time.posixToMillis model.time.now // model.newTodo.length) * model.newTodo.length
        selectableTimes = getSelectableTimes model
    in
    Maybe.withDefault roundedTime (List.head selectableTimes)

todoGenerator : (Todo, Model) -> Html Msg
todoGenerator (todo, model) =
    let
        time = model.time
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
    li []
        [ table []
            [ tr []
                [ td []
                    [ textarea [ onInput <| todoEditor todo model ] [ text todo.content ]
                    , div [] [ text ("Start by: " ++ timeStringGenerator model (Time.millisToPosix todo.startTime)) ]
                    ]
                , td [] [ countdownHtml ]
                , td [] [ button [ onClick (RemoveTodo todo) ] [ text "done" ] ]
                ]
            ]
        ]

todoEditor :  Todo -> Model -> String -> Msg
todoEditor todo model newContent = 
    let 
        reducedTodoList = List.filter (\oldTodo -> oldTodo /= todo) model.todoList
        editedTodoList = {todo | content = newContent} :: reducedTodoList
    in
    UpdateTodoList editedTodoList

noteEditor :  Note -> Model -> String -> Msg
noteEditor note model newContent = 
    let 
        newNote = { note | content = newContent}
        noteIndex = List.foldl (+) 0 (List.map (\(i, n) -> i) (List.filter (\(i, n) -> n == note) (Array.toIndexedList model.noteArray)))
        editedNoteArray = Array.set noteIndex newNote model.noteArray
        
    in
    UpdateNoteArray editedNoteArray

overdueGenerator : Todo -> Html Msg
overdueGenerator todo =
    li []
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
    let
        overDueList =
            List.filter (\overdueTodo -> (overdueTodo.startTime + overdueTodo.length) < Time.posixToMillis model.time.now) model.todoList

        futureList =
            List.filter (\overdueTodo -> not <| List.member overdueTodo overDueList) model.todoList

    in
    div []
        [ ol [] (List.map overdueGenerator overDueList)
        , ol [] (List.map todoGenerator (List.map (\todo -> (todo, model)) futureList))
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
        [ div [] [div [] [ button [ onClick (UpdateNoteArray (removeNote note model)) ] [ text "x" ], button [ onClick (UpdateNoteArray (moveNote note model)) ] [ text "move" ] ], textarea [ onInput <| noteEditor note model ] [ text note.content ]] ]


noteArrayGenerator : Model -> Html Msg
noteArrayGenerator model =
    div [ style "z-index" "5", style "position" "fixed" ] (Array.toList (Array.map (\x -> x model) (Array.map noteGenerator model.noteArray)))

removeNote : Note -> Model -> Array.Array Note
removeNote note model = Array.filter (\x -> x /= note) model.noteArray
moveNote : Note -> Model -> Array.Array Note
moveNote note model = Array.filter (\x -> x  /= note) model.noteArray

view : Model -> Browser.Document Msg
view model =
    let
        whiteboard =
            span [ on "click" (Decoder.map SpawnNote mousePositionDecoder), class "Whiteboard", style "position" "fixed", style "top" "0", style "width" "100vw", style "height" "100vh", style "z-index" "1" ] []
        newTodo = cleanNewTodo model.newTodo model
    in
    { title = "Todo2"
    , body =
        [ div [ style "display" "flex", style "justify-content" "center", style "width" "100vw", style "height" "100vh" ]
            [ whiteboard
            , article [ class "TodoList", style "z-index" "3", style "position" "fixed" ]
                [ header []
                    [ h1 [] [ text ("Todo : " ++ timeStringGenerator model (Time.millisToPosix newTodo.startTime)) ] ]
                , main_ []
                    [ div []
                        [ span [] [ text "What to do... " ]
                        , input [ value model.newTodo.content, onInput NewTodoContent ] []
                        , button [ onClick (SubmitNewTodo newTodo)] [ text "+" ]
                        ]
                        , div [] 
                        [ span [] 
                            [ span [] [ text "How long " ]
                            , button [ onClick <| NewTodoLength (model.newTodo.length - 5*60*1000) ] [text "-"], span [] [ text <| countdownStringGenerator model.newTodo.length ]
                            , button [ onClick <| NewTodoLength (model.newTodo.length + 5*60*1000) ] [text "+"]
                            ]
                        ]
                        , div []
                        [ span [] 
                            [   span [] [ text "When " ]
                            ,   span [ style "display" "inline-block", style "width" "70%" ] [ timeSelectorGenerator model ]
                            ]
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
