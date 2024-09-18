module Todo2 exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decoder
import Json.Encode as Encoder
import List
import Platform.Cmd as Cmd
import String exposing (fromInt)
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
    }


type alias MousePosition =
    { x : Int
    , y : Int
    }


type alias TimeModel =
    { zone : Time.Zone
    , now : Time.Posix
    }


type alias Model =
    { newTodo : Todo
    , todoList : List Todo
    , noteList : List Note
    , time : TimeModel
    , timeSlotCount : Int
    , timeSlotLength : Int
    }


defaultTimeSlotLength : Int
defaultTimeSlotLength =
    15 * 60 * 1000


initializeTime : Cmd Msg
initializeTime =
    Cmd.batch [ Task.perform GetNewTime Time.now, Task.perform GetTimeZone Time.here ]


init : () -> ( Model, Cmd Msg )
init _ =
    let
        blankModel =
            { newTodo = { content = "", startTime = 0, length = defaultTimeSlotLength }
            , todoList = []
            , noteList = []
            , time = { now = Time.millisToPosix 0, zone = Time.utc }
            , timeSlotCount = 9
            , timeSlotLength = defaultTimeSlotLength
            }
    in
    ( blankModel
    , Cmd.batch [ initializeTime, getTodoList ]
    )



-- UPDATE


type Msg
    = NewTodoContent String
    | NewTodoEndTime Int Bool
    | FinishTodo Todo
    | UpdateTodoList (List Todo)
    | SpawnNote MousePosition
    | GetNewTime Time.Posix
    | GetTimeZone Time.Zone
    | SyncServer (Result Http.Error Decoder.Value)


encodeTodoList : List Todo -> Encoder.Value
encodeTodoList todoList =
    Encoder.list todoEncoder todoList


todoEncoder : Todo -> Encoder.Value
todoEncoder todo =
    Encoder.object
        [ ( "content", Encoder.string todo.content )
        , ( "startTime", Encoder.int todo.startTime )
        , ( "length", Encoder.int todo.length )
        ]


todoDecoder : Decoder.Decoder Todo
todoDecoder =
    Decoder.map3 Todo
        (Decoder.field "content" Decoder.string)
        (Decoder.field "startTime" Decoder.int)
        (Decoder.field "length" Decoder.int)


mousePositionDecoder : Decoder.Decoder MousePosition
mousePositionDecoder =
    Decoder.map2 MousePosition
        (Decoder.field "pageX" Decoder.int)
        (Decoder.field "pageY" Decoder.int)


decodeMousePosition : Decoder.Value -> MousePosition
decodeMousePosition encodedPosition =
    mousePositionDecoderResultsHandler (Decoder.decodeValue mousePositionDecoder encodedPosition)


postTodoList : List Todo -> Cmd Msg
postTodoList todoList =
    Http.post
        { url = "http://localhost:7999/sync/todo"
        , body = Http.jsonBody (encodeTodoList todoList)
        , expect = Http.expectJson SyncServer Decoder.value
        }


getTodoList : Cmd Msg
getTodoList =
    Http.get
        { url = "http://localhost:7999/sync/todo"
        , expect = Http.expectJson SyncServer Decoder.value
        }


todoDecoderResultsHandler : Result Decoder.Error (List Todo) -> List Todo
todoDecoderResultsHandler result =
    case result of
        Err error ->
            let
                _ =
                    Debug.log "Error is " error
            in
            []

        Ok todoList ->
            todoList


noteDecoderResultsHandler : Result Decoder.Error (List Note) -> List Note
noteDecoderResultsHandler result =
    case result of
        Err error ->
            let
                _ =
                    Debug.log "Error is " error
            in
            []

        Ok noteList ->
            noteList


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


cleanNewTodo : ( Model, Todo ) -> Todo
cleanNewTodo ( model, todo ) =
    if todo.startTime == 0 then
        { todo | startTime = getNextFreeTime model }

    else
        todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTodoContent newContent ->
            ( { model | newTodo = { content = newContent, startTime = model.newTodo.startTime, length = model.timeSlotLength } }
            , Cmd.none
            )

        NewTodoEndTime selectedTime _ ->
            ( { model | newTodo = { content = model.newTodo.content, startTime = selectedTime, length = model.timeSlotLength } }
            , Cmd.none
            )

        FinishTodo todo ->
            let
                newTodoList =
                    List.filter (\x -> x /= todo) model.todoList
            in
            ( { model | todoList = newTodoList }
            , postTodoList newTodoList
            )

        UpdateTodoList newTodoList -> 
            ( let
                newModel = {model | todoList = List.sortBy .startTime newTodoList}
                _ = Debug.log "updated todo" newModel.todoList
            in
                newModel
            
            , postTodoList newTodoList)

        SpawnNote position ->
            let
                newNote =
                    { content = "", position = Debug.log "Click Position: " position }
            in
            ( { model | noteList = newNote :: model.noteList }, Cmd.none )

        GetNewTime newTime ->
            let
                cleanTodoList =
                    List.filter (\nilTodo -> not (nilTodo.content == "" && (nilTodo.startTime + nilTodo.length < Time.posixToMillis model.time.now))) model.todoList
            in
            ( { model
                | time = { now = newTime, zone = model.time.zone }
                , todoList = cleanTodoList
              }
            , Cmd.none
            )

        GetTimeZone timeZone ->
            ( { model | time = { now = model.time.now, zone = timeZone } }
            , Cmd.none
            )

        SyncServer result ->
            case result of
                Ok todoListJson ->
                    ( { model | todoList = List.sortBy .startTime (todoDecoderResultsHandler (Decoder.decodeValue (Decoder.list todoDecoder) todoListJson)) }
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
    in
    if Time.posixToMillis timeModel.now > Time.posixToMillis timePosix then
        "Now"
    else if Time.posixToMillis timeModel.now + model.timeSlotLength > Time.posixToMillis timePosix then
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


getSelectableTimes : Model -> Int -> List ( Int, Bool ) -> List ( Int, Bool )
getSelectableTimes model countRemaining targetTimeList =
    let
        time =
            model.time

        roundedTimeMillis =
            (Time.posixToMillis time.now // model.timeSlotLength) * model.timeSlotLength

        newSelectableTime =
            roundedTimeMillis + countRemaining * model.timeSlotLength

        occupiedTimes =
            List.map (\{ content, startTime, length } -> startTime) model.todoList

        selected =
            model.newTodo.startTime == newSelectableTime
    in
    if countRemaining >= 0 then
        if List.member newSelectableTime occupiedTimes then
            getSelectableTimes model (countRemaining - 1) targetTimeList
        else
            getSelectableTimes model (countRemaining - 1) (( newSelectableTime, selected ) :: targetTimeList)
    else
        targetTimeList


getNextFreeTime : Model -> Int
getNextFreeTime model =
    let
        selectableTimes =
            List.map (\( time, selected ) -> time) (getSelectableTimes model model.timeSlotCount [])
    in
    Maybe.withDefault (Time.posixToMillis model.time.now) (List.minimum selectableTimes)


timeSelectorGenerator : Model -> Html Msg
timeSelectorGenerator model =
    let
        time =
            model.time

        count =
            model.timeSlotCount

        selectableTimes =
            getSelectableTimes model count []
    in
    let
        selectableTimesHtml =
            List.map (\( newEndTime, selected ) -> span [] [ input [ checked selected, name "TimeSelector", type_ "radio", onCheck (NewTodoEndTime newEndTime) ] [], span [] [ text (timeStringGenerator model (Time.millisToPosix newEndTime)) ] ]) selectableTimes
    in
    span [ style "display" "inline-block", style "width" "100%", style "height" "1.3em", style "white-space" "nowrap", style "overflow" "scroll", style "scrollbar-width" "none" ] selectableTimesHtml


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
                    [ textarea [ onInput (todoEditor (todo, model)) ] [ text todo.content ]
                    , div [] [ text ("Start by: " ++ timeStringGenerator model (Time.millisToPosix todo.startTime)) ]
                    ]
                , td [] [ countdownHtml ]
                , td [] [ button [ onClick (FinishTodo todo) ] [ text "done" ] ]
                ]
            ]
        ]

todoEditor :  (Todo, Model) -> String -> Msg
todoEditor (todo, model) newContent = 
    let 
        reducedTodoList = List.filter (\oldTodo -> oldTodo /= todo) model.todoList

        editedTodoList = {todo | content = newContent} :: reducedTodoList
        _ = Debug.log "edited todo list" editedTodoList

    in
    UpdateTodoList editedTodoList

overdueGenerator : Todo -> Html Msg
overdueGenerator todo =
    li []
        [ table []
            [ tr []
                [ td []
                    [ span [] [ text todo.content ] ]
                , td [] [ text "overdue" ]
                , td [] [ button [ onClick (FinishTodo todo) ] [ text "done" ] ]
                ]
            ]
        ]


todoListGenerator : Model -> Html Msg
todoListGenerator model =
    let
        overDueList =
            List.filter (\overdueTodo -> (overdueTodo.startTime + overdueTodo.length) < Time.posixToMillis model.time.now) model.todoList

        futureList =
            List.filter (\overdueTodo -> not (List.member overdueTodo overDueList)) model.todoList
    in
    div []
        [ ol [] (List.map overdueGenerator overDueList)
        , ol [] (List.map todoGenerator (List.map (\todo -> (todo, model)) futureList))
        ]


noteGenerator : Note -> Html Msg
noteGenerator note =
    let
        x =
            fromInt note.position.x ++ "px"

        y =
            fromInt note.position.y ++ "px"
    in
    div
        [ style "position" "fixed"
        , style "left" x
        , style "top" y
        ]
        [ textarea [] [] ]


noteListGenerator : Model -> Html Msg
noteListGenerator model =
    div [ style "z-index" "5", style "position" "fixed" ] (List.map noteGenerator model.noteList)


view : Model -> Browser.Document Msg
view model =
    let
        whiteboard =
            span [ on "click" (Decoder.map SpawnNote mousePositionDecoder), class "Whiteboard", style "position" "fixed", style "top" "0", style "width" "100vw", style "height" "100vh", style "z-index" "1" ] []
        newTodoList = cleanNewTodo ( model, model.newTodo ) :: model.todoList
    in
    { title = "Todo2"
    , body =
        [ div [ style "display" "flex", style "justify-content" "center", style "width" "100vw", style "height" "100vh" ]
            [ whiteboard
            , article [ class "TodoList", style "z-index" "3", style "position" "fixed" ]
                [ header []
                    [ h1 [] [ text "Todo" ] ]
                , main_ []
                    [ div []
                        [ span [] [ text "What to do... " ]
                        , input [ value model.newTodo.content, onInput NewTodoContent ] []
                        , button [ onClick (UpdateTodoList newTodoList), onClick (NewTodoContent "")] [ text "+" ]
                        ]
                    , div []
                        [ span [] [ span [] [ text "When... " ], span [ style "display" "inline-block", style "width" "70%" ] [ timeSelectorGenerator model ] ]
                        ]
                    , hr [] []
                    ]
                , div []
                    [ todoListGenerator model ]
                ]
            , noteListGenerator model
            ]
        ]
    }
