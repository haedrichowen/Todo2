module Todo2 exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decoder
import Json.Encode as Encoder
import List
import Task
import Time
import String exposing (fromInt)
import Platform.Cmd as Cmd



-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Todo =
    { content : String
    , endTime : Int
    }

type alias Note =
    { 
        content : String
    ,   position : MousePosition
    }

type alias MousePosition =
    {   x : Int
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
    }


initializeTime : Cmd Msg
initializeTime =
    Cmd.batch [ Task.perform GetNewTime Time.now, Task.perform GetTimeZone Time.here ]


init : () -> ( Model, Cmd Msg )
init _ =
    let
        blankModel =
            { newTodo = { content = "", endTime = 0 }
            , todoList = []
            , noteList = []
            , time = { now = Time.millisToPosix 0, zone = Time.utc }
            , timeSlotCount = 9
            }
    in
    ( blankModel
    , Cmd.batch [ initializeTime, getTodoList ]
    )



-- UPDATE


type Msg
    = ChangeContent String
    | ChangeEndTime Int Bool
    | SubmitTodo
    | Finish Todo
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
        , ( "endTime", Encoder.int todo.endTime )
        ]

todoDecoder : Decoder.Decoder Todo
todoDecoder =
    Decoder.map2 Todo
        (Decoder.field "content" Decoder.string)
        (Decoder.field "endTime" Decoder.int)

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
        { url = "http://localhost:7999/sync"
        , body = Http.jsonBody (encodeTodoList todoList)
        , expect = Http.expectJson SyncServer Decoder.value
        }

getTodoList : Cmd Msg
getTodoList =
    Http.get
        { url = "http://localhost:7999/sync"
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
            {x=0,y=0}

        Ok position ->
            position

cleanNewTodo : ( Model, Todo ) -> Todo
cleanNewTodo ( model, todo ) =
    if todo.endTime == 0 then
        { todo | endTime = getNextFreeTime model }

    else
        todo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeContent newContent ->
            ( { model | newTodo = { content = newContent, endTime = model.newTodo.endTime } }
            , Cmd.none
            )

        ChangeEndTime newEndTime _ ->
            ( { model | newTodo = { content = model.newTodo.content, endTime = newEndTime } }
            , Cmd.none
            )

        SubmitTodo ->
            let newModel =  { model | newTodo = { content = "", endTime = 0 }, todoList = model.todoList ++ [ cleanNewTodo ( model, model.newTodo ) ] }
            in
            ( newModel
            , postTodoList newModel.todoList
            )

        Finish todo ->
            let newTodoList = List.filter (\x -> x /= todo) model.todoList
            in
            ( { model | todoList = newTodoList }
            , postTodoList newTodoList
            )

        SpawnNote position -> 
            let newNote = {content =  "", position = Debug.log "Click Position: " position}
            in
            ({model | noteList = newNote :: model.noteList}, Cmd.none)

        GetNewTime newTime ->
            ( { model | time = { now = newTime, zone = model.time.zone } }
            , Cmd.none
            )

        GetTimeZone timeZone ->
            ( { model | time = { now = model.time.now, zone = timeZone } }
            , Cmd.none
            )

        SyncServer result ->
            case result of
                Ok todoListJson ->
                    ( { model | todoList = todoDecoderResultsHandler (Decoder.decodeValue (Decoder.list todoDecoder) todoListJson) }
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


timeStringGenerator : ( TimeModel, Time.Posix ) -> String
timeStringGenerator ( timeModel, timePosix ) =
    if timeModel.now == timePosix then
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

        else
            timeLeadingZero hour ++ ":" ++ timeLeadingZero minute


countdownStringGenerator : Int -> String
countdownStringGenerator secondsRemaining =
    let
        minute =
            timeLeadingZero (secondsRemaining // 60)

        second =
            timeLeadingZero (modBy 60 secondsRemaining)
    in
    minute ++ ":" ++ second


timeBlockLength : Int
timeBlockLength =
    15 * 60 * 1000


getSelectableTimes : ( Model, Int, List ( Int, Bool ) ) -> List ( Int, Bool )
getSelectableTimes ( model, countRemaining, selectableTimeList ) =
    let
        time =
            model.time

        roundedTimeMillis =
            (Time.posixToMillis time.now // timeBlockLength) * timeBlockLength

        newEndTime =
            roundedTimeMillis + countRemaining * 15 * 60 * 1000

        selected =
            if model.newTodo.endTime == newEndTime then
                True

            else
                False
    in
    if countRemaining > 0 then
        getSelectableTimes ( model, countRemaining - 1, ( newEndTime, selected ) :: selectableTimeList )

    else
        ( Time.posixToMillis time.now, True ) :: selectableTimeList


getNextFreeTime : Model -> Int
getNextFreeTime model =
    let
        selectableTimes =
            List.map (\( time, selected ) -> time) (getSelectableTimes ( model, model.timeSlotCount, [] ))

        occupiedTimes =
            List.map (\{ content, endTime } -> endTime) model.todoList

        availableTimes =
            List.filter (\time -> List.member time occupiedTimes) selectableTimes
    in
    Maybe.withDefault (Time.posixToMillis model.time.now) (List.minimum availableTimes)


timeSelectorGenerator : Model -> Html Msg
timeSelectorGenerator model =
    let
        time =
            model.time

        count =
            model.timeSlotCount

        selectableTimes =
            getSelectableTimes ( model, count, [] )
    in
    let
        selectableTimesHtml =
            List.map (\( newEndTime, selected ) -> span [] [ input [ checked selected, name "TimeSelector", type_ "radio", onCheck (ChangeEndTime newEndTime) ] [], span [] [ text (timeStringGenerator ( time, Time.millisToPosix newEndTime )) ] ]) selectableTimes
    in
    span [] selectableTimesHtml


todoGenerator : ( Todo, TimeModel ) -> Html Msg
todoGenerator ( todo, time ) =
    let
        secondsRemaining =
            (todo.endTime - Time.posixToMillis time.now) // 1000
    in
    li []
        [ table []
            [ tr []
                [ td []
                    [ div [] [ text todo.content ]
                    , div [] [ text (timeStringGenerator ( time, Time.millisToPosix todo.endTime )) ]
                    ]
                , td [] [ text (countdownStringGenerator secondsRemaining) ]
                , td [] [ button [ onClick (Finish todo) ] [ text "done" ] ]
                ]
            ]
        ]


todoListGenerator : Model -> Html Msg
todoListGenerator model =
    ol [] (List.map todoGenerator (List.map (\a -> ( a, model.time )) model.todoList))

noteGenerator : Note -> Html Msg
noteGenerator note =
    let 
        x = fromInt note.position.x ++ "px"
        y = fromInt note.position.y ++ "px"
    in
    div [ style "position" "fixed"
        , style "left" x
        , style "top" y
        ] [ textarea [] [] ]

noteListGenerator : Model -> Html Msg
noteListGenerator model = 
    div [] (List.map noteGenerator model.noteList)

view : Model -> Browser.Document Msg
view model =
    let whiteboard = span [on "click" (Decoder.map SpawnNote mousePositionDecoder), class "Whiteboard", style "min-width" "17%"] []
    in
    { title = "Todo2"
    , body =
        [ div [ style "display" "flex", style "flex-direction" "row", style "justify-content" "center" ] 
            [ whiteboard
             ,   article [ class "TodoList" ]
                    [ header []
                        [ h1 [] [ text "Todo" ] ]
                    , main_ []
                        [ div []
                            [ span [] [ text "What to do... " ]
                            , input [ value model.newTodo.content, onInput ChangeContent ] []
                            , button [ onClick SubmitTodo ] [ text "+" ]
                            ]
                        , div []
                            [ span [] [ text "When... " ]
                            , span [] [ timeSelectorGenerator model ]
                            ]
                        , hr [] []
                        ]
                    , div []
                        [ div [] [ todoListGenerator model ]
                        ]
                    ]
                , whiteboard
            , noteListGenerator model ]
        ]
    }
