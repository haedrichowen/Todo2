module Todo2 exposing (..)

import Array exposing (Array)
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



-- MAIN


main : Program () Model Msg
main =
    Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Todo =
    { content : String
    , endTime : Int
    }


type alias TimeModel =
    { zone : Time.Zone
    , now : Time.Posix
    }


type alias Model =
    { newTodo : Todo
    , todoList : List Todo
    , time : TimeModel
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
            , time = { now = Time.millisToPosix 0, zone = Time.utc }
            }
    in
    ( blankModel
    , Cmd.batch [ initializeTime ]
    )



-- UPDATE


type Msg
    = ChangeContent String
    | ChangeEndTime Int Bool
    | SubmitTodo
    | Finish Todo
    | GetNewTime Time.Posix
    | GetTimeZone Time.Zone
    | SyncServer (Result Http.Error Decoder.Value)


extractTodo : Todo -> String
extractTodo todo =
    todo.content ++ String.fromInt todo.endTime


encodeTodos : List Todo -> Encoder.Value
encodeTodos todoList =
    Encoder.list Encoder.string (List.map extractTodo todoList)


todoDecoder : Decoder.Decoder Todo
todoDecoder =
    Decoder.map2 Todo
        (Decoder.field "content" Decoder.string)
        (Decoder.field "endTime" Decoder.int)


postEncodedTodoList : Encoder.Value -> Cmd Msg
postEncodedTodoList encodedTodoList =
    let
        todoListJson =
            Http.jsonBody encodedTodoList
    in
    Http.post
        { url = "localhost:7999"
        , body = todoListJson
        , expect = Http.expectJson SyncServer Decoder.value
        }


todoDecoderResultsHandler : Result Decoder.Error (List Todo) -> List Todo
todoDecoderResultsHandler result =
    case result of
        Err _ ->
            []

        Ok todoList ->
            todoList


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
            let
                newModel =
                    { model
                        | newTodo = { content = "", endTime = 0 }
                        , todoList = model.todoList ++ [ cleanNewTodo ( model, model.newTodo ) ]
                    }
            in
            ( newModel
            , postEncodedTodoList (encodeTodos newModel.todoList)
            )

        Finish todo ->
            ( { model | todoList = List.filter (\x -> x /= todo) model.todoList }
            , Cmd.none
            )

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


timeStringGenerator : (TimeModel, Time.Posix) -> String
timeStringGenerator (timeModel, timePosix) =
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
getSelectableTimes ( model, count, selectableTimeList ) =
    let
        time =
            model.time

        roundedTimeMillis =
            (Time.posixToMillis time.now // timeBlockLength) * timeBlockLength

        newEndTime =
            roundedTimeMillis + count * 15 * 60 * 1000

        selected =
            if model.newTodo.endTime == newEndTime then
                True

            else
                False
    in
    if count > 0 then
        getSelectableTimes ( model, count - 1, ( newEndTime, selected ) :: selectableTimeList )

    else
        ( Time.posixToMillis time.now, True ) :: selectableTimeList


getNextFreeTime : Model -> Int
getNextFreeTime model =
    let
        selectableTimes =
            List.map (\( time, selected ) -> time) (getSelectableTimes ( model, 9, [] ))

        occupiedTimes =
            List.map (\{ content, endTime } -> endTime) model.todoList
    in
    Maybe.withDefault (Time.posixToMillis model.time.now) (List.minimum selectableTimes)


timeSelectorGenerator : ( Model, Int ) -> Html Msg
timeSelectorGenerator ( model, count ) =
    let
        time =
            model.time

        selectableTimes =
            getSelectableTimes ( model, count, [] )
    in
    let
        selectableTimesHtml =
            List.map (\( newEndTime, selected ) -> span [] [ input [ checked selected, name "TimeSelector", type_ "radio", onCheck (ChangeEndTime newEndTime) ] [], span [] [ text (timeStringGenerator (time, Time.millisToPosix newEndTime) ) ] ]) selectableTimes
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
                    , div [] [ text (timeStringGenerator (time, Time.millisToPosix todo.endTime )) ]
                    ]
                , td [] [ text (countdownStringGenerator secondsRemaining) ]
                , td [] [ button [ onClick (Finish todo) ] [ text "done" ] ]
                ]
            ]
        ]


todoListGenerator : ( List Todo, TimeModel ) -> Html Msg
todoListGenerator ( todoList, time ) =
    ol [] (List.map todoGenerator (List.map (\a -> ( a, time )) todoList))


view : Model -> Browser.Document Msg
view model =
    { title = "Todo2"
    , body =
        [ article []
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
                    , span [] [ timeSelectorGenerator ( model, 9 ) ]
                    ]
                , hr [] []
                ]
            , div []
                [ div [] [ todoListGenerator ( model.todoList, model.time ) ]
                ]
            ]
        ]
    }
