module Todo2 exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

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
    , endTimeMillis : Int
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


init : () -> ( Model, Cmd Msg )
init _ =
    let
        blankModel =
            { newTodo = { content = "", endTimeMillis = 0 }
            , todoList = []
            , time = { now = Time.millisToPosix 0, zone = Time.utc }
            }
    in
    ( blankModel
    , Cmd.batch [ getTimeZone, getNewTime ]
    )



-- UPDATE


type Msg
    = ChangeContent String
    | ChangeEndTimeMillis Int Bool
    | SubmitTodo
    | Finish Todo
    | GetNewTime Time.Posix
    | GetTimeZone Time.Zone
    | SyncServer (Result Http.Error Decoder.Value)


extractTodo : Todo -> String
extractTodo todo =
    todo.content ++ String.fromInt todo.endTimeMillis


encodeTodos : List Todo -> Encoder.Value
encodeTodos todoList =
    Encoder.list Encoder.string (List.map extractTodo todoList)


todoDecoder : Decoder.Decoder Todo
todoDecoder =
    Decoder.map2 Todo
        (Decoder.field "content" Decoder.string)
        (Decoder.field "endTimeMillis" Decoder.int)


defaultEndTimeMillis : Model -> Int
defaultEndTimeMillis model =
    if model.newTodo.endTimeMillis < Time.posixToMillis model.time.now then
        Time.posixToMillis model.time.now

    else
        model.newTodo.endTimeMillis


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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeContent newContent ->
            ( { model | newTodo = { content = newContent, endTimeMillis = model.newTodo.endTimeMillis } }
            , Cmd.none
            )

        ChangeEndTimeMillis newEndTime _ ->
            ( { model | newTodo = { content = model.newTodo.content, endTimeMillis = newEndTime } }
            , Cmd.none
            )

        SubmitTodo ->
            let
                newModel =
                    { model
                        | newTodo = { content = "", endTimeMillis = defaultEndTimeMillis model }
                        , todoList = model.todoList ++ [ model.newTodo ]
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

        GetTimeZone newTimeZone ->
            ( { model | time = { now = model.time.now, zone = newTimeZone } }
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


getNewTime : Cmd Msg
getNewTime =
    Task.perform GetNewTime Time.now


getTimeZone : Cmd Msg
getTimeZone =
    Task.perform GetTimeZone Time.here


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 GetNewTime



-- VIEW


timeNumberFormatter : Int -> String
timeNumberFormatter input =
    if input < 10 then
        "0" ++ String.fromInt input

    else
        String.fromInt input


timeStringGenerator : TimeModel -> String
timeStringGenerator model =
    let
        hour =
            timeNumberFormatter (Time.toHour model.zone model.now)

        minute =
            timeNumberFormatter (Time.toMinute model.zone model.now)
    in
    hour ++ ":" ++ minute


countdownStringGenerator : Int -> String
countdownStringGenerator secondsRemaining =
    let
        minute =
            String.fromInt (secondsRemaining // 60)

        second =
            String.fromInt (modBy 60 secondsRemaining)
    in
    minute ++ ":" ++ second


timeBlockLength : Int
timeBlockLength =
    15 * 60 * 1000


getSelectableTimes : ( Model, Int, List Int ) -> List Int
getSelectableTimes ( model, count, selectableTimeList ) =
    let
        time =
            model.time

        roundedTimeMillis =
            (Time.posixToMillis time.now // timeBlockLength) * timeBlockLength

        newEndTimeMillis =
            roundedTimeMillis + count * 15 * 60 * 1000
    in
    if count > 0 then
        getSelectableTimes ( model, count - 1, newEndTimeMillis :: selectableTimeList )

    else
        selectableTimeList

timeSelectorGenerator : Model -> Html Msg
timeSelectorGenerator model =
    let
        time =
            model.time
        
        selectableTimes =
            getSelectableTimes ( model, 9, [] )

    in
    span [] (List.map (\newEndTimeMillis -> span [] [ input [ name "TimeSelector", type_ "radio", onCheck (ChangeEndTimeMillis newEndTimeMillis) ] [], span [] [ text (timeStringGenerator { time | now = Time.millisToPosix newEndTimeMillis }) ] ]) selectableTimes)


todoGenerator : ( Todo, TimeModel ) -> Html Msg
todoGenerator ( todo, time ) =
    let
        secondsRemaining =
            (todo.endTimeMillis - Time.posixToMillis time.now) // 1000
    in
    li []
        [ table []
            [ tr []
                [ td []
                    [ div [] [ text todo.content ]
                    , div [] [ text (timeStringGenerator { time | now = Time.millisToPosix todo.endTimeMillis }) ]
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
                    [ input [ placeholder "What to do...", value model.newTodo.content, onInput ChangeContent ] []
                    , button [ onClick SubmitTodo ] [ text "+" ]
                    ]
                , hr [] []
                , div []
                    [ span [] [ text "By when..." ]
                    , span [] [ timeSelectorGenerator model ]
                    ]
                , hr [] []
                ]
            , div []
                [ div [] [ todoListGenerator ( model.todoList, model.time ) ]
                ]
            ]
        ]
    }
