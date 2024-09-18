package main

import (
	"encoding/json"
	"io"
	"io/fs"
	"log"
	"net/http"
	"os"
	"time"
)

type todo struct {
	Content   string `json:"content"`
	StartTime int    `json:"startTime"`
	Length    int    `json:"length"`
}

var todoList = []todo{}
var todoDefaultLength = 15 * 60 * 1000

func roundedTime(time int64) int {
	rounded := (int(time) / todoDefaultLength) * todoDefaultLength
	return rounded
}

func generateDefaultTodoList() []todo {
	return []todo{{Content: "Make a Todo!", StartTime: roundedTime(time.Now().UnixMilli()), Length: todoDefaultLength}}
}

type apiError struct {
	Err    string
	Status int
}

type apiErrorHandlerFunc func(http.ResponseWriter, *http.Request) error

func httpHandler(f apiErrorHandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if err := f(w, r); err != nil {
			writeJSON(w, http.StatusInternalServerError, apiError{Err: "Internal server error", Status: http.StatusInternalServerError})
		}
	}
}

func init() {
	initializeTodoList()
}

func initializeTodoList() {
	storedTodoList, readErr := os.ReadFile("todoList.json")
	if readErr != nil {
		encodedDefaultTodoList, marshalErr := json.Marshal(generateDefaultTodoList())
		if marshalErr != nil {
			log.Fatal("Failed creating the default Todo.", marshalErr)
		}
		writeErr := os.WriteFile("todoList.json", encodedDefaultTodoList, fs.ModePerm)
		if writeErr != nil {
			log.Fatal("Error when writing stored todo list", writeErr)
		}
		todoList = generateDefaultTodoList()
		return
	}
	unmarshalErr := json.Unmarshal(storedTodoList, &todoList)
	if unmarshalErr != nil {
		log.Fatal("Error when unmarshalling stored todo list: ", unmarshalErr)
	}
}

func main() {
	http.HandleFunc("/sync", httpHandler(syncTodos))
	http.ListenAndServe(":7999", nil)
}

func syncTodos(w http.ResponseWriter, r *http.Request) error {
	if len(todoList) == 0 {
		todoList = generateDefaultTodoList()
	}
	w.Header().Set("Access-Control-Allow-Origin", "http://localhost:8000")
	switch r.Method {
	case http.MethodOptions:
		w.Header().Set("Access-Control-Allow-Headers", "content-type")
		return nil
	case http.MethodPost:
		requestBody, requestBodyErr := io.ReadAll(r.Body)
		if requestBodyErr != nil {
			log.Fatal("Error when reading response body: ", requestBodyErr)
		}
		var postedTodoList []todo
		var unmarshalErr = json.Unmarshal(requestBody, &postedTodoList)
		if unmarshalErr != nil {
			log.Fatal("Error when unmarshalling response body: ", unmarshalErr)
		}
		todoList = postedTodoList
		os.WriteFile("todoList.json", requestBody, fs.ModePerm)
		return nil
	case http.MethodGet:
		return writeJSON(w, http.StatusOK, todoList)

	default:
		return writeJSON(w, http.StatusMethodNotAllowed, apiError{Err: "Invalid Method", Status: http.StatusMethodNotAllowed})
	}
}

func writeJSON(w http.ResponseWriter, status int, v any) error {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	return json.NewEncoder(w).Encode(v)
}
