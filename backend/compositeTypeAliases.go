package main

type todo struct {
	Content   string `json:"content"`
	StartTime int    `json:"startTime"`
	Length    int    `json:"length"`
}

type position struct {
	X int `json:"x"`
	Y int `json:"y"`
}

type note struct {
	Content     string   `json:"content"`
	Position    position `json:"position"`
	CreatedTime int      `json:"createdTime"`
}

type apiError struct {
	Err    string
	Status int
}
