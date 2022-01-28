module Main exposing (..)

import Browser
import Html exposing (div, text, ol, ul, li, input, button, Html)
import Html.Attributes exposing (type_, placeholder, style)
import Html.Events exposing (onClick, onInput)
import Array exposing (Array, push, append, slice, length, indexedMap, empty)
import Html.Attributes exposing (value)
import Html exposing (h1)


-- MAIN

main : Program () (Model) Msg
main = 
  Browser.sandbox 
  { init = init
  , update = update
  , view = view
  }


-- MODEL
  
type alias Model = 
    { todoList: Array String
    , doneList: List String
    , inputValue: String
    } 

type Msg
  = Typing String
  | Add
  | Done Int
  | Clear

init: Model
init = {inputValue = "", todoList = empty, doneList = []}


-- UPDATE

update: Msg -> Model -> Model
update msg model = 
    case msg of
        Typing currentValue -> 
            {model | inputValue = currentValue }
        Add -> 
            {model | inputValue = "", todoList = (push model.inputValue model.todoList )}
        Done indx -> 
            { model 
            | todoList = append (slice 0 indx model.todoList) (slice (indx + 1) (length model.todoList) model.todoList)
            , doneList = 
                case Array.get indx model.todoList of 
                    Maybe.Just item ->
                        List.append model.doneList [item]
                    Maybe.Nothing -> 
                        model.doneList
            }
        Clear -> 
            { model | todoList = empty, doneList = [] }
      

-- VIEW
      

view: Model -> Html Msg
view model =
  div [ style "margin" "10px", style "font-size" "200%"] [
      h1 [style "color" "blue"] [ text "Todo List" ]
      , input [type_ "text", value model.inputValue, placeholder "Enter a new todo item", onInput Typing] []
      , button [onClick Add] [text "Add"]
      , ol [] (Array.toList (todoList model))
      , div [] [
          if List.length model.doneList > 0 then 
            Html.b [] [text "Done"]
          else text ""
          , ul [style "text-decoration" "line-through", style "color" "gray"] (List.map (\item -> li [] [text item]) model.doneList)
          , button [onClick Clear] [ text "Clear List" ]
        ] 
  ]
  
todoList: Model -> Array (Html Msg)
todoList model =
  indexedMap generateListItem model.todoList


generateListItem: Int -> String -> Html Msg
generateListItem indx txt = 
  li [] [text txt
  , button [style "margin-left" "10px", style "color" "red", onClick (Done indx)] [text "done"]
  ]
  