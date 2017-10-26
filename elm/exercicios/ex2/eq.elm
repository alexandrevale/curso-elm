module Dois exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

-- MODEL
type alias Model =
  { a : Int
  , b : Int
  , c : Int
  , delta : Int
  , raizDelta : Float
  , x1 : Float
  , x2 : Float
  }
  
modelInicial : Model
modelInicial =
  Model 0 0 0 0 0.0 0.0 0.0

-- UPDATE
type Msg
    = Campo1 String
    | Campo2 String
    | Campo3 String
    | Calcular

valorDef : Result String Int -> Int 
valorDef okx =
    case okx of
        Ok x -> x 
        Err _ -> 0 

updateView : Msg -> Model -> Model
updateView action model =
  case action of
    Campo1 x ->
        { model | a = (valorDef (String.toInt x)) }
    Campo2 x ->
        { model | b = (valorDef (String.toInt x)) }
    Campo3 x ->
        { model | c = (valorDef (String.toInt x)) }        
    Calcular ->
        { if a == 0 && b == 0 && c == 0 then "Não é possível calcular"
          else 
            model | delta = b ^ 2 - 4 * a * c
            model | raizDelta = delta ^ 0.5
            model | x1 = (-b + raizDelta)/(2*a)
            model | x2 = (-b - raizDelta)/(2*a)
        }

-- VIEW
viewSoma : Model -> Html Msg
viewSoma model =
  div []
    [ label [for "a"] [text "A", input [ type_ "number", name "a", onInput Campo1] [] ]
    , br [] []
    , label [for "b"] [text "B", input [ type_ "number", name "b", onInput Campo2] [] ]
    , br [] []  
    , label [for "c"] [text "C", input [ type_ "number", name "c", onInput Campo3] [] ]
    , button [onClick Calcular] [text "Calcular"]
    , br [] []
    , p [] [text "Delta = " ++ (toString model.delta)]
    , p [] [text "x1 = " ++ (toString model.x1)]
    , p [] [text "x2 = " ++ (toString model.x2)]
    ] 

-- MAIN
main =
  beginnerProgram 
    { model = modelInicial
    , view = viewSoma
    , update = updateView 
    }