module Dois exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

-- MODEL
type alias Model =
  { n1 : Int
  , n2 : Int
  , res : Int
  }
  
modelInicial : Model
modelInicial =
  Model 0 0 0

-- UPDATE
type Msg
    = Campo1 String -- Quando o ELM trabalha com formulário é obrigatório a utilização de String nos campos.
    | Campo2 String 
    | Somar
-- "valorDef" é uma função que confere o tipo de dado que recebe no input
valorDef : Result String Int -> Int -- espera receber como resultado um String OU int e retornar int
valorDef okx =
    case okx of
        Ok x -> x -- se receber int = OK
        Err _ -> 0 -- se receber String = ERRO, isto é, 0 (int)

updateView : Msg -> Model -> Model
updateView action model =
  case action of
    Campo1 x ->
      { model | n1 = (valorDef (String.toInt x)) } -- Na conversão para int, o parâmetro "x" previne a alteração do type do input no front.
    
    Campo2 x ->
      { model | n2 = (valorDef (String.toInt x)) }
    
    Somar -> -- Somar não possui valor pois na linha 22 ele não recebe nenhum valor
      { model | res = model.n1+model.n2 } -- Caso n2 receba "Qualquer String" no input, a linha 28 entrega "0" 

-- VIEW
viewSoma : Model -> Html Msg
viewSoma model =
  div []
    [ input [ type_ "number", placeholder "Digite um numero", onInput Campo1] []
    , br [] []
    , input [ type_ "number", placeholder "Digite outro numero", onInput Campo2] []
    , button [onClick Somar] [text "Somar"]
    , br [] []
    , div [style [("color", "blue")]] [text (toString model.res)] -- Converte o "res" (int) para String
    ] 

-- MAIN
main =
  beginnerProgram 
    { model = modelInicial
    , view = viewSoma
    , update = updateView 
    }