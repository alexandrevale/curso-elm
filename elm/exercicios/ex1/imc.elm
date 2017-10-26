module Dois exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

-- MODEL
type alias Model =
  { peso : Int
  , altura : Int
  , res : Float
  , analise : String
  }
  
modelInicial : Model
modelInicial =
  Model 0 0 0.0 ""

-- UPDATE
type Msg
    = Campo1 String -- Quando o ELM trabalha com formulário é obrigatório a utilização de String nos campos.
    | Campo2 String
    | Calcular
    | Resultado
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
        { model | peso = (valorDef (String.toInt x)) }
    Campo2 x ->
        { model | altura = (valorDef (String.toInt x)) }
    Calcular ->
        { model | res = model.peso/(model.altura ^ 2) }  -- IMC = peso ÷ (altura)²
    Resultado res ->
        {   if res < 17 then model | analise = "Muito abaixo do peso" 
            else if res >= 17 || res <= 18.49 then model | analise = "Abaixo do peso"
            else if res >= 18.5 || res <= 24.99 then model | analise = "Peso normal"
            else if res >= 25 || res <= 29.99 then model | analise = "Acima do peso"
            else if res >= 30 || res <= 34.99 then model | analise = "Obesidade I"  
            else if res >= 35 || res <= 39.99 then model | analise = "Obesidade II (severa)"
            else res >= 40 then model | analise = "Obesidade III (mórbida)" 
        }

-- VIEW
viewSoma : Model -> Html Msg
viewSoma model =
  div []
    [ input [ type_ "number", placeholder "Seu peso em kg", onInput Campo1] []
    , br [] []
    , input [ type_ "number", placeholder "Sua altura em cm", onInput Campo2] []
    , br [] []    
    , button [onClick Calcular] [text "Calcular"]
    , br [] []
    , p [] [text (toString model.res)]
    , p [] [text modal.analise]
    ] 

-- MAIN
main =
  beginnerProgram 
    { model = modelInicial
    , view = viewSoma
    , update = updateView 
    }