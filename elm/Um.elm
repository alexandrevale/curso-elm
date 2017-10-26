module Main exposing (..)

-- Importa todos os elementos HTML que serão necessários para a execução
import Html exposing (Html, button, div, text, beginnerProgram) 
-- Importa os eventos
import Html.Events exposing (onClick)

-- MODEL - O que sofre a ação do usuário
type alias Model = Int -- Indica que o Model da aplicação será um número (int), no caso o contador.
init : Model -- Indica que o Model é do tipo inteiro (linha 10)
init = 0 -- e que retorna 0
-- Exemplo da linha 11 e 12 em javascript
--function init(){
--    return 0;
--}

-- MESSAGES
type Msg
  = Increment Int -- Primeira mensagem (Increment) e valor (Int)
  | Decrement Int -- Segunda mensgem (Decrement) e valor (int)

-- Aqui definimos duas mensagens, uma para incremento 
-- e outra para decremento. Cada ação da aplicação é definida com
-- um verbo (em letra maiúscula) com seu valor ao lado.

-- VIEW
view : Model -> Html Msg -- Significa que a view receberá o Model e retornará o Html e Msg
-- Onde "view" é o nome da função, "Model" é a entrada (parâmetro) e "Html Msg" é a página (retorno + msg).
view model = -- Exemplo da página no arquivo ex-Um.html
  div []
    -- [elemento [atributo] [filho do elemento]
    [ button [ onClick (Increment 2) ] [ text "+" ]
    , button [ onClick (Decrement 2) ] [ text "-"]
    , text (toString model) ] -- Model é um inteiro, por esse motivo é necessária a conversão.
    
-- UPDATE
update : Msg -> Model -> Model -- "update" é a função que pega a "Msg" (que representa a ação na tela), um "Model" (tela atual) e um "Model" (tela atualizada)
update msg model = 
  case msg of -- "case" é o pattern match, semelhante ao "if"
    Increment qtd ->
      model + qtd
    
    Decrement qtd ->
      model - qtd

-- MAIN - Onde inicia o problema, definido o que é cada coisa no MVU
main =
  beginnerProgram
    { model = init
    , view = view
    , update = update
    }