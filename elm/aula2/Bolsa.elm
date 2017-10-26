-- Exemplo de JSON: https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=MSFT&apikey=demo

module Bolsa exposing (..)
-- Como padrão, é recomendado colocar todas as funções no "exposing"
import Html exposing (table, thead, tbody, tr, td, text, input, label, fieldset, div, form, button, Html, program)
import Html.Events exposing (onSubmit, onInput, onClick, on)
import Html.Attributes exposing (type_, placeholder, value, required, style, min)
import Http exposing (post, send, get)
import Json.Decode exposing (string, field, float, list, Decoder,map5,keyValuePairs )

type alias Stock = -- Função auxiliar
  { open_ : String
  , high : String
  , low : String
  , close : String
  , volume : String
  }

type alias Model =
  { symbol : String -- Nome da empresa, "2. Symbol" no JSON "Meta Data" do exemplo
  , stocks : List (String, Stock) -- Lista de tupla (vetor com duas posições) data (String) e o conteúdo do json
  }

init : Model
init = Model "" []

getStocks : String -> Cmd Msg -- Cmd indica AJAX
getStocks symb = send Resposta 
    <| get ("https://www.alphavantage.co/query?function=TIME_SERIES_DAILY&symbol=" 
            ++ symb 
            ++"&apikey=BWD5GUHO3AJMBYR0") decodeStock

decodeStock : Decoder (List (String,Stock))
decodeStock = 
        field "Time Series (Daily)" -- field indica acesso ao JSON, no caso, ao campo "Time Series (Daily)"
        <| keyValuePairs  <| 
        map5 Stock (field "1. open" string) -- <| indica (), isto é, "keyValuePairs" esta dentro de "field "Time Series (Daily)"" e pega "map5 Stock"
                   (field "2. high" string) -- O "string" esta em minúsculo pois indica função
                   (field "3. low"  string)
                   (field "4. close"  string)
                   (field "5. volume" string)
        -- JSON -> campos não numéricos 
        -- Lista -> campos numéricos, por isso o "keyValuePairs" vai pegar os 
type Msg 
  = Submit -- Envio do form
  | Symbol String -- A empresa que quero pesquisar
  | Resposta (Result Http.Error (List (String,Stock))) -- Result indica que é um ou outro, ou é "Http.Error" ou a "List (String,Stock)"

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Symbol digitado ->
            ({ model | symbol = digitado }, Cmd.none)

        Submit ->
            (model, getStocks model.symbol)

        Resposta resp -> 
            case resp of -- O "case" ocorre porque o Result recebe dois parâmetros
                Err x -> ({ model | symbol = "Inexistent Symbol!" }, Cmd.none)
                Ok lista -> ({model | stocks = lista}, Cmd.none)


tiraAspas : String -> String
tiraAspas palavra = String.filter (\x -> x /= '\"') palavra

viewStock : (String,Stock) -> Html Msg
viewStock (dia,stock) =
    tr [] [ td [] [text <| tiraAspas <| toString dia]
          , td [] [text <| tiraAspas <| toString stock.open_]
          , td [] [text <| tiraAspas <| toString stock.high]
          , td [] [text <| tiraAspas <| toString stock.low]
          , td [] [text <| tiraAspas <| toString stock.close]
          , td [] [text <| tiraAspas <| toString stock.volume]
          ]

view : Model -> Html Msg
view model =
    div []
        [ form [onSubmit Submit]
            [ input [type_ "text", required True, placeholder "Nome", onInput Symbol] []
            , button [] [text "Enviar"]
            ]
          , div [] [text model.symbol]
          , table []
                [ thead [] [tr [] [ td [] [text "Dia"]
                                  , td [] [text "Abertura"]
                                  , td [] [text "Alta"]
                                  , td [] [text "Baixa"]
                                  , td [] [text "Fechamento"]
                                  , td [] [text "Volume"]
                                  ]
                            ]
                , tbody [] (List.map viewStock model.stocks)
                ]
        ]

main =
  program -- Devido a utilização do AJAX não podemos usar o "beginnerProgram"
    { init = (init, Cmd.none)
    , view = view
    , update = update
    , subscriptions = \_ -> Sub.none
    }
