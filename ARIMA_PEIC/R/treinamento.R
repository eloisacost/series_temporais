# Ajuste de modelos
fit <- treino %>%
  model(
    arima_310 = ARIMA(valor ~ pdq(3,1,0)),
    arima_013 = ARIMA(valor ~ pdq(0,1,3)),
    arima_312 = ARIMA(valor ~ pdq(3,1,2)),
    arima_210 = ARIMA(valor ~ pdq(2,1,0)),
    arima_012 = ARIMA(valor ~ pdq(0,1,2)),
    arima_212 = ARIMA(valor ~ pdq(2,1,2)),
    arima_110 = ARIMA(valor ~ pdq(1,1,0)),
    arima_011 = ARIMA(valor ~ pdq(0,1,1)),
    arima_111 = ARIMA(valor ~ pdq(1,1,1)),
    rb = ARIMA(valor ~ pdq(0,1,0)),
    auto = ARIMA(valor, stepwise = FALSE, approx = FALSE),
    ETS1 = ETS(valor ~ error("A") + trend("A") + season("N")),
    ETS2 = ETS(valor ~ error('A') + trend('M') + season('N'))
  )

# Avaliação de desempenho
fit %>% select(auto)
glance(fit) 

# Validação com dados de teste (2025)
previsao_teste <- fit %>% forecast(new_data = teste)

#Tabela MAPE
accuracy(previsao_teste, teste) %>% 
  arrange(MAPE) %>% 
  select(.model, RMSE, MAPE) %>% 
  print()

# Ajuste do modelo definitivo com base total
modelo_definitivo <- peic_ts %>%
  model(
    campeao = ETS(valor ~ error('A') + trend('M') + season('N'))
  )
