# Ajuste de modelos
fit <- treino %>%
  model(
    arima_011_011 = ARIMA(valor ~ pdq(0,1,1) + PDQ(0,1,1)),
    arima_210_011 = ARIMA(valor ~ pdq(2,1,0) + PDQ(0,1,1)),
    arima_410_011 = ARIMA(valor ~ pdq(4,1,0) + PDQ(0,1,1)),
    arima_013_011 = ARIMA(valor ~ pdq(0,1,3) + PDQ(0,1,1)),
    arima_413_011 = ARIMA(valor ~ pdq(4,1,3) + PDQ(0,1,1)),
    auto = ARIMA(valor, stepwise = FALSE, approx = FALSE)
  )

# Modelo automático selecionado
fit %>% select(auto)

# Desempenho dos modelos (Diagnóstico)
glance(fit)

# (Ano de 2024 - Validação)
previsao_teste <- fit %>% forecast(new_data = teste)

# Tabela de Acurácia
accuracy(previsao_teste, varejo_ts) %>% 
  arrange(MAPE) %>% 
  select(.model, RMSE, MAPE)

# Ajuste do modelo definitivo com base total
modelo_definitivo <- varejo_ts %>%
  model(
    campeao_elo = ARIMA(valor ~ pdq(0,1,3) + PDQ(0,1,1))
  )