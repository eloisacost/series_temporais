# PROJETO SARIMA

# 1.  pacotes
library(tidyverse)
library(fpp3)
library(dplyr)
library(TSA)
library(forecast)

# DADOS 

url_bcb <- "https://api.bcb.gov.br/dados/serie/bcdata.sgs.1455/dados?formato=csv"
dados_brutos <- read.csv(url_bcb, sep = ";", dec = ",")

varejo_ts <- dados_brutos %>%
  mutate(Data = dmy(data), Mes = yearmonth(Data)) %>%
  select(Mes, valor) %>%
  as_tsibble(index = Mes) %>%
  filter_index("2015 Jan" ~ "2024 Dec")

#  ANÁLISE EXPLORATÓRIA DA SÉRIE ORIGINAL

# Gráfico da Série Original
g_original <- autoplot(varejo_ts, valor) +
  labs(title = "Série Original: Vendas do Varejo", x = "Mês", y = "Índice") +
  theme_minimal()
print(g_original)

# B) Teste KPSS Original (Para provar que NÃO é estacionária)
# p-valor deverá ser menor que 0.05 (Rejeita a estacionariedade)

varejo_ts %>% features(valor, unitroot_kpss) %>% print()

# C) Decomposição STL (Mostra a tendência e sazonalidade separadas)

g_decomposicao <- varejo_ts %>%
  model(STL(valor ~ trend(window = 13) + season(window = "periodic"))) %>%
  components() %>%
  autoplot() +
  labs(title = "Decomposição STL da Série Original")
print(g_decomposicao)

#  SEPARAÇÃO EM TREINO E TESTE

treino <- varejo_ts %>% filter_index(. ~ "2023 Dec")
teste  <- varejo_ts %>% filter_index("2024 Jan" ~ .)

#  1ª DIFERENCIAÇÃO (TIRANDO A SAZONALIDADE)

treino_diff_sazonal <- treino %>%
  mutate(diff_12 = difference(valor, 12)) %>%
  drop_na()

treino_diff_sazonal %>% 
  features(diff_12, unitroot_kpss) %>% print()

# Gráfico ACF e PACF da 1ª Diferença

g_acf_1 <- treino_diff_sazonal %>%
  gg_tsdisplay(diff_12, plot_type = 'partial', lag_max = 36) +
  labs(title = "1ª Diferença (Sazonal) com ACF e PACF", subtitle = "Avaliando a necessidade da 2ª diferença")
print(g_acf_1)

# 2ª DIFERENCIAÇÃO (TIRANDO A TENDÊNCIA) (EM TORNO DA MÉDIA (d))

treino_diff_total <- treino_diff_sazonal %>%
  mutate(diff_final = difference(diff_12, 1)) %>%
  drop_na()

# Aqui o p-valor DEVE ser maior que 0.05 
treino_diff_total %>% 
  features(diff_final, unitroot_kpss) %>% print()

# Gráfico ACF e PACF Final (Série Estacionária)

g_acf_final <- treino_diff_total %>%
  gg_tsdisplay(diff_final, plot_type = 'partial', lag_max = 36) +
  labs(title = "Série Totalmente Estacionária com ACF e PACF", subtitle = "Base para escolha dos parâmetros p, d, q, P, D, Q")
print(g_acf_final)


#  MODELAGEM 
# Com base na análise visual da ACF e PACF e nas observações avançadas

fit <- treino %>%
  model(
   
    arima_011_011 = ARIMA(valor ~ pdq(0,1,1) + PDQ(0,1,1)),
    arima_210_011 = ARIMA(valor ~ pdq(2,1,0) + PDQ(0,1,1)),
    
    arima_410_011 = ARIMA(valor ~ pdq(4,1,0) + PDQ(0,1,1)),
    
    arima_013_011 = ARIMA(valor ~ pdq(0,1,3) + PDQ(0,1,1)),
    
    arima_413_011 = ARIMA(valor ~ pdq(4,1,3) + PDQ(0,1,1)),
    
    auto = ARIMA(valor, stepwise = FALSE, approx = FALSE)
  )


# CRITÉRIO DE INFORMAÇÃO (AICc)
print("--- AVALIAÇÃO DE AJUSTE: MENOR AICc VENCE ---")
glance(fit) %>% 
  arrange(AICc) %>% 
  select(.model, AICc, BIC) %>% 
  print()

# PREVISÃO E VALIDAÇÃO

 previsao_teste <- fit %>% forecast(new_data = teste)

accuracy(previsao_teste, varejo_ts) %>% 
  arrange(MAPE) %>% 
   select(.model, RMSE, MAPE) %>% 
    print()

# Gráfico visual da competição de modelos acertando 2024

g_validacao <- previsao_teste %>%
  autoplot(varejo_ts %>% filter_index("2022 Jan" ~ .), level = NULL) +
  labs(title = "Comparação de Modelos SARIMA no Teste (2024)", x = "Mês", y = "Índice") +
  theme_minimal()
print(g_validacao)


# PREVISÃO PARA 2025
# Usando o modelo campeão (arima_013_011) treinado na base completa

modelo_definitivo <- varejo_ts %>%
  model(
    # O modelo vencedor diagnosticado visualmente na ACF/PACF
    campeao_elo = ARIMA(valor ~ pdq(0,1,3) + PDQ(0,1,1))
  )

# Gerando a previsão para os próximos 12 meses (Janeiro a Dezembro de 2025)
previsao_futuro <- modelo_definitivo %>% forecast(h = "12 months")

# GRÁFICO FINAL 

g_futuro <- previsao_futuro %>%
  # Filtramos a partir de 2020 para o gráfico focar no cenário recente
  autoplot(varejo_ts %>% filter_index("2020 Jan" ~ .)) +
  labs(title = "Previsão Oficial: Vendas do Varejo para 2025",
       subtitle = "Projeção gerada pelo modelo vencedor: ARIMA(0,1,3)(0,1,1)[12]",
       y = "Índice de Vendas", 
       x = "Ano/Mês") +
  theme_minimal()

print(g_futuro)
