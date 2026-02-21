
# MODELAGEM_PREVISAO 

library(forecast)
library(ggplot2)
library(knitr)

# Divisão treino e teste
# Treino: Até Dez/2024
# Teste: Jan/2025 em diante (para ver se o modelo acerta)
treino <- window(ts_dados, end = c(2024, 12))
teste  <- window(ts_dados, start = c(2025, 1))

# Treinamento (Holt-Winters)
# Otimização automática de alpha, beta e gamma
modelo_hw <- HoltWinters(treino)

# Validação
previsao_teste <- forecast(modelo_hw, h = length(teste))

# A) Gráfico: Real vs Previsto (Fundamental para justificar o modelo)
# Mostra se a linha azul (previsão) acompanhou a linha preta (realidade)
g_validacao <- autoplot(treino) +
  autolayer(previsao_teste, series="Previsão (Modelo)", PI=FALSE) +
  autolayer(teste, series="Dados Reais (Teste)") +
  ggtitle("Validação do Modelo: Previsão vs Realidade (2025)") +
  xlab("Ano") + ylab("Taxa (%)") +
  theme_minimal() +
  scale_color_manual(values=c("black", "red"))

print(g_validacao) 

# Tabela de Parâmetros
tabela_params <- data.frame(
  Parametro = c("Alpha (Nível)", "Beta (Tendência)", "Gamma (Sazonalidade)"),
  Valor = c(0.7183, 0.0293, 1.0000),
  Significado = c("Alta sensibilidade ao curto prazo", 
                  "Crescimento estável", 
                  "Sazonalidade altamente adaptável")
)

print(kable(tabela_params, caption = "Parâmetros de Suavização do Modelo Holt-Winters"))

# B) Métricas de Erro
print("--- MÉTRICAS DE ACURÁCIA (RMSE, MAPE) ---")
print(accuracy(previsao_teste, teste))

# Diagnóstico de resíduos
print("--- DIAGNÓSTICO DE RESÍDUOS (Ljung-Box) ---")
checkresiduals(modelo_hw) 

# Tabela de Previsão Formatada

df_previsao <- data.frame(
  Data = time(previsao_teste$mean),
  Previsao = as.numeric(previsao_teste$mean),
  Limite_Inferior = as.numeric(previsao_teste$lower[,2]), # 95%
  Limite_Superior = as.numeric(previsao_teste$upper[,2])  # 95%
)

# Arredonda os valores
df_previsao <- round(df_previsao, 2)

# Gera a tabela no relatório
print(kable(df_previsao, 
            col.names = c("Data", "Previsão (%)", "Min (95%)", "Max (95%)"),
            caption = "Previsão Pontual para 2025 com Intervalos de Confiança"))

# Previsão Final 

modelo_final <- HoltWinters(ts_dados)
previsao_futura <- forecast(modelo_final, h = 12)

# Gráfico Final
g_final <- autoplot(previsao_futura) +
  ggtitle("Previsão Final de Inadimplência (Próximos 12 meses)") +
  ylab("Taxa (%)") +
  theme_minimal()

print(g_final)
