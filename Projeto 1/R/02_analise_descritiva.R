library(forecast)
library(ggplot2)
library(gridExtra) # Se não tiver, o R instala ou você usa install.packages("gridExtra")

# --- 1. Leitura dos dados ---
# Ajuste o caminho conforme o seu computador

if(file.exists("../data/ts_inadimplencia.rds")) {
  ts_dados <- readRDS("../data/ts_inadimplencia.rds")
} else {
  ts_dados <- readRDS("C:/Users/Elô/Documents/UEPB/SERIES/Projeto 1/data/ts_inadimplencia.rds")
}

# --- 2. Evolução Temporal ---

g1 <- autoplot(ts_dados) +
  ggtitle("Evolução da Inadimplência (2013-2025)") +
  ylab("Taxa (%)") + xlab("Ano") +
  theme_minimal() +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") # Adicionei linha de tendência

print(g1) 

# --- 3. Decomposição (Entendendo os componentes) ---
# Mostra Tendência, Sazonalidade e Resíduos separados
decomposicao <- decompose(ts_dados, type = "additive")
g2 <- autoplot(decomposicao) +
  ggtitle("Decomposição: Tendência vs Sazonalidade") +
  theme_minimal()

print(g2)

# --- 4. Sazonalidade Detalhada ---
# O ggsubseriesplot mostra a média de cada mês e como ele variou ao longo dos anos
g3 <- ggsubseriesplot(ts_dados) +
  ggtitle("Variação por Mês (Subséries)") +
  ylab("Taxa (%)") +
  theme_minimal()

print(g3)

# --- 5. Correlogramas (ACF e PACF) ---
# Isso mostra matematicamente a "memória" da série
g_acf <- ggAcf(ts_dados) + ggtitle("Autocorrelação (ACF)") + theme_minimal()
g_pacf <- ggPacf(ts_dados) + ggtitle("Autocorrelação Parcial (PACF)") + theme_minimal()

# Imprime os dois juntos ou separados
print(g_acf)
print(g_pacf)

# --- 6. Plot Polar ---
# Mostra o ciclo como um relógio. Ajuda a ver se existe um ciclo repetitivo forte.
g_polar <- ggseasonplot(ts_dados, polar = TRUE) +
  ggtitle("Ciclo Sazonal (Polar)") +
  theme_minimal()

print(g_polar)
