# esboço redes neurais ----------------------------------------------------
library(caret)
library(dplyr)
library(nnet)

dados_dummy <- dummyVars(" ~ .", data = dados) %>%
  predict(dados) %>%
  as.data.frame()

set.seed(659)

dados_particao <- dados_dummy %>%
  dplyr::mutate(ID = row_number())

particao <- createDataPartition(dados_particao$status_doenca_final_trat.Óbito,
                                       p = 0.8,
                                       list = FALSE)

dados_treinamento <- dados_particao[particao, ]

dados_teste <- dados_particao[-particao, ]

library(doParallel)
cl <- makeCluster(detectCores() - 1) 

registerDoParallel(cl)

set.seed(123)
modelo_rede_neural <- nnet(
  status_doenca_final_trat.Óbito ~ .,               # Fórmula do modelo
  data = dados_treinamento,        # Conjunto de treinamento
  size = 5,                        # Número de neurônios na camada oculta
  rang = 0.1,                      # Inicialização dos pesos- testar 0,5 e 1
  decay = 5e-4,                    # Penalização (regularização), Testar valores entre 0.0001, 0.001 e 0.01
  maxit = 200                      # Número máximo de iterações, se ficar ruim testat 500 e 1000
)

# Visualizar o modelo treinado
print(modelo_rede_neural)

predicoes_prob <- predict(modelo_rede_neural, dados_teste, type = "raw")

predicoes <- ifelse(predicoes_prob > 0.5, 1, 0)

head(predicoes)

# Calcular a acurácia no conjunto de teste
# Converter ambas as variáveis (predições e classes reais) para fatores com os mesmos níveis
levels <- union(levels(as.factor(predicoes)), levels(as.factor(classes_reais)))

predicoes_factor <- factor(predicoes, levels = levels)
classes_reais_factor <- factor(classes_reais, levels = levels)

# Calcular a matriz de confusão
matriz_confusao <- caret::confusionMatrix(predicoes_factor, classes_reais_factor)

# Visualizar a matriz de confusão
print(matriz_confusao)

# Exibir a acurácia
print(paste("Acurácia do modelo no conjunto de teste: ", round(acuracia * 100, 2), "%"))

# Finalizar e desregistrar o cluster paralelo
stopCluster(cl)
registerDoSEQ()

rm(cl)
rm(list = ls(pattern = "^cl"))
gc()
