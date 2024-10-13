
# esboço redes neurais ----------------------------------------------------
# Carregar pacotes necessários
library(caret)
library(dplyr)
library(nnet)

# Exemplo de um conjunto de dados categórico chamado 'dados'
# Vamos supor que o nome do data frame seja 'dados'

dados = dados_filtrados_sem_na

# Transformar variáveis categóricas usando dummy variables (one-hot encoding)
dados_dummy <- dummyVars(" ~ .", data = dados) %>%
  predict(dados) %>%
  as.data.frame()


dados = dados_dummy

# Definir semente para reprodutibilidade
set.seed(659)

# Adicionar identificador
dados_particao <- dados %>%
  dplyr::mutate(ID = row_number())

# Primeira partição: 70% para treinamento, 30% para teste e validação
particao_treino <- createDataPartition(dados_particao$status_doenca_final_trat,
                                       p = 0.8,
                                       list = FALSE)
dados_treinamento <- dados_particao[particao_treino, ]

dados_teste <- dados_particao[-particao_treino, ]

# Treinamento do modelo com o pacote nnet
set.seed(123)
modelo_rede_neural <- nnet(
  status_doenca ~ .,               # Fórmula do modelo
  data = dados_treinamento,        # Conjunto de treinamento
  size = 5,                        # Número de neurônios na camada oculta
  rang = 0.1,                      # Inicialização dos pesos- testar 0,5 e 1
  decay = 5e-4,                    # Penalização (regularização), Testar valores entre 0.0001, 0.001 e 0.01
  maxit = 200                      # Número máximo de iterações, se ficar ruim testat 500 e 1000
)

# Visualizar o modelo treinado
print(modelo_rede_neural)

predicoes <- predict(modelo_rede_neural, x_test, type = "class")

# Calcular a acurácia no conjunto de teste
acuracia <- caret::confusionMatrix(predicoes, y_test)$overall["Accuracy"]

# Exibir a acurácia
print(paste("Acurácia do modelo no conjunto de teste: ", round(acuracia * 100, 2), "%"))
