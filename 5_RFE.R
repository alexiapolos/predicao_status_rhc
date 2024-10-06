
# 1. Pacotes --------------------------------------------------------------
library(caret)
library(tidyverse)
library(xgboost)
library(caret)
library(ranger)
library(randomForest)
library(doParallel)

# RFE Random Forest -------------------------------------------------------
cl <- makeCluster(detectCores() - 1) 

registerDoParallel(cl)

clusterEvalQ(cl, {
  library(randomForest)  # Carregar randomForest em cada nó do cluster
  library(caret)
  # library(dplyr)
})

# Definir as funções de RFE usando Random Forest
custom_rfFuncs <- rfFuncs

# Ajustar a função de fit para o Random Forest com mtry correto
custom_rfFuncs$fit <- function(x, y, first, last, ...) {
  # Ajustar mtry dinamicamente com base no número de variáveis explicativas
  mtry_value <- min(10, ncol(x))  # mtry não pode ser maior que o número de variáveis em x
  
  randomForest(
    x = x, 
    y = y,
    mtry = mtry_value,            # Usar mtry ajustado
    nodesize = 1,                 
    ntree = 500,                  
    importance = TRUE,            
    replace = TRUE,               
    do.trace = FALSE,             
    ...
  )
}

# Definir controlador de RFE com validação cruzada repetida
set.seed(462)
ctrl_rfe <- rfeControl(
  functions = custom_rfFuncs,         
  method = "repeatedcv",             
  repeats = 5,                        
  number = 5,                        
  allowParallel = TRUE,              
  verbose = TRUE                     
)

# Definir as variáveis a serem utilizadas e executar o RFE
rfe_result <- rfe(
  x = dados_treinamento %>% select(-status_doenca),  # Dados de entrada (todas as colunas, exceto a resposta)
  y = dados_treinamento$status_doenca,              # Variável de resposta
  sizes = c(3, 5, 7, 10),                            # Número de variáveis a serem selecionadas
  rfeControl = ctrl_rfe,                             # Controlador do RFE
  na.action = na.omit                                # Remover NA durante o processo
)



variaveis_selecionadas <- predictors(rfe_result)

plot(rfe_result, type = "hist")

dados_treino_rfe <- dados_treinamento %>%
  dplyr::select(all_of(variaveis_selecionadas), status_doenca)

ctrl <- trainControl(
  method = "cv",                     
  number = 10,                        
  classProbs = TRUE,                 
  summaryFunction = multiClassSummary, 
  sampling = "up",                    
  returnData = TRUE,
  savePredictions = TRUE,
  allowParallel = TRUE, 
  verboseIter = TRUE                 
)

tuneGrid <- expand.grid(
  mtry = 10,                          
  splitrule = "extratrees",           
  min.node.size = 1                  
)

set.seed(462)
rf_model_rfe <- train(
  status_doenca ~ .,
  data = dados_treino_rfe,             
  method = "ranger",                   
  trControl = ctrl,                    
  tuneGrid = tuneGrid,                
  num.trees = 500,                     
  na.action = na.omit,
  importance = "impurity"              
)

print(rf_model_rfe$results)

save.image("RFE.Rdata")

# Finalizar e desregistrar o cluster paralelo
stopCluster(cl)
registerDoSEQ()

rm(cl)
rm(list = ls(pattern = "^cl"))
gc()


# 1. Pacotes --------------------------------------------------------------
library(caret)
library(tidyverse)
library(xgboost)
library(doParallel)
library(tictoc)

# 2. Configuração do cluster ----------------------------------------------
cl <- makeCluster(detectCores() - 1) 
registerDoParallel(cl)

# Certificar-se de que todos os pacotes necessários estão carregados nos nós do cluster
clusterEvalQ(cl, {
  library(xgboost)  # Carregar xgboost em cada nó do cluster
  library(caret)
  library(tidyverse)
})

# Exportar o conjunto de dados para os clusters
clusterExport(cl, varlist = c("dados_treinamento"))

# 3. Definir funções personalizadas de RFE para xgboost --------------------
# Funções de ajuste personalizadas para xgboost
custom_xgbFuncs <- caretFuncs

# Definir a função de ajuste específica para o xgboost com parâmetros fixos
custom_xgbFuncs$fit <- function(x, y, first, last, ...) {
  # Converter para matriz numérica e DMatrix
  x_matrix <- model.matrix(~ ., data = as.data.frame(x))  # Converter para matriz numérica
  y_numeric <- as.numeric(as.factor(y)) - 1  # Converter o vetor de resposta para numérico
  
  # Treinamento do modelo xgboost
  xgb.train(
    data = xgb.DMatrix(data = x_matrix, label = y_numeric),  # Criar DMatrix a partir do x_matrix
    nrounds = 100,                                # Número de iterações (melhor valor encontrado)
    max_depth = 4,                                # Profundidade máxima das árvores
    eta = 0.08949752,                             # Taxa de aprendizado (melhor valor encontrado)
    gamma = 0.3054291,                            # Mínimo ganho para dividir o nó
    colsample_bytree = 0.7523018,                 # Proporção de colunas amostradas
    min_child_weight = 4,                         # Peso mínimo das instâncias no nó filho
    subsample = 0.5186806,                        # Proporção das amostras usadas para cada árvore
    objective = "binary:logistic",                # Função de perda para classificação binária
    verbosity = 0,                                # Modo silencioso
    ...
  )
}

# 4. Definir controlador de RFE -------------------------------------------
set.seed(462)
ctrl_rfe <- rfeControl(
  functions = custom_xgbFuncs,  # Utilizar as funções personalizadas para xgboost
  method = "repeatedcv",        # Validação cruzada repetida
  repeats = 5,                  # Número de repetições
  number = 5,                   # Número de folds na validação cruzada
  allowParallel = TRUE,         # Permitir execução paralela
  verbose = TRUE                # Mostrar progresso
)

# 5. Executar o RFE -------------------------------------------------------
# Converter variáveis para numérico e remover variáveis não numéricas do conjunto de dados
x_data <- dados_treinamento %>% select(-status_doenca) %>% 
  dplyr::select_if(is.numeric)  # Selecionar apenas colunas numéricas

# Definir as variáveis a serem utilizadas e executar o RFE
rfe_result <- rfe(
  x = x_data,                                      # Dados de entrada (todas as colunas numéricas, exceto a resposta)
  y = as.factor(dados_treinamento$status_doenca),  # Variável de resposta como fator
  sizes = c(3, 5, 7, 10),                          # Número de variáveis a serem selecionadas
  rfeControl = ctrl_rfe,                           # Controlador do RFE
  na.action = na.omit                              # Remover NA durante o processo
)

# 6. Visualizar resultados ------------------------------------------------
# Variáveis selecionadas
variaveis_selecionadas <- predictors(rfe_result)

# Visualizar o resultado do RFE
plot(rfe_result, type = "hist")

# 7. Treinar modelo final com as variáveis selecionadas -------------------
# Filtrar dados com as variáveis selecionadas
dados_treino_rfe <- dados_treinamento %>%
  dplyr::select(all_of(variaveis_selecionadas), status_doenca)

# Definir controlador para o modelo final
ctrl <- trainControl(
  method = "cv",                     # Validação cruzada
  number = 5,                        # Número de folds
  classProbs = TRUE,                 # Calcular probabilidades de classe
  summaryFunction = multiClassSummary, 
  sampling = "up",                   # Reamostragem para balanceamento
  returnData = TRUE,
  savePredictions = "final",
  allowParallel = TRUE, 
  verboseIter = TRUE                 
)

# Treinar modelo XGBoost final com as variáveis selecionadas
set.seed(462)
xgb_final_model <- train(
  status_doenca ~ .,                 # Fórmula do modelo
  data = dados_treino_rfe,           # Dados de treinamento com as variáveis selecionadas
  method = "xgbTree",                # Algoritmo a ser utilizado
  trControl = ctrl,                  # Controlador do treinamento
  tuneGrid = data.frame(
    nrounds = 100,                   # Parâmetros ótimos para o XGBoost
    max_depth = 4,
    eta = 0.08949752,
    gamma = 0.3054291,
    colsample_bytree = 0.7523018,
    min_child_weight = 4,
    subsample = 0.5186806
  ),
  metric = "Accuracy",               # Métrica a ser otimizada
  na.action = na.omit
)

# Visualizar resultados do modelo final
print(xgb_final_model$results)

# 8. Finalizar e desregistrar o cluster paralelo -------------------------
stopCluster(cl)
registerDoSEQ()

# Limpar variáveis relacionadas ao cluster e liberar memória
rm(cl)
rm(list = ls(pattern = "^cl"))
gc()

# Salvar o ambiente de trabalho
save.image("RFE_XGBoost.Rdata")