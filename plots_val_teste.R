
# plots  ------------------------------------------------------------------


# treinamento e validação  ------------------------------------------------

library(ggplot2)
library(tidyverse)

# Random Forest - resultados do conjunto de treinamento e validação
rf_preds <- best_model$pred %>%
  mutate(model = "Random Forest", phase = "validação")

# XGBoost - resultados do conjunto de treinamento e validação
xgb_preds <- xgb_model_random$pred %>%
  mutate(model = "XGBoost", phase = "validação")

# Combinar predições de ambos os modelos
combined_preds <- bind_rows(rf_preds, xgb_preds)

# Calcular acurácia por fold (validação cruzada)
accuracy_results <- combined_preds %>%
  group_by(model, phase, Resample) %>%
  summarise(accuracy = mean(pred == obs))

# Gráfico para Treinamento e Validação
ggplot(accuracy_results, aes(x = Resample, y = accuracy, color = model, group = model)) +
  geom_line() +
  geom_point() +
  labs(title = "Curvas de Acurácia - Treinamento e Validação",
       x = "Fold de Validação",
       y = "Acurácia",
       color = "Modelo") +
  theme_minimal()



# teste -------------------------------------------------------------------

# Calcular a acurácia para o conjunto de teste (para XGBoost)
accuracy_xgb_teste <- MLmetrics::Accuracy(previsoes_xgb_model_random, dados_teste$status_doenca)

# Adicionar essa informação a um dataframe
test_results <- data.frame(
  model = c("XGBoost"),
  phase = c("teste"),
  accuracy = c(accuracy_xgb_teste)
)

# Você pode repetir este passo para o modelo Random Forest, caso tenha gerado predições de teste para ele.

# Plotar o resultado de teste para o XGBoost
ggplot(test_results, aes(x = phase, y = accuracy, fill = model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Acurácia no Conjunto de Teste",
       x = "Conjunto de Teste",
       y = "Acurácia") +
  theme_minimal()

