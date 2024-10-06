
# 1. Pacotes ----------------------------------------------------------------
library(caret)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(flextable)
library(officer)

# 2. Formatar dados ----------------------------------------------------------
dados_teste =
  dados_teste %>%
  dplyr::mutate(ID = row_number())

dados_teste_sem_status =
  dados_teste %>%
  dplyr::filter(complete.cases(.)) %>%
  dplyr::select(-status_doenca)

# 3. Desempenho do modelo ---------------------------------------------------

png(
  filename = "rf_model_rfe_desempenho.png",
  width = 4000,
  height = 3000,
  res = 300
)

plot(rf_model_rfe)

dev.off()

# 4. Previsões --------------------------------------------------------------
previsoes_rf_model <- predict(rf_model_rfe, newdata = dados_teste_sem_status)

previsoes_rf_model <- factor(previsoes_rf_model, levels = levels(dados_teste$status_doenca))

dados_preditos_rf_model <- dados_teste_sem_status %>%
  dplyr::mutate(predicoes = previsoes_rf_model)

write.xlsx(dados_preditos_rf_model, "dados_preditos_rf_rfe.xlsx")

# 5. Matriz de confusão ---------------------------------------------------
matriz_confusao_previsoes_rf_model <- confusionMatrix(previsoes_rf_model, dados_teste$status_doenca)

accuracy <- matriz_confusao_previsoes_rf_model$overall['Accuracy']

metricas <- matriz_confusao_previsoes_rf_model$byClass %>%
  as.data.frame() %>%
  mutate(Acuracia_Geral = accuracy)

write.xlsx(metricas,
           file = "metricas_rf_rfe.xlsx",
           rowNames = TRUE,
           colNames = TRUE)

matriz_confusao_tabela <- matriz_confusao_previsoes_rf_model$table

matriz_confusao_df <- as.data.frame(matriz_confusao_tabela)

heatmap <- ggplot(matriz_confusao_df, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(alpha = 0.8) +
  scale_fill_gradient(low = "#0e2044", high = "#adcbfc") +
  geom_text(aes(label = Freq), color = "black", size = 6) +  #
  labs(title = "Random Forest - RFE",
       x = "Classe Referência",
       y = "Classe Predita",
       fill = "N") +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(size = 16, face = "bold"),
    axis.text.y = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    panel.grid = element_blank()
  )

ggsave(
  filename = "mc_rf_rfe.png",
  plot = heatmap,
  width = 10,
  height = 8,
  dpi = 600
)

# 6. Importancia das variaveis ---------------------------------------------
importancia <- varImp(rf_model_rfe, scale = TRUE)

importancia_df <- importancia$importance %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "Variável") %>%
  arrange(desc(Overall))

tabela_importancia <- regulartable(importancia_df)

tabela_importancia <- tabela_importancia %>%
  theme_vanilla() %>%
  set_caption("Tabela de Importância das Variáveis - rf rfe") %>%
  autofit()

doc <- read_docx() %>%
  body_add_flextable(tabela_importancia) %>%
  body_add_par("Tabela", style = "Normal")

print(doc, target = "importancia_variaveis.docx")

importancia_filtrada_df <- importancia_df %>%
  filter(Overall > 10)

importancia_plot <- ggplot(importancia_filtrada_df, aes(x = reorder(Variável, Overall), y = Overall)) +
  geom_bar(stat = "identity",
           fill = "#4e6d9a",
           width = 0.7) +
  geom_text(
    aes(label = round(Overall, 2)),
    hjust = -0.2,
    size = 5,
    color = "black",
    fontface = "bold"
  ) +
  coord_flip() +
  labs(title = "Random Forest - RFE", x = "Variáveis", y = "Importância") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    axis.title.y = element_text(size = 16, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray", linetype = "dotted"),
    panel.border = element_blank()
  )

ggsave(
  filename = "importancia_variaveis_rfe.png",
  plot = importancia_plot,
  width = 10,
  height = 8,
  dpi = 600
)

# Curva Roc ---------------------------------------------------------------
probabilidades_rf <- predict(rf_model_rfe, newdata = dados_teste, type = "prob")
roc_rf <- roc(response = dados_teste$status_doenca,
              predictor = probabilidades_rf[, "Óbito"],  
              levels = rev(levels(dados_teste$status_doenca)))

# 4.3 Plotar a curva ROC com ggplot2 e exibir AUC (plotar com todos os modelos)
roc_plot <- ggplot() +
  geom_line(data = data.frame(roc_rf$sensitivities, 1 - roc_rf$specificities), 
            aes(x = 1 - roc_rf$specificities, y = roc_rf$sensitivities), 
            color = "#0e2044", size = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  labs(title = sprintf("Curva ROC - AUC: %.2f", auc(roc_rf)), 
       x = "1 - Especificidade", 
       y = "Sensibilidade") +
  theme_minimal(base_size = 16) +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        axis.text.x = element_text(size = 14, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"),
        axis.title.x = element_text(size = 16, face = "bold"),
        axis.title.y = element_text(size = 16, face = "bold"))

# 4.4 Salvar o gráfico da curva ROC
ggsave(
  filename = "curva_ROC.png",
  plot = roc_plot,
  width = 10,
  height = 8,
  dpi = 600
)

