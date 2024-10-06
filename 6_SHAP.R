
# 1. pacotes --------------------------------------------------------------
library(DALEX)
library(tidyverse)
library(ggplot2)
library(openxlsx)
library(flextable)
library(gtsummary)
library(officer)


# 2. Modelo explicativo SHAP ----------------------------------------------
explainer_rf <- DALEX::explain(
  model = rf_model_rfe,
  data = dados_treinamento[, -which(names(dados_treinamento) == "status_doenca")],
  y = as.factor(dados_treinamento$status_doenca)
)

save.image("rf_rfe.Rdata")
# 3. Extrair SHAP values de variaveis + categorias ------------------------
shap_values_rf <- predict_parts(explainer_rf, new_observation = dados_teste, type = "shap")

write.csv(shap_values_rf, "shap_cat_rf_rfe.csv")

png(
  filename = 'shap_values_rf_plot_rfe.png',
  width = 1920,
  height = 1080,
  res = 300
)
plot(shap_values_rf,
     main = "SHAP Values - RF RFE",
     col = "#4e6d9a",
     lwd = 2)
dev.off()


# 4. Gerar tabela de importância de variáveis + categorias  ----------------
shap_df <- shap_values_rf %>%
  as.data.frame() %>%
  select(variable, contribution) %>%
  group_by(variable) %>%
  summarise(Mean_Contribution = mean(contribution)) %>%
  arrange(desc(Mean_Contribution))

shap_table <- flextable(shap_df) %>%
  theme_vanilla() %>%  
  autofit() %>%
  set_caption("Tabela de Importância das Variáveis - SHAP")

doc <- read_docx() %>%
  body_add_flextable(shap_table) %>%
  body_add_par("Tabela gerada automaticamente com valores SHAP.", style = "Normal")

print(doc, target = "shapley_importance.docx")


# 5. Plotar contribuição média das variáveis ------------------------------
shap_plot <- ggplot(shap_df, aes(x = reorder(variable, Mean_Contribution), y = Mean_Contribution)) +
  geom_bar(stat = "identity",
           fill = "#4e6d9a",
           width = 0.7) +
  geom_text(
    aes(label = round(Mean_Contribution, 2)),
    hjust = -0.2,
    size = 5,
    color = "black",
    fontface = "bold"
  ) +  
  coord_flip() +  #
  labs(title = "Valores SHAP - Random Forest - RFE", x = "Variáveis + categorias", y = "Valor SHAP") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    axis.text.x = element_text(size = 14, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 16, face = "bold"),
    #
    axis.title.y = element_text(size = 16, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray", linetype = "dotted"),
    panel.border = element_blank()
  )


ggsave(
  filename = "impacto_variaveis_shap_rf_rfe.png",
  plot = shap_plot,
  width = 10,
  height = 7,
  dpi = 300
)

# 5. Gerar contribuição e plot agrupado por variavel --------------------------
aggregate_shap <- function(shap_values) {
  shap_values$variable_main <- sapply(strsplit(as.character(shap_values$variable), " = "), `[`, 1)
  
  shap_aggregated <- shap_values %>%
    group_by(variable_main) %>%
    summarise(contribution = sum(contribution),
              feature_value = mean(abs(contribution))) %>%
    arrange(desc(abs(contribution)))
  
  return(shap_aggregated)
}

shap_aggregated <- aggregate_shap(shap_values_rf)

write.xlsx (shap_aggregated, "rf_shapley_var.csv", rownames = TRUE)

plot <- ggplot(shap_aggregated,
               aes(
                 x = contribution,
                 y = reorder(variable_main, contribution),
                 color = feature_value
               )) +
  geom_point(alpha = 0.6, size = 3) +
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "black") +
  labs(
    title = "Random Forest - RFE",
    x = "Valor SHAP",
    y = "Variável Principal",
    color = "Média do Valor da Contribuição"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(vjust = 0.5))

ggsave(
  filename = "impacto_variaveis_modelo_rf_rfe.png",
  plot = plot,
  width = 10,
  height = 7,
  dpi = 300
)