# 1. Pacotes --------------------------------------------------------------
library(foreign)
library(tidyverse)
library(lubridate)
library(gtsummary)
library(flextable)
library(factoextra)
library(finalfit)
library(stringr)
library(readxl)
library(officer)
library(dplyr)
library(forcats)
library(stringr)

dados <- foreign::read.dbf("rhc17.dbf") %>%
  dplyr::mutate(ID = row_number())

# Conversão e cálculo das datas  ---------------------------------------------
dados <- dados %>%
  dplyr::mutate(
    across(.cols = c(DATAINITRT, DTDIAGNO), .fns = dmy),
    dtdiag_dtinitrat = interval(DTDIAGNO, DATAINITRT) / days(1),
    lei60_respeitou = if_else(dtdiag_dtinitrat <= 60, "Sim", "Não")
  )

# CNES --------------------------------------------------------------------
CNES <- read_excel("hospitais-habilitados-em-oncologia-alta-complexidade.xlsx") %>%
  setNames(.[2, ]) %>%
  dplyr::filter(!is.na(`TOTAL DE HABILITAÇÕES`), `TOTAL DE HABILITAÇÕES` == "1") %>%
  dplyr::select(CNES, HABILITAÇÃO) %>%
  dplyr::mutate(
    CNES = str_pad(
      CNES,
      width = 7,
      side = "left",
      pad = "0"
    ),
    HABILITAÇÃO = str_to_upper(HABILITAÇÃO),
    cacon_unacon = case_when(
      str_detect(HABILITAÇÃO, "UNACON") ~ "UNACON",
      str_detect(HABILITAÇÃO, "CACON") ~ "CACON",
      TRUE ~ "Outra"
    )
  )

dados <- left_join(dados, CNES, by = "CNES")
# CID-O -------------------------------------------------------------------
CID_O_topo <- read_excel("CID_O.xlsx", sheet = 1) %>%
  dplyr::select(CID_O = "Código_CID-OEd3", topografia = "Descrição_CID-OEd3")

dados <- left_join(dados, CID_O_topo, by = c("LOCTUDET" = "CID_O")) %>%
  rename(topografia_loc_primaria_3d = topografia)

CID_O_morfo = read_excel("CID_O.xlsx", sheet = 2) %>%
  dplyr::select("Código_CID-OEd3", "Descrição_CID-OEd3") %>%
  dplyr::rename(codigo_morfo = "Código_CID-OEd3", 
                morfologia = "Descrição_CID-OEd3")

CID_O_morfo$codigo_morfo =  as.character(CID_O_morfo$codigo_morfo)

dados =  dados %>%
  dplyr::mutate(TIPOHIST = gsub("\\/", "", TIPOHIST))

dados = left_join(dados, CID_O_morfo, by = c("TIPOHIST" = "codigo_morfo"))

# Tratar registros inconsistentes ------------------------------------------
dados <- dados %>%
  # Filtrar apenas os casos desejados: Pele com melanoma e todos os outros que não são "Pele"
  dplyr::filter(
    (topografia_loc_primaria_3d == "Pele" & str_detect(morfologia, regex("melanoma", ignore_case = TRUE))) |
      topografia_loc_primaria_3d != "Pele"
  ) %>%
  
  # Aplicar transformações e recodificações
  dplyr::mutate(
    # Ajustar e recodificar variável `tratamento_inicial` a partir dos primeiros caracteres de `PRITRATH`
    tratamento_inicial = as.factor(substr(as.character(PRITRATH), 1, 1)),
    
    # Corrigir valores inválidos na variável `idade` e converter para numérico
    idade = dplyr::case_when(
      IDADE %in% c("999", "888", "159", "142", "140", "139", "138", "136", "134", 
                   "133", "131", "128", "127", "125", "124", "122", "121", "120", 
                   "118", "117", "115", "114", "113", "112", "111", "109", "108", 
                   "107", "0-1", "-51", "-52", "-55", "-57", "-59", "-67", "135", 
                   "119", "116", "110", "106", "105", "104", "103", "102", "101") ~ NA_real_,
      TRUE ~ as.numeric(IDADE)
    ),
    
    # Recodificar estadiamento usando `case_when`
    estadiamento = dplyr::case_when(
      ESTADIAM %in% c("0", "00", "0A") ~ "0",
      ESTADIAM %in% c("01", "1", "10", "11", "12", "14", "1A", "1B", "1C") ~ "I",
      ESTADIAM %in% c("02", "2", "20", "21", "23", "28", "24", "2A", "2B", "2C", "2D") ~ "II",
      ESTADIAM %in% c("30", "33", "39", "03", "34", "3", "3A", "3B", "3C", "3D") ~ "III",
      ESTADIAM %in% c("04", "4", "40", "41", "43", "44", "4A", "4B", "4C", "4D", "49") ~ "IV",
      TRUE ~ NA_character_  # Definir como NA para todos os valores não especificados
    ),
    
    # Recodificar tipo de caso
    tipo_caso = dplyr::case_when(
      TPCASO == "1" ~ "Analítico",
      TPCASO == "2" ~ "Não analítico",
      TRUE ~ NA_character_  # Definir como NA para todos os valores não especificados
    ),
    
    # Recodificação de variáveis categóricas
    sexo = dplyr::recode(SEXO, `1` = "Masc", `2` = "Fem", `3` = NA_character_, `0` = NA_character_),
    estado_residencia = dplyr::recode(ESTADRES, `77` = NA_character_, `99` = NA_character_),
    
    # Agrupamento por região
    regiao = dplyr::case_when(
      estado_residencia %in% c("SP", "RJ", "MG", "ES") ~ "Sudeste",
      estado_residencia %in% c("RS", "SC", "PR") ~ "Sul",
      estado_residencia %in% c("BA", "SE", "AL", "PE", "PB", "RN", "CE", "PI", "MA") ~ "Nordeste",
      estado_residencia %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      estado_residencia %in% c("AM", "RR", "AP", "PA", "TO", "RO", "AC") ~ "Norte",
      TRUE ~ NA_character_
    ),
    
    # Agrupamento dos tumores
    tumores_agrupado = dplyr::case_when(
      topografia_loc_primaria_3d %in% c(
        "Esôfago", "Estomago", "Cólon", "Reto", "Pâncreas", "Fígado e vias biliares intrahepáticas",
        "Vesícula biliar", "Intestino delgado", "Retroperitônio e peritônio", "Outros órgãos digestivos",
        "Vias biliares, outras partes e partes não especificadas", "Junção retossigmoidiana", "Anus e canal anal"
      ) ~ "Sistema Digestivo",
      topografia_loc_primaria_3d %in% c(
        "Brônquios e pulmões", "Laringe", "Traquéia", "Orofaringe", "Língua", "Palato", "Hipofaringe", 
        "Amígdala", "Nasofaringe", "Cavidade nasal e ouvido médio", "Gengiva", "Seios da face", 
        "Glândula tiróide", "Glândula parótida", "Glândula supra-renal"
      ) ~ "Sistema Respiratório",
      topografia_loc_primaria_3d %in% c(
        "Próstata", "Bexiga", "Rim", "Pênis", "Testículo", "Ureter", "Pelve renal"
      ) ~ "Sistema Urológico",
      topografia_loc_primaria_3d %in% c(
        "Colo do útero", "Ovário", "Corpo do útero", "Vagina", "Vulva", "Mama"
      ) ~ "Mama e Ginecológicos",
      topografia_loc_primaria_3d %in% c("Sistema hematopoiético e reticuloendotelial", "Linfonodos") ~ "Sistema hematopoético",
      topografia_loc_primaria_3d %in% c(
        "Pele", "Encéfalo", "Medula espinhal", "Meninges", "Nervos periféricos e SNC"
      ) ~ "Melanomas, Sarcomas e SNC",
      topografia_loc_primaria_3d %in% c(
        "Localização primária desconhecida", "Outras localizações mal definidas"
      ) ~ "Outros", 
      TRUE ~ NA_character_
    ),
    
    # Recodificar tratamento inicial
    tratamento_inicial = dplyr::case_when(
      tratamento_inicial == "1" ~ "Nenhum",
      tratamento_inicial == "2" ~ "Cirurgia",
      tratamento_inicial == "3" ~ "Radioterapia",
      tratamento_inicial == "4" ~ "Quimioterapia",
      tratamento_inicial %in% c("5", "6", "7", "8") ~ "Outro",  # Agrupar valores 5, 6 e 8 como "Outro"
      TRUE ~ NA_character_  # Definir como NA para todos os valores não especificados
    ),
    
    # Categorização da faixa etária
    faixa_etaria = dplyr::case_when(
      idade >= 31 & idade <= 40 ~ "31-40 anos",
      idade >= 41 & idade <= 50 ~ "41-50 anos",
      idade >= 51 & idade <= 60 ~ "51-60 anos",
      idade >= 61 & idade <= 70 ~ "61-70 anos",
      idade > 70 ~ "Mais de 70 anos",
      TRUE ~ NA_character_
    ),
    
    # Recodificar status da doença
    status_doenca = dplyr::case_when(
      ESTDFIMT %in% c("1", "2", "3", "4", "5") ~ "Vivo",
      ESTDFIMT == "6" ~ "Óbito",
      TRUE ~ NA_character_  # Definir como NA para todos os valores não especificados
    ),
    
    # Conversão para fator das variáveis recategorizadas
    regiao = as.factor(regiao),
    faixa_etaria = as.factor(faixa_etaria),
    estado_residencia = as.factor(estado_residencia),
    tumores_agrupado = as.factor(tumores_agrupado)
  ) %>%
  
  # Filtro de idade para manter apenas maiores de 30 anos
  dplyr::filter(idade > 30) %>%
  
  # Selecionar colunas de interesse
  dplyr::select(
    faixa_etaria, sexo, tipo_caso, estado_residencia, lei60_respeitou, tratamento_inicial, 
    cacon_unacon, status_doenca, estadiamento, regiao, tumores_agrupado
  ) %>%
  
  # Converter variáveis de texto restantes para fator
  dplyr::mutate(across(where(is.character), as.factor))

# Exploratório do banco de dados ------------------------------------------
dados_explanatory =
  dados %>%
  ff_glimpse()

estrutura_banco = dados_explanatory$Categorical

# Drop de NAs -------------------------------------------------------------
dados_filtrados_sem_na =
  dados %>%
  drop_na()

# Frequências -------------------------------------------------------------
frequencias =
  dados_filtrados_sem_na %>%
  tbl_summary()

frequencias_df <- as.data.frame(frequencias$table_body)

frequencias_ft <- regulartable(frequencias_df) %>%
  theme_vanilla() %>%  # Aplicar um tema padrão
  autofit() %>%        # Ajustar a largura das colunas automaticamente
  set_caption("Tabela de Frequências")  # Título da tabela

doc <- read_docx() %>%
  body_add_flextable(frequencias_ft) %>%
  body_add_par("Tabela gerada automaticamente a partir do gtsummary.", style = "Normal")

print(doc, target = "frequencias.docx")

# Salvar ambiente ---------------------------------------------------------
save.image("dados_tratados_05102024.Rdata")
