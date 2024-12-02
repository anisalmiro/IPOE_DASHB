cli::cli_alert_info("Carregamento de Bases de dados")
infile <- file.path("Formulario_de_Vigilancia_Integrada.csv")
r <- rio::import(infile, encoding = "UTF-8")

#correcao de variaveis da base
raw_base_rename <- r %>%
  dplyr::rename (
    vg_fa = "vigilancia/fa",
    vg_vinadia = "vigilancia/vinadia",
    vg_iras="vigilancia/iras",
    vg_virahe="vigilancia/virahe",
    
  ) %>% rename_with(.fn = ~ tolower(gsub("vigilancia/", "", .x, fixed = TRUE)), .col = starts_with("vigilancia/")) %>%
        mutate(Data_reporte = as.Date(start, format = "%Y-%m-%d"))
  
