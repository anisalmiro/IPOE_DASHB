

cli::cli_alert_success("Criando faixas etarias para analise")
# Corrigir a categorização da idade em faixas etárias
raw_base_rename <- raw_base_rename %>%
  mutate(faixa_etaria = cut(idade,
                            breaks = c(0, 1, 4, 14, 29, 44, 64, 100),
                            labels = c("00 a 1ano", "2 a 4anos", "5 a 14anos", "15 a 29anos", "30 a 44anos", "45 a 64anos", "65 anos ou +"),
                            include.lowest = TRUE))  # Incluir o limite inferior nos intervalos

