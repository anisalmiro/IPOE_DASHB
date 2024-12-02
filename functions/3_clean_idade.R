cli::cli_alert_success("Corrigindo idades")

#incosistencia na base de dados nas idades calculadas no formulario 
#esta funcao garante:
#completar a idade em campos em que a idade nao foi calculada no formulario
#estas garante que todos campos nao prenchidos na idade sejam calculados baseando-se na data de reporte como base de calculo e a data de nascimento
raw_base_rename <- raw_base_rename %>%
  mutate(
    data_nascimento = parse_date_time(data_nascimento, orders = c("ymd", "dmy", "mdy")),
    Data_reporte = parse_date_time(Data_reporte, orders = c("ymd", "dmy", "mdy"))
  ) %>% 
 mutate(idade = ifelse(is.na(idade), interval(data_nascimento, Data_reporte) %/% years(1), idade))


#gerando variavel ano na base
cli::cli_alert_success("criando ano na base de reporte")
raw_base_rename <-raw_base_rename %>% dplyr::mutate(ano=year(Data_reporte))
# Converter a coluna idade para numérica
raw_base_rename$idade <- as.numeric(raw_base_rename$idade)




dim(r)
