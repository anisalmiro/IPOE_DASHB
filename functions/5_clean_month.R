cli::cli_alert_info("Criando variavel mes e definindo Nome do mes")

# Adicionar coluna com nome do mês
raw_base_rename <- raw_base_rename %>%
  mutate(
    mes = case_when(
      month(Data_reporte) == 1 ~ "1 - Janeiro",
      month(Data_reporte) == 2 ~ "2 - Fevereiro",
      month(Data_reporte) == 3 ~ "3 - Março",
      month(Data_reporte) == 4 ~ "4 - Abril",
      month(Data_reporte) == 5 ~ "5 - Maio",
      month(Data_reporte) == 6 ~ "6 - Junho",
      month(Data_reporte) == 7 ~ "7 - Julho",
      month(Data_reporte) == 8 ~ "8 - Agosto",
      month(Data_reporte) == 9 ~ "9 - Setembro",
      month(Data_reporte) == 10 ~ "10 - Outubro",
      month(Data_reporte) == 11 ~ "11 - Novembro",
      month(Data_reporte) == 12 ~ "12 - Dezembro"
    )
  )
