
cli::cli_alert_success("criando bases auxiliares para dashboard")

#base para carregamento no Dahboad com exclusao de algumas variaveis
r_Base <-raw_base_rename %>% select(Data_reporte,7:344)


#pegando ultima data para base diaria
ultima_data <- max(as.Date(raw_base_rename$Data_reporte))

#base preliminar para analise
Prel_diaria <-raw_base_rename %>% dplyr::filter(raw_base_rename$Data_reporte == ultima_data)
#base diaria
BD_diaria <-Prel_diaria %>% select(Data_reporte,7:344)

#selecionando dados semanais
test_7dias<-raw_base_rename$Data_reporte - ddays(7)
#gerando dados semanais
prel_semanal<- raw_base_rename %>% dplyr::filter(Data_reporte >= test_7dias) 
BD_semanal <-prel_semanal %>% select(Data_reporte,7:344)


