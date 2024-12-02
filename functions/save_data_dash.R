
cli::cli_alert_success("Exportando Bases de Dados para DashBoard")


cli::cli_alert_success("Exportando Bases preliminar")
save(raw_base_rename, file = paste0("data/DB_Dashboard/raw_base_rename.rda"))

#Exportando Bases de dados para DashBoard
cli::cli_alert_success("Exportando Bases Geral")
save(r_Base, file = paste0("data/DB_Dashboard/Base_Geral.rda"))


cli::cli_alert_success("Exportando semanal")
save(BD_semanal, file = paste0("data/DB_Dashboard/Base_semanal.rda"))


cli::cli_alert_success("Exportando Bases diaria")
save(BD_diaria, file = paste0("data/DB_Dashboard/Base_diaria.rda"))
