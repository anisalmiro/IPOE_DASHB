cli::cli_alert_info("Corrigindo provincias, estado do paciente, locais de atendimento")

#Corregindo provincias
#Correcao para maiusculo codigo da vigilancia
raw_base_rename <- raw_base_rename %>%
  mutate(provincia = recode(provincia,
                            "Maputo_Cidade" = "Maputo Cidade",
                            "Cabo_Delgado" = "Cabo Delgado",
                            "Maputo_Provincia" = "Maputo Provincia",
                            .default = provincia)) %>% # CORREGINDO PROVINCIA
  
  mutate(provincia_resid = recode(provincia_resid,#corregindo provincia de residencia
                            "Maputo_Cidade" = "Maputo Cidade",
                            "Cabo_Delgado" = "Cabo Delgado",
                            "Maputo_Provincia" = "Maputo Provincia",
                            .default = provincia_resid)) %>%
  
  mutate(estado_paciente = recode(estado_paciente,#corregindo estado do paciente
                                  "ligeiro" = "Ligeiro",
                                  "grave" = "Grave")) %>% # CORREGINDO ESTADO DO PACIENTE
  
  mutate(desfecho = recode(desfecho,#corregindo variavel desfecho de caso
                                  "continua_hospitalizado" = "Continua Hospitalizado",
                                  "nao sabe" = "Não sabe",
                                  "alta_melhoria" = "Alta/melhorou",
                                  "obito" = "Óbito",
                                  "transferido_hospital" = "Transferido para outro Hospital",
                                  "abandono" = "Abandono",
                                  .default = desfecho)) %>%
  
  mutate(local_atendimento = recode(local_atendimento,
                                    "cuidados_intermediarios" = "Cuidados intermediários")) %>%  #correcao de Local de atendimento
  mutate(nome_local_atendimento = recode(nome_local_atendimento,
                                         "triagem" = "Triagem",
                                         "so_adulto"="So Adulto",
                                         "so_pediatrico"="So Pediátrico",
                                         "cuidados_intermediarios_adultos"="Cuidados intermediários adultos",
                                         "cuidados_intermediarios_pediatrico"="Cuidados intermediários Pediátricos",
                                         "medicina"="Medicina",
                                         "pediatria"="Pediatria",
                                         "bercario"="Bercario",
                                         "cirurgia"="Cirurgia",
                                         "maternidade"="Maternidade",
                                         "nao_aplicavel"="Nao Aplicavel"
  ))



cli::cli_alert_info("Corrigindo Caracteres iniciais dos atributos (Provincia,nome do local de atendiamento)")
#Trocar para maiusculo a primeira letra do local de atendimento,nome_local de atendimento
#trocar tudo para maiusculo vigilancia
#corregir local Hospitalizacao
raw_base_rename <- raw_base_rename %>%
  mutate(local_atendimento = str_to_title(local_atendimento)) %>%   #Local ATENDIMENTO
  mutate(nome_local_atendimento=str_to_title(nome_local_atendimento)) %>%  #CORREGINDO NOME DO Local ATENDIMENTO
  mutate(local_hospitalizacao=str_to_title(local_hospitalizacao)) %>%  #CORREGINDO Local HSPITALIZACAO
  mutate(nome_local_hospitalizacao=str_to_title(nome_local_hospitalizacao)) %>%  #COREGINDO NOME DO Local HSPITALIZACAO
  mutate(vigilancia = toupper(vigilancia)) %>%  #CONVERTENDO EM MAIUSCULO VIGILANCIA
  mutate(Sem_Epi=date2week(Data_reporte, floor_day = TRUE)) #criando variavel para semana Epidemiologica

