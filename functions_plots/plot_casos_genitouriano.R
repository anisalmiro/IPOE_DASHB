
  AA<-raw_base_rename
  
dd <- bind_cols(
  AA %>% 
    dplyr::filter(!is.na(Infeccao_Urinaria) & Infeccao_Urinaria == "sim") %>% 
    dplyr::summarise(Infeccao_Urinaria = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Incontinencia_Urinaria) & Incontinencia_Urinaria == "sim") %>% 
    dplyr::summarise(Incontinencia_Urinaria = n()),

  AA %>% 
    dplyr::filter(!is.na(Calculos_Renais) & Calculos_Renais == "sim") %>% 
    dplyr::summarise(Calculos_Renais = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Insuficiencia_Renal_Aguda) & Insuficiencia_Renal_Aguda == "sim") %>% 
    dplyr::summarise(Insuficiencia_Renal_Aguda = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Doenca_Renal_Cronica) & Doenca_Renal_Cronica == "sim") %>% 
    dplyr::summarise(Doenca_Renal_Cronica = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Doenca_Inflamatoria_Pelvica_DIP) & Doenca_Inflamatoria_Pelvica_DIP == "sim") %>% 
    dplyr::summarise(Doenca_Inflamatoria_Pelvica_DIP = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Abcesso_tubo_ovarico) & Abcesso_tubo_ovarico == "sim") %>% 
    dplyr::summarise(Abcesso_tubo_ovarico = n())
)

  casos_Sistema_genitourinario<-data.frame(
    Sistema_Genitourinario=c("Infecção Urinária","Incontinência Urinária","Cálculos Renais","Insuficiência Renal Aguda","Doença Renal Crónica","Doença Inflamatória Pélvica (DIP)","Abcesso tubo-ovárico"),
    Frequencia=c(dd$Infeccao_Urinaria,dd$Incontinencia_Urinaria,dd$Calculos_Renais,dd$Insuficiencia_Renal_Aguda,dd$Doenca_Renal_Cronica,dd$Doenca_Inflamatoria_Pelvica_DIP,dd$Abcesso_tubo_ovarico)
  )

 # Criar o gráfico de colunas empilhadas
 Plot_casos_sist_Genitourinario <- hchart(
   casos_Sistema_genitourinario, 
   'bar', 
   hcaes(x = Sistema_Genitourinario, y = Frequencia, group = Sistema_Genitourinario),
   dataLabels = list(enabled = TRUE, style = list(fontSize = "11px")),
   stacking = "normal"
 ) %>%
   hc_colors(c( "#32A594","#EFC000FF", "#203d7d","#a0a0ed","#203d7e",'#00a1cd','#adad30')) %>%
   hc_yAxis(labels=list(
     style= list (
       fontSize= '16px',
       color= 'black'
     ))) %>% 
   hc_xAxis(labels=list(
     style= list (
       fontSize= '12px',
       color= 'black'
     ))) %>% 
   hc_tooltip(croddhairs = TRUE,
              shared = TRUE, borderWidth = 5) %>%
   hc_title(
     text = "<b><i></i>Sistema Neurológico</b>",
     margin = 20,
     align = "left",
     style = list(color = "#22A884", useHTML = TRUE)
   ) %>%
   hc_add_theme(hc_theme_google()) %>%
   hc_exporting(enabled = TRUE) 
 
 