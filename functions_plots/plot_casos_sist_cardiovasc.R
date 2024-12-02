
  AA<-raw_base_rename
  
kk <- bind_cols(
  AA %>% 
    dplyr::filter(!is.na(Acidente_Vascular_Cerebral) & Acidente_Vascular_Cerebral == "sim") %>% 
    dplyr::summarise(Acidente_Vascular_Cerebral = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Febre_reumatica) & Febre_reumatica == "sim") %>% 
    dplyr::summarise(Febre_reumatica = n()),

  AA %>% 
    dplyr::filter(!is.na(Insuficiencia_cardiaca_congestiva) & Insuficiencia_cardiaca_congestiva == "sim") %>% 
    dplyr::summarise(Insuficiencia_cardiaca_congestiva = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Endocardite) & Endocardite == "sim") %>% 
    dplyr::summarise(Endocardite = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Pericardite) & Pericardite == "sim") %>% 
    dplyr::summarise(Pericardite = n())
)

  casos_cardiovascular<-data.frame(
    Sistema_Cardiovascular=c("Acidente Vascular Cerebral","Febre reumática","Insuficiência cardíaca congestiva","Endocardite","Pericardite"),
    Frequencia=c(kk$Acidente_Vascular_Cerebral,kk$Febre_reumatica,kk$Insuficiencia_cardiaca_congestiva,kk$Endocardite,kk$Pericardite)
  )

 # Criar o gráfico de colunas empilhadas
 Plot_casos_sist_cardiovascular <- hchart(
   casos_cardiovascular, 
   'bar', 
   hcaes(x = Sistema_Cardiovascular, y = Frequencia, group = Sistema_Cardiovascular),
   dataLabels = list(enabled = TRUE, style = list(fontSize = "11px")),
   stacking = "normal"
 ) %>%
   hc_colors(c( "#32A594","#EFC000FF", "#203d7d","#a0a0ed","#203d7e")) %>%
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
   hc_tooltip(crokkhairs = TRUE,
              shared = TRUE, borderWidth = 5) %>%
   hc_title(
     text = "<b><i></i>Sistema Neurológico</b>",
     margin = 20,
     align = "left",
     style = list(color = "#22A884", useHTML = TRUE)
   ) %>%
   hc_add_theme(hc_theme_google()) %>%
   hc_exporting(enabled = TRUE) 
 
 