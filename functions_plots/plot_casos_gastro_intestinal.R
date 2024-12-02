
  AA<-raw_base_rename
  
dd <- bind_cols(
  AA %>% 
    dplyr::filter(!is.na(diarreia_aguda) & diarreia_aguda == "sim") %>% 
    dplyr::summarise(diarreia_aguda = n()),
  
  AA %>% 
    dplyr::filter(!is.na(diarreia_cronica) & diarreia_cronica == "sim") %>% 
    dplyr::summarise(diarreia_cronica = n()),

  AA %>% 
    dplyr::filter(!is.na(Gastroenterite_aguda) & Gastroenterite_aguda == "sim") %>% 
    dplyr::summarise(Gastroenterite_aguda = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Peritonite) & Peritonite == "sim") %>% 
    dplyr::summarise(Peritonite = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Apendicite) & Apendicite == "sim") %>% 
    dplyr::summarise(Apendicite = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Colecistite) & Colecistite == "sim") %>% 
    dplyr::summarise(Colecistite = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Ascite) & Ascite == "sim") %>% 
    dplyr::summarise(Ascite = n())
)

  casos_gastroIntestinal<-data.frame(
    Sistema_GastroIntestinal=c("Diarreia aguda","Diarreia Cronica","Gastroenterite aguda","Peritonite","Apendicite","Colecistite","Ascite"),
    Frequencia=c(dd$diarreia_aguda,dd$diarreia_cronica,dd$Gastroenterite_aguda,dd$Peritonite,dd$Apendicite,dd$Colecistite,dd$Ascite)
  )

 # Criar o gráfico de colunas empilhadas
 Plot_casos_sist_gastroIntestinal <- hchart(
   casos_gastroIntestinal, 
   'bar', 
   hcaes(x = Sistema_GastroIntestinal, y = Frequencia, group = Sistema_GastroIntestinal),
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
 
 