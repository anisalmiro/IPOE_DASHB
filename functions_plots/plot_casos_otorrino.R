
  AA<-raw_base_rename
  
kk <- bind_cols(
  AA %>% 
    dplyr::filter(!is.na(Sinusite) & Sinusite == "sim") %>% 
    dplyr::summarise(Sinusite = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Rinite) & Rinite == "sim") %>% 
    dplyr::summarise(Rinite = n()),

  AA %>% 
    dplyr::filter(!is.na(Amigdalite) & Amigdalite == "sim") %>% 
    dplyr::summarise(Amigdalite = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Laringite) & Laringite == "sim") %>% 
    dplyr::summarise(Laringite = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Faringite) & Faringite == "sim") %>% 
    dplyr::summarise(Faringite = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Dor_na_Garganta) & Dor_na_Garganta == "sim") %>% 
    dplyr::summarise(Dor_na_Garganta = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Otite) & Otite == "sim") %>% 
    dplyr::summarise(Otite = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Vertigem) & Vertigem == "sim") %>% 
    dplyr::summarise(Vertigem = n())
)

  casos_otorrinol<-data.frame(
    Sistema_otorrino=c("Sinusite","Rinite","Amigdalite","Laringite","Faringite","Dor na Garganta","Otite","Vertigem"),
    Frequencia=c(kk$Sinusite,kk$Rinite,kk$Amigdalite,kk$Laringite,kk$Faringite,kk$Dor_na_Garganta,kk$Otite,kk$Vertigem)
  )

 # Criar o gráfico de colunas empilhadas
 Plot_casos_otorrinologolog <- hchart(
   casos_otorrinol, 
   'bar', 
   hcaes(x = Sistema_otorrino, y = Frequencia, group = Sistema_otorrino),
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
 
 