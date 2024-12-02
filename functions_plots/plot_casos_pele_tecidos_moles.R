
  AA<-raw_base_rename
  
dd <- bind_cols(
  AA %>% 
    dplyr::filter(!is.na(Dermatite) & Dermatite == "sim") %>% 
    dplyr::summarise(Dermatite = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Fascite_necrotizante) & Fascite_necrotizante == "sim") %>% 
    dplyr::summarise(Fascite_necrotizante = n()),

  AA %>% 
    dplyr::filter(!is.na(Gangrena) & Gangrena == "sim") %>% 
    dplyr::summarise(Gangrena = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Sarcoma_de_Kaposi) & Sarcoma_de_Kaposi == "sim") %>% 
    dplyr::summarise(Sarcoma_de_Kaposi = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Piodermite_incl._Impetigo) & Piodermite_incl._Impetigo == "sim") %>% 
    dplyr::summarise(Piodermite_incl._Impetigo = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Erisipela_celulite) & Erisipela_celulite == "sim") %>% 
    dplyr::summarise(Erisipela_celulite = n()),
  
  AA %>% 
    dplyr::filter(!is.na(ulceras_de_pressao) & ulceras_de_pressao == "sim") %>% 
    dplyr::summarise(ulceras_de_pressao = n())
)

  casos_Sistema_pele_tecidos<-data.frame(
    Sistema_pele_tecidos=c("Dermatite","Fascite necrotizante","Gangrena","Sarcoma de Kaposi","Piodermite incl. Impetigo","Erisipela/celulite","Úlceras de pressão"),
    Frequencia=c(dd$Dermatite,dd$Fascite_necrotizante,dd$Gangrena,dd$Sarcoma_de_Kaposi,dd$Piodermite_incl._Impetigo,dd$Erisipela_celulite,dd$ulceras_de_pressao)
  )

 # Criar o gráfico de colunas empilhadas
 Plot_casos_sist_peletecidos <- hchart(
   casos_Sistema_pele_tecidos, 
   'bar', 
   hcaes(x = Sistema_pele_tecidos, y = Frequencia, group = Sistema_pele_tecidos),
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
     text = "<b><i></i>Pele e tecidos moles</b>",
     margin = 20,
     align = "left",
     style = list(color = "#22A884", useHTML = TRUE)
   ) %>%
   hc_add_theme(hc_theme_google()) %>%
   hc_exporting(enabled = TRUE) 
 
 