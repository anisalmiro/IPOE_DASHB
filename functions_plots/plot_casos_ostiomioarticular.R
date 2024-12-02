
  AA<-raw_base_rename
  
kk <- bind_cols(
  AA %>% 
    dplyr::filter(!is.na(Artrite_septica) & Artrite_septica == "sim") %>% 
    dplyr::summarise(Artrite_septica = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Osteomielite) & Osteomielite == "sim") %>% 
    dplyr::summarise(Osteomielite = n()),

  AA %>% 
    dplyr::filter(!is.na(TB_ossea_Mal_de_Pott) & TB_ossea_Mal_de_Pott == "sim") %>% 
    dplyr::summarise(TB_ossea_Mal_de_Pott = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Miosite) & Miosite == "sim") %>% 
    dplyr::summarise(Miosite = n())
)

  casos_osteomolite<-data.frame(
    Sistema_Osteomioarticular=c("Artrite séptica","Osteomielite","TB óssea (Mal de Pott)","Miosite"),
    Frequencia=c(kk$Artrite_septica,kk$Osteomielite,kk$TB_ossea_Mal_de_Pott,kk$Miosite)
  )

 # Criar o gráfico de colunas empilhadas
 Plot_casos_osteomolite <- hchart(
   casos_osteomolite, 
   'bar', 
   hcaes(x = Sistema_Osteomioarticular, y = Frequencia, group = Sistema_Osteomioarticular),
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
 
 