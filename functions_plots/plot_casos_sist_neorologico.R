
  AA<-raw_base_rename
  
ss <- bind_cols(
  AA %>% 
    dplyr::filter(!is.na(Meningite) & Meningite == "sim") %>% 
    dplyr::summarise(Meningite = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Encefalite) & Encefalite == "sim") %>% 
    dplyr::summarise(Encefalite = n()),

  AA %>% 
    dplyr::filter(!is.na(Malaria_cerebral) & Malaria_cerebral == "sim") %>% 
    dplyr::summarise(Malaria_cerebral = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Sindrome_convulsivo) & Sindrome_convulsivo == "sim") %>% 
    dplyr::summarise(Sindrome_convulsivo = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Alteracao_do_nivel_de_consciencia_incluindo_coma) & Alteracao_do_nivel_de_consciencia_incluindo_coma == "sim") %>% 
    dplyr::summarise(Alteracao_do_nivel_de_consciencia_incluindo_coma = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Lesao_ocupante_de_espaco) & Lesao_ocupante_de_espaco == "sim") %>% 
    dplyr::summarise(Lesao_ocupante_de_espaco = n()),
  
  AA %>% 
    dplyr::filter(!is.na(Asfixia_neonatal) & Asfixia_neonatal == "sim") %>% 
    dplyr::summarise(Asfixia_neonatal = n())
)

  casos_neorol<-data.frame(
    Desc_caso=c("Meningite","Encefalite","Malária cerebral","Síndrome convulsivo","Alteração do nível de consciência","Lesão ocupante de espaço","Asfixia neonatal"),
    N=c(ss$Meningite,ss$Encefalite,ss$Malaria_cerebral,ss$Sindrome_convulsivo,ss$Alteracao_do_nivel_de_consciencia_incluindo_coma,ss$Lesao_ocupante_de_espaco,ss$Asfixia_neonatal)
  )

 # Criar o gráfico de colunas empilhadas
 Plot_casos_sist_neorol <- hchart(
   casos_neorol, 
   'bar', 
   hcaes(x = Desc_caso, y = N, group = Desc_caso),
   dataLabels = list(enabled = TRUE, style = list(fontSize = "11px")),
   stacking = "normal"
 ) %>%
   hc_colors(c( "#32A594","#EFC000FF", "#203d7d","#a0a0ed","#203d7e", '#00a1cd','#adad30')) %>%
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
   hc_tooltip(crosshairs = TRUE,
              shared = TRUE, borderWidth = 5) %>%
   hc_title(
     text = "<b><i></i>Sistema Neurológico</b>",
     margin = 20,
     align = "left",
     style = list(color = "#22A884", useHTML = TRUE)
   ) %>%
   hc_add_theme(hc_theme_google()) %>%
   hc_exporting(enabled = TRUE) 
 
 