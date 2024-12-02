
  AA<-raw_base_rename
  
dd <- bind_cols(
  AA %>% 
    dplyr::filter(!is.na(asma) & asma == "sim") %>% 
    dplyr::summarise(asma = n()),
  
  AA %>% 
    dplyr::filter(!is.na(hepatica_cronica) & hepatica_cronica == "sim") %>% 
    dplyr::summarise(hepatica_cronica = n()),

  AA %>% 
    dplyr::filter(!is.na(renal_cronica) & renal_cronica == "sim") %>% 
    dplyr::summarise(renal_cronica = n()),
  
  AA %>% 
    dplyr::filter(!is.na(neuromuscular) & neuromuscular == "sim") %>% 
    dplyr::summarise(neuromuscular = n()),
  
  AA %>% 
    dplyr::filter(!is.na(diabetes) & diabetes == "sim") %>% 
    dplyr::summarise(diabetes = n()),
  
  AA %>% 
    dplyr::filter(!is.na(cardiovascular) & cardiovascular == "sim") %>% 
    dplyr::summarise(cardiovascular = n()),
  
  AA %>% 
    dplyr::filter(!is.na(gravidez) & gravidez == "sim") %>% 
    dplyr::summarise(gravidez = n()),
  
  AA %>% 
    dplyr::filter(!is.na(hiv) & hiv == "sim") %>% 
    dplyr::summarise(hiv = n())
)

  cond_clinic_associadas<-data.frame(
    Condicoes_Clinicas_associadas=c("asma","Doença Hepática crónica","Doença Renal Crónica","Doença Neuromuscular","Diabetes","Doença Cardiovascular","Gravidez","HIV"),
    Frequencia=c(dd$asma,dd$hepatica_cronica,dd$renal_cronica,dd$neuromuscular,dd$diabetes,dd$cardiovascular,dd$gravidez,dd$hiv)
  )

 # Criar o gráfico de colunas empilhadas
 Plot_ccass <- hchart(
   cond_clinic_associadas, 
   'bar', 
   hcaes(x = Condicoes_Clinicas_associadas, y = Frequencia, group = Condicoes_Clinicas_associadas),
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
 
 