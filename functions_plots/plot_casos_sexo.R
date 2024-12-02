
raw_base_rename$sexo <- toupper(raw_base_rename$sexo)

casos_sexo <- raw_base_rename %>%
  mutate(sexo = recode(sexo,
      "F" = "Femenino",
      "M" = "Masculino")) %>% 
  dplyr::filter(!is.na(sexo)) %>%  # Remover linhas com faixa_etaria NA 
  group_by(sexo) %>%
  dplyr::summarise(Casos = n()) 

 
 # Criar o gráfico de colunas empilhadas
 Plot_casos_sexo <- hchart(
   casos_sexo, 
   'column', 
   hcaes(x = sexo, y = Casos, group = sexo),
   dataLabels = list(enabled = TRUE, style = list(fontSize = "11px")),
   stacking = "normal"
 ) %>%
   hc_colors(c("#ad2f30", "#32A594")) %>%
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
     text = "<b><i></i>Descrição dos pacientes por sexo</b>",
     margin = 20,
     align = "left",
     style = list(color = "#22A884", useHTML = TRUE)
   ) %>%
   hc_add_theme(hc_theme_google()) %>%
   hc_exporting(enabled = TRUE) 
 
 
 
 
 Plot_casos_sexo <- hchart(
   casos_sexo, 
   'pie', 
   hcaes(name = sexo, y = Casos),
   dataLabels = list(
     enabled = TRUE, 
     format = '<b>{point.name}</b>: {point.y} ({point.percentage:.1f}%)', 
     style = list(fontSize = "11px"),
     distance = -30  # Ajustar distância para dentro do pie
   )
 ) %>%
   hc_colors(c("#ad2f30", "#32A594")) %>%
   hc_tooltip(pointFormat = '<b>{point.y}</b> casos<br><b>{point.percentage:.1f}%</b>') %>%
   hc_title(
     text = "<b><i></i>Descrição dos pacientes por sexo</b>",
     margin = 20,
     align = "top",
     style = list(color = "#22A884", useHTML = TRUE)
   ) %>%
   hc_add_theme(hc_theme_google()) %>%
   hc_exporting(enabled = TRUE)
 