
raw_base_rename$hospitalizacao <- toupper(raw_base_rename$hospitalizacao)

casos_internamento <- raw_base_rename %>%
  mutate(hospitalizacao = recode(hospitalizacao,
      "SIM" = "Internado",
      "NAO" = "Ambulatório")) %>% 
  dplyr::filter(!is.na(hospitalizacao)) %>%  # Remover linhas com faixa_etaria NA 
  group_by(hospitalizacao) %>%
  dplyr::summarise(Casos = n()) 


 # Criar o gráfico de colunas empilhadas
 Plot_casos_internamento <- hchart(
   casos_internamento, 
   'column', 
   hcaes(x = hospitalizacao, y = Casos, group = hospitalizacao),
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
     text = "<b><i></i>Regime de Atendimento</b>",
     margin = 20,
     align = "left",
     style = list(color = "#22A884", useHTML = TRUE)
   ) %>%
   hc_add_theme(hc_theme_google()) %>%
   hc_exporting(enabled = TRUE) 
 
 
 Plot_casos_internamento <- hchart(
   casos_internamento, 
   'bar', 
   hcaes(x = hospitalizacao, y = Casos, group = hospitalizacao),
   dataLabels = list(enabled = TRUE, style = list(fontSize = "11px")),
   stacking = "normal"
 ) %>%
   hc_colors(c("#32A594", "#ad2f30")) %>%
   hc_xAxis(labels = list(
     style = list (
       fontSize = '16px',
       color = 'black'
     ))) %>% 
   hc_yAxis(labels = list(
     style = list (
       fontSize = '12px',
       color = 'black'
     ))) %>% 
   hc_tooltip(crosshairs = TRUE,
              shared = TRUE, borderWidth = 5) %>%
   hc_title(
     text = "<b><i></i>Regime de Atendimento</b>",
     margin = 20,
     align = "left",
     style = list(color = "#22A884", useHTML = TRUE)
   ) %>%
   hc_add_theme(hc_theme_google()) %>%
   hc_exporting(enabled = TRUE)
 
 