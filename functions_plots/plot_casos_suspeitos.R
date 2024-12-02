
casos_sem_epi <- raw_base_rename %>%
  dplyr::filter(!is.na(Sem_Epi)) %>%  # Remover linhas com faixa_etaria NA 
  group_by(Sem_Epi) %>%
  dplyr::summarise(Casos = n()) 


 highchart() %>%
  hc_chart(type = "column") %>%
  
  hc_title(text = "Casos Suspeitos por semana Epi") %>%
  hc_xAxis(categories = casos_sem_epi$Sem_Epi) %>%
  hc_add_series(name = "Casos", data = casos_sem_epi$Casos, dataLabels = list(enabled = TRUE, style = list(fontSize = "16px"))) %>%
  hc_colors(c("#002060")) %>%
  hc_yAxis(labels = list(style = list(fontSize = '12px', color = 'black'))) %>%
  hc_xAxis(labels = list(style = list(fontSize = '12px', color = 'black'))) %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 5) %>%
  hc_title(text = "<b>Casos Suspeitos por semana Epi</b>", margin = 20, align = "left",
           style = list(color = "#22A884", useHTML = TRUE)) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_exporting(enabled = TRUE)
