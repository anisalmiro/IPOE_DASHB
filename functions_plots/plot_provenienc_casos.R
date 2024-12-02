
raw_base_rename$distrito_resid <- toupper(raw_base_rename$distrito_resid)

casos_provincia <- raw_base_rename %>%
  dplyr::filter(!is.na(provincia_resid)) %>%  # Remover linhas com faixa_etaria NA 
  group_by(provincia_resid,distrito_resid) %>%
  dplyr::summarise(Casos = n()) 


view(casos_provincia)

 highchart() %>%
  hc_chart(type = "column") %>%
  
  hc_title(text = "Casos Suspeitos por Distrito") %>%
  hc_xAxis(categories = casos_provincia$distrito_resid) %>%
  hc_add_series(name = "Casos", data = casos_provincia$Casos, dataLabels = list(enabled = TRUE, style = list(fontSize = "16px"))) %>%
  hc_colors(c("#002060")) %>%
  hc_yAxis(labels = list(style = list(fontSize = '12px', color = 'black'))) %>%
  hc_xAxis(labels = list(style = list(fontSize = '12px', color = 'black'))) %>%
  hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 5) %>%
  hc_title(text = "<b>Proveniencia dos casos</b>", margin = 20, align = "left",
           style = list(color = "#22A884", useHTML = TRUE)) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_exporting(enabled = TRUE)
