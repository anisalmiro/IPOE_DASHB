
testados_por_faixa <- raw_base_rename %>%
  dplyr::filter(!is.na(faixa_etaria)) %>%  # Remover linhas com faixa_etaria NA 
  group_by(faixa_etaria) %>%
  dplyr::summarise(testados = n()) 

Plot_Faix_etar =  testados_por_faixa %>%  hchart(
  'column', hcaes(x = 'faixa_etaria', y = 'testados', group = 'faixa_etaria'), dataLabels = list(enabled = TRUE, style = list(fontSize = "18px")),
  stacking = "normal"
) %>%
  hc_colors(c("#EFC000FF")) %>%
  hc_yAxis(labels=list(
    style= list (
      fontSize= '18px',
      color= 'black'
    ))) %>%    hc_xAxis(labels=list(
      style= list (
        fontSize= '18px',
        color= 'black'
      ))) %>% 
  hc_tooltip(crosshairs = TRUE,
             shared = TRUE, borderWidth = 5) %>%
  
  hc_title(
    text = "Faixa etaria <i></i> vs <b>Testados</b>",
    margin = 20,
    align = "left",
    style = list(color = "#22A884", useHTML = TRUE)
  ) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_exporting(enabled = TRUE) %>% 
  hc_legend(enabled = FALSE)


Plot_Faix_etar
