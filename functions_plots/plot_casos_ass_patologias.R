
## Plotagem: nome local de atendimento por estado paciente


casos_provincia <- raw_base_rename %>%
  dplyr::filter(!is.na(vigilancia)) %>%  # Remover linhas com faixa_etaria NA 
  group_by(vigilancia) %>%
  dplyr::summarise(Casos = n()) 


# Criar o gráfico de colunas empilhadas
Plot_casos_provincia <- hchart(
  casos_provincia, 
  'column', 
  hcaes(x = vigilancia, y = Casos, group = vigilancia),
  dataLabels = list(enabled = TRUE, style = list(fontSize = "11px")),
  stacking = "normal"
) %>%
  hc_colors(c("#ad2f30", "#32A594","#EFC000FF", "#203d7d","#a0a0ed","#203d7e","#a0a0ad", '#f0f0f5','#00a1cd','#0058b8','#002060','#adad30')) %>%
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
    text = "Local de Atendimento <i></i> vs <b>Estado do Paciente</b>",
    margin = 20,
    align = "left",
    style = list(color = "#22A884", useHTML = TRUE)
  ) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_exporting(enabled = TRUE) 

