
## Plotagem : Vigilancia por estado do paciente
summ_vigilanc <- raw_base_rename %>%
  dplyr::filter(!is.na(vigilancia), !is.na(estado_paciente)) %>%  # Remover linhas com NA
  group_by(vigilancia) %>%
  dplyr::summarise(
    Ligeiro = sum(estado_paciente == "Ligeiro"),
    Grave = sum(estado_paciente == "Grave")
  ) %>% 
  pivot_longer(cols = c( Ligeiro,Grave), names_to = "estado_paciente", values_to = "Casos")

# Criar o gráfico de colunas empilhadas
Plot_summ_vigilanc <- hchart(
  summ_vigilanc, 
  'column', 
  hcaes(x = vigilancia, y = Casos, group = estado_paciente),
  dataLabels = list(enabled = TRUE, style = list(fontSize = "16px")),
  stacking = "normal"
) %>%
  hc_colors(c("#C00000","#002060")) %>%
  hc_yAxis(labels=list(
    style= list (
      fontSize= '16px',
      color= 'black'
    ))) %>% 
  hc_xAxis(labels=list(
    style= list (
      fontSize= '16px',
      color= 'black'
    ))) %>% 
  hc_tooltip(crosshairs = TRUE,
             shared = TRUE, borderWidth = 5) %>%
  hc_title(
    text = "Vigilancia <i></i> vs <b>Estado do Paciente</b>",
    margin = 20,
    align = "left",
    style = list(color = "#22A884", useHTML = TRUE)
  ) %>%
  hc_add_theme(hc_theme_google()) %>%
  hc_exporting(enabled = TRUE) 
