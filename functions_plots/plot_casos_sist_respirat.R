
  SR<-raw_base_rename
  
dados <- bind_cols(
  SR %>% 
    dplyr::filter(!is.na(ivrs) & ivrs == "sim") %>% 
    dplyr::summarise(ivrs = n()),
  
  SR %>% 
    dplyr::filter(!is.na(laringotraqueite) & laringotraqueite == "sim") %>% 
    dplyr::summarise(laringotraqueite = n()),

  SR %>% 
    dplyr::filter(!is.na(laringite) & laringite == "sim") %>% 
    dplyr::summarise(laringite = n()),
  
  SR %>% 
    dplyr::filter(!is.na(Pneumonia_PN) & Pneumonia_PN == "sim") %>% 
    dplyr::summarise(Pneumonia_PN = n()),
  
  SR %>% 
    dplyr::filter(!is.na(Broncopneumonia_e_aspirativa_BPN) & Broncopneumonia_e_aspirativa_BPN == "sim") %>% 
    dplyr::summarise(Broncopneumonia_e_aspirativa_BPN = n()),
  
  SR %>% 
    dplyr::filter(!is.na(Sindrome_de_aspiracao_meconial_SAM) & Sindrome_de_aspiracao_meconial_SAM == "sim") %>% 
    dplyr::summarise(Sindrome_de_aspiracao_meconial_SAM = n()),
  
  SR %>% 
    dplyr::filter(!is.na(Tuberculose_pulmonar) & Tuberculose_pulmonar == "sim") %>% 
    dplyr::summarise(Tuberculose_pulmonar = n()),
  SR %>% 
    dplyr::filter(!is.na(Abcesso_pulmonar) & Abcesso_pulmonar == "sim") %>% 
    dplyr::summarise(Abcesso_pulmonar = n()),
  SR %>% 
    dplyr::filter(!is.na(Empiema) & Empiema == "sim") %>% 
    dplyr::summarise(Empiema = n()),
  SR %>% 
    dplyr::filter(!is.na(COVID_19) & COVID_19 == "sim") %>% 
    dplyr::summarise(COVID_19 = n()),
  SR %>% 
    dplyr::filter(!is.na(Bronquiolite) & Bronquiolite == "sim") %>% 
    dplyr::summarise(Bronquiolite = n()),
  SR %>% 
    dplyr::filter(!is.na(Insuficiencia_respiratoria) & Insuficiencia_respiratoria == "sim") %>% 
    dplyr::summarise(Insuficiencia_respiratoria = n()),
  SR %>% 
    dplyr::filter(!is.na(Taquipneia_transitorio_do_recem_nascido_TTRN) & Taquipneia_transitorio_do_recem_nascido_TTRN == "sim") %>% 
    dplyr::summarise(Taquipneia_transitorio_do_recem_nascido_TTRN = n()),
  SR %>% 
    dplyr::filter(!is.na(Prematuridade_e_SDR_DMH) & Prematuridade_e_SDR_DMH == "sim") %>% 
    dplyr::summarise(Prematuridade_e_SDR_DMH = n())
)

  casos_respiratorios<-data.frame(
    Sistema_Respiratorio=c("IVRS","Laringotraqueíte","Laringite","Pneumonia (PN)","Broncopneumonia e aspirativa (BPN)","Síndrome de aspiração meconial (SAM)","Tuberculose pulmonar","Abcesso pulmonar","Empiema","COVID-19","Bronquiolite","Insuficiência respiratória","Taquipneia transitório do recém-nascido (TTRN)","Prematuridade e SDR/DMH"),
    frequencia=c(dados$ivrs,dados$laringotraqueite,dados$laringite,dados$Pneumonia_PN,dados$Broncopneumonia_e_aspirativa_BPN,dados$Sindrome_de_aspiracao_meconial_SAM,dados$Tuberculose_pulmonar,dados$Abcesso_pulmonar,dados$Empiema,dados$COVID_19,dados$Bronquiolite,dados$Insuficiencia_respiratoria,dados$Taquipneia_transitorio_do_recem_nascido_TTRN,dados$Prematuridade_e_SDR_DMH)
  )

 # Criar o gráfico de colunas empilhadas
 Plot_casos_sist_respirt <- hchart(
   casos_respiratorios, 
   'bar', 
   hcaes(x = Sistema_Respiratorio, y = frequencia, group = Sistema_Respiratorio),
   dataLabels = list(enabled = TRUE, style = list(fontSize = "11px")),
   stacking = "normal"
 ) %>%
   hc_colors(c( "#ad2f30", "#32A594","#EFC000FF", "#203d7d","#a0a0ed","#203d7e","#a0a0ad", '#f0f0f5','#00a1cd','#0058b8','#002060','#adad30')) %>%
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
   hc_tooltip(crodadoshairs = TRUE,
              shared = TRUE, borderWidth = 5) %>%
  
   hc_add_theme(hc_theme_google()) %>%
   hc_exporting(enabled = TRUE) 
 
 