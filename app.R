
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    Anisio Bule
#


#libraries
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(rpivotTable)
#internet
library(httr)

#fonts
library(extrafont)

#carpintery
library(glue)
library(scales)
library(lubridate)
library(forcats)
library(reactable)
library(htmltools)
library(fontawesome)
library("htmlwidgets")
#maps
#library(sf)



#ggplot
library(grid)
library(ggplot2)
library(ggrepel)
library(cowplot)


#tidyverse: 
library(tidyr)
library(stringr)
library(dplyr)


#other
library(rio)
library(cli)
library(zoo)

#   devtools::install_github("jcheng5/googleCharts")
#   https://github.com/jcheng5/googleCharts

#shiny
library(DT)
library(Hmisc)
library(htmltools)

library(shinydashboardPlus)
library(shinyauthr)
library(shinymanager)
library("shinydashboard")
library("rpivotTable")
library(shinythemes)

library(bslib)
library(shinyWidgets)
library(highcharter)
library(rsconnect)



#Conexao com base de dados
library(DBI)
library(RSQLite)
library(readxl)
#Mapa
library(leaflet)

## Loadind data ##
load(file = 'data/DB_Dashboard/raw_base_rename.rda')
load(file = 'data/DB_Dashboard/Base_Geral.rda')
load(file = 'data/DB_Dashboard/Base_semanal.rda')
load(file = 'data/DB_Dashboard/Base_diaria.rda')


#metodos para inserir na base de daods

initialize_database <- function() {
  if (!file.exists("users.sqlite")) {
    con <- dbConnect(RSQLite::SQLite(), dbname = "users.sqlite")
    dbExecute(con, "CREATE TABLE IF NOT EXISTS credentials (user TEXT, password TEXT, admin TEXT)")
    dbDisconnect(con)
  }
}

# Function to insert user registration data into the database
insert_into_database <- function(user, password, admin) {
  print(paste("User:", user))
  print(paste("Password:", password))
  print(paste("Admin:", admin))
  
  con <- dbConnect(RSQLite::SQLite(), dbname = "users.sqlite")
  dbExecute(con, "INSERT INTO credentials (user, password, admin) VALUES (?, ?, ?)", 
            params = list(user, password, admin))
  dbDisconnect(con)
  return("Utilizador registado com sucesso!")
  
  updateTextInput(session, "user_name", value = "")
  updateTextInput(session, "password", value = "")
  updateSelectInput(session, "admin", selected = NULL)
  
}

select_info_db <- function(user, password, admin) {
  db_path <- "users.sqlite"
  con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
  
  # Retrieve user information from the database
  credentials <- dbGetQuery(con, "SELECT user, password, admin FROM credentials")
  
  dbDisconnect(con)
  
  return(credentials)
}


# Specify the path where you want to create the database
db_path <- "users.sqlite"
con <- dbConnect(RSQLite::SQLite(), dbname = db_path)
# Disconnect from the database
#result <- dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table';")
credentials<-dbGetQuery(con, "SELECT user,password,admin FROM credentials")
dbDisconnect(con)



inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"


#library(esquisse)

# Menu de Autenticacao
#metodo de credenciais

#End of Hiding Menu

#deployAPI()


#Metodos auxiliares
#
#ler base de dados a cada 3min
autoReload <- reactiveTimer(60000)

ultima_data <- max(raw_base_rename$Data_reporte)
#ultima_data <- tail(raw_base_rename$Data_reporte, n = 1)

PAGE_TITLE <- "Vigilância Integrada
"

#refresn para carregamento da base de dados
reloadDB <- reactive({
  autoReload() 
  select_info_db()
})


ui <- secure_app(
  theme = shinythemes::shinytheme("flatly"),
  head_auth = tags$script(inactivity),
  tags_top = tags$img(
    src = "INS.png", width = 100
  ),
  fluidPage(
    tags$style(HTML(".navbar{background-color: #00879C}")),
    titlePanel(
      windowTitle = PAGE_TITLE,
      title = div(
        img(
          src = "INS.png",
          height = 100,
          width = 170,
          style = "margin:10px 10px"
        ),
        PAGE_TITLE
      )
    ),
    navbarPage(
      theme = shinytheme("flatly"),
      collapsible = TRUE,
      "INS",
      position = "static-top",
      tags$html(HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="www.ins.gov.mz">INS</a>')),
      windowTitle = " IDS- REPORTE DE DADOS",
      
      header = tagList(
        useShinydashboard() #Garante que fornece estruturas e estilos adicionais para criar painéis de controle bonitos segundo a nossa escolha
      ),
      
      #abba de resumo de dados
      tabPanel(
        "Dashboard",
        verbatimTextOutput("auth_output"),
        icon = icon("dashboard"),
        
        img(src="sample.gif", align = "left",height='25px',width='30px'),
        "Última actualizacão: ", ultima_data, tags$b(uiOutput("date")),
        "Algumas informações estão disponíveis no site do INS", tags$a(href="https://ins.gov.mz/", "INS Official site", target="_blank"),
        tags$br(), hr(),
        
        
        #  fluidRow(
        #    div(
        #      infoBoxOutput("progressBox", width = 3)
        #    ),
        #    div(
        #      infoBoxOutput("progressBox2", width = 3)
        #    ),
        #    div(
        #      infoBoxOutput("progressBox3", width = 3)
        #    ),
        #    div(
        #      infoBoxOutput("progressBox4", width = 3)
        #    ),
        #    div(
        #      infoBoxOutput("progressBox5", width = 3)
        #    ),
        
        #  ),
        # Descritivo da Vigilancia
        ##
        tags$head(
          tags$style(HTML("
        .custom-tab-content {
          background-color: #f0f8ff; /* Cor do background*/
          padding: 20px;
          font-size: 18px; /* Tamanho da fonte */
        }
        .nav-tabs-custom .nav-tabs li.active a {
          background-color: #4682b4; /* Cor da aba ativa */
          color: white; /* Cor do texto na aba ativa */
        }
        .nav-tabs-custom .nav-tabs li a {
          color: #4682b4; /* Cor do texto nas abas */
        }
      "))
        ),
        fluidRow(
          tabBox( 
            title = "OS DADOS PODEM SER FILTRADOS POR: ", tags$br(),tags$a("Provincia, Posto sentinela, Semana Epidemiologica, Vigilancia, ", target="_blank"),
            id = "tabset1",
            height = "auto",
            width = 12, 
            
            fluidRow(
              tags$div(
                
                tags$br(),tags$br()),
              panel( 
                selectizeGroupUI(
                  
                  id = "my-filters",
                  params = list(
                    provincia = list(inputId = "provincia", title = "Provincia:"),
                    unidade_sanitaria = list(inputId = "unidade_sanitaria", title = "Posto sentinela:"),
                    ano = list(inputId = "ano", title = "Ano:"),
                    mes = list(inputId = "mes", title = "Mês:"),
                    Sem_Epi = list(inputId = "Sem_Epi", title = "Semana Epidemiologica:"),
                    Vigilancia = list(inputId = "vigilancia", title = "Vigilancia:")
                    
                  )
                ), status = "primary"
              ),
              
              
              panel("Descrição do Dashboard", style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                    div(class = "custom-tab-content",
                        p("A vigilância integrada ou y é uma vigilância que etc, início e locais de implementação, definição de caso --------------------------------------------------------------------------")
                    )
              ),
              
            ),
            
          ),
          tabBox( 
            title = "Descrição dos Pacientes  na Semana Epidemiológica (SE) de Reporte",
            id = "tabset1",
            height = "auto",
            width = 12, 
            
            fluidRow(
              
              box(#"Número de casos suspeitos por SE",
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput("casos_susp"), width = 4),
              
              box(#"Número de casos associados",
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput("num_casos_associados"), width = 4),
              
              box(#"Descrição por proveniencia de casos",
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput("prov_casos"), width = 4),
              
              box(#"Descrição por sexo",
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput("desc_sexo"), width = 3),
              
              box(#"Descrição por Faixa-etária",
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput("desc_faica_etar"), width = 3),
              
              box( #"Descicao por Hospitalizacao",
                
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput("des_hospitaliz"), width = 3),
              
              box( #"Testados por Local de Atendimento",
                
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput("condic_clinicas"), width = 3)
              
              
            ),
            
          ),
          #Segunda Secao de apresentacao de dados
          tabBox( 
            title = "Motivo de hospitalização",
            id = "tabset1",
            height = "auto",
            width = 12, 
            
            fluidRow(
              
              box(#"Sistema Neurológico",
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput("sist_neorolog"), width = 4),
              
              box(#"Sistema Respiratório",
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput("sist_respirat"), width = 4),
              
              box(#"Sistema Cardiovascular",
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput("sist_cadiovasc"), width = 4),
              
              box(#"Sistema Gastrointestinal",
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput("sist_gastr"), width = 4),
              
              box(#"Sistema Genitourinário",
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput("sist_genitorian"), width = 4),
              
              box( #"Pele e tecidos moles",
                
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput("sis_pele_tecidos"), width = 4),
              
              box( #"Sistema Osteomioarticular",
                
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput("sis_osteomioa"), width = 4),
              
              box( #"Otorrinolaringologia",
                
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput("sis_otorrino"), width = 4),
              
              box( #"condições clínicas associadas",
                
                style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
                highchartOutput(""), width = 4)
              
            ),
            
          )
          
        ),
        
        hr(), br(),
        fluidRow(
          tabBox(
            side = "left",
            width = "auto",
            height = "auto",
            
            tabPanel("Base de dados de Reporter Semanal",
                     tags$style(
                       HTML("
                            #download1 {
                              width: auto%;
                              height: auto%;
                            }
                          ")
                     ),
                     fluidRow(
                       div(
                         downloadButton("download1", "BAIXAR BD"), 
                         style = "display: inline-block;"
                       ),
                       div(
                         reactableOutput("db_semanal")
                       )
                     )
                     
            )
          )
        )
      ),
      
      #aba de tabelas
      tabPanel(
        "Tabelas",
        icon = icon("table"),
        
        panel("secção para analises Basicas no Dashboard", style = "font-size: 18px; font-weight: bold; margin-bottom: 15px;",
              div(class = "custom-tab-content",
                  p("Esta secção permite fazer algumas analises preliminares dos dados colhidos a nível dos postos sentinela")
              )
        ),
        tabBox(
          side = "left",
          width = "auto",
          height = "auto",
          
          tabPanel(
            "TABELA PIVO",
            width = "auto",
            fluidRow(
              tabBox(
                tabPanel(
                  rpivotTableOutput("pivotTable")
                ),
                height = "auto"
              )
            )
          ),
          tabPanel(
            "Base de dados Semanal",
            sidebarPanel(
              tags$br(),
              selectInput("dataD", "Data de Reporte", choices = unique(BD_diaria$Data_reporte)),
              width = 2,
              downloadButton("downloadData", "Download")
            ),
            mainPanel(
              reactableOutput("Toda_BD"),
              filterable = TRUE,
              minRows = 10,
              width = "auto"
            )
          )
          
        )
      ),
      #aba de tabelas
      tabPanel(
        "Monitoria",
        icon = icon("microscope"),
        tabBox(
          side = "left",
          width = "auto",
          height = "auto",
          tabPanel(
            "Infecções Respiratórias Agudas",
            mainPanel(
              
            )
          ),
          tabPanel(
            "Febres Agudas",
            mainPanel(
              
            )
          ),
          tabPanel(
            "Diarreia",
            mainPanel(
              
            )
          ),
          tabPanel(
            "Resistência aos antibióticos em Isolados de Hemocultura",
            mainPanel(
              
            )
          ),
        )
      ),
    ),
    tags$footer(
      "Algumas informações estão disponíveis no site do INS",
      align = "center",
      tags$a(href = "https://ins.gov.mz/", "INS Official site", target = "_blank"),
      tags$br(),
      tags$br(),
      print("Instituto Nacional de Saúde - @ Todos Direitos reservados - por RGDTIC", align = "center")
    ),
    enable_admin = TRUE,
    language = "en"
  )
  
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  #Verificacao se esta autenticado
  result_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  #visualizar dados do utilizador na tela
  #output$auth_output <- renderPrint({
  #  reactiveValuesToList(result_auth)
  #})
  
  observe({
    print(input$shinymanager_where)
    print(input$shinymanager_language)
  })
  #Fim metodo Authenticacao
  
  
  # Bloco de codigo para filtragem de provincia, unidade sanitaria, data de reporte e vigilancia
  
  res_mod <- callModule(
    module = selectizeGroupServer,
    id = "my-filters",
    data = raw_base_rename,
    vars = c("ano","mes","provincia", "unidade_sanitaria", "vigilancia","Sem_Epi")
  )
  
  #fim dos metodos para filtragem de provincia, unidade sanitaria, data de reporte e vigilancia
  
  
  #Baixar BD do estudo
  output$download1 <- downloadHandler(
    filename = function() {
      paste0("Vigilancia -", ultima_data, ".csv")
    },
    content = function(file) {
      if (!is.null(result_auth) && result_auth$admin == "True") {
        
        if (exists("BD_diaria") && is.data.frame(BD_diaria)) {
          # Write Base_geral to a CSV file
          write.csv(Base_geral, file, row.names = FALSE)
        } else {
          showModal(modalDialog(
            title = "Erro ao exportar",
            "A Base de Dados não está disponível ou não está no formato correto."
          ))
        }
      } else {
        showModal(modalDialog(
          title = "Notificação",
          success_message <- "Contate o administrador para acessar a base de dados."
        ))
      }
    }
  )
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0(input$dataD, ".csv")
    },
    content = function(file) {
      if (!is.null(result_auth) && result_auth$admin == "True") {
        # Check if Base_geral is available and correctly formatted
        if (exists("r_Base") && is.data.frame(r_Base)) {
          write.csv(r_Base, file, row.names = FALSE)
          
        } else {
          showModal(modalDialog(
            title = "Erro ao exportar",
            "A Base de Dados não está disponível ou não está no formato correto."
          ))
        }
      } else {
        showModal(modalDialog(
          title = "Notificação",
          success_message <- "Contate o administrador para acessar a base de dados."
        ))
      }
    }
  )
  
  
  #PIVOTABLE
  output$pivotTable <- renderRpivotTable({
    rpivotTable(r_Base,  rows = c( "provincia"),cols="vigilancia",
                vals = "vigilancia", aggregatorName = "Count", rendererName = "Bar Chart",
                width="auto")
    
  })
  
  #usando recurso do datatable para mostrar tabela de dados colhidos
  output$db_semanal<-renderReactable({
    reactable(r_Base)
  })
  
  output$Toda_BD <- renderReactable({
    filtroprov<-reactable(subset(r_Base, r_Base$Data_reporte == input$dataD))
  })
  
  
  #Casos suspeitos por semanaEpi
  output$casos_susp <- renderHighchart({
    
    BASE_FINAL_V1 <- res_mod()
    
    casos_sem_epi <- BASE_FINAL_V1 %>%
      dplyr::filter(!is.na(Sem_Epi)) %>%  # Remover linhas com faixa_etaria NA 
      group_by(Sem_Epi) %>%
      dplyr::summarise(Semana_EPI = n()) 
    
    
    highchart() %>%
      hc_chart(type = "column") %>%
      
      hc_title(text = "Número de casos suspeitos por SE") %>%
      hc_xAxis(categories = casos_sem_epi$Sem_Epi) %>%
      hc_add_series(name = "Semana_EPI", data = casos_sem_epi$Semana_EPI, dataLabels = list(enabled = TRUE, style = list(fontSize = "16px"))) %>%
      hc_colors(c("#002060")) %>%
      hc_yAxis(labels = list(style = list(fontSize = '12px', color = 'black'))) %>%
      hc_xAxis(labels = list(style = list(fontSize = '12px', color = 'black'))) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 5) %>%
      hc_title(text = "<b>Casos Suspeitos por semana Epi</b>", margin = 20, align = "left",
               style = list(color = "#22A884", useHTML = TRUE)) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE)
    
  })
  
  
  
  #Grafico por resultado
  output$prov_casos<-renderHighchart({
    
    BASE_FINAL_V1 <- res_mod()
    
    raw_base_rename$distrito_resid <- toupper(raw_base_rename$distrito_resid)
    
    casos_provincia <- BASE_FINAL_V1 %>%
      dplyr::filter(!is.na(provincia_resid)) %>%  # Remover linhas com faixa_etaria NA 
      group_by(provincia_resid,distrito_resid) %>%
      dplyr::summarise(Distrito = n()) 
    
    
    
    
    highchart() %>%
      hc_chart(type = "column") %>%
      
      hc_title(text = "Casos Suspeitos por Distrito") %>%
      hc_xAxis(categories = casos_provincia$distrito_resid) %>%
      hc_add_series(name = "Distrito", data = casos_provincia$Distrito, dataLabels = list(enabled = TRUE, style = list(fontSize = "16px"))) %>%
      hc_colors(c("#002060")) %>%
      hc_yAxis(labels = list(style = list(fontSize = '12px', color = 'black'))) %>%
      hc_xAxis(labels = list(style = list(fontSize = '12px', color = 'black'))) %>%
      hc_tooltip(crosshairs = TRUE, shared = TRUE, borderWidth = 5) %>%
      hc_title(text = "<b>Proveniencia dos casos</b>", margin = 20, align = "left",
               style = list(color = "#22A884", useHTML = TRUE)) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE)
    
    
  })
  
  
  
  #Grafico por resultado
  output$desc_sexo<-renderHighchart({
    
    BASE_FINAL_V1 <- res_mod()
    
    BASE_FINAL_V1$sexo <- toupper(BASE_FINAL_V1$sexo)
    
    casos_sexo <- BASE_FINAL_V1 %>%
      mutate(sexo = recode(sexo,
                           "F" = "Femenino",
                           "M" = "Masculino")) %>% 
      dplyr::filter(!is.na(sexo)) %>%  # Remover linhas com faixa_etaria NA 
      group_by(sexo) %>%
      dplyr::summarise(Frequencia = n()) 
    
    
    
    Plot_casos_sexo <- hchart(
      casos_sexo, 
      'pie', 
      hcaes(name = sexo, y = Frequencia),
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
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
    
    
  })
  
  
  
  #Grafico por resultado
  output$desc_faica_etar<-renderHighchart({
    
    BASE_FINAL_V1 <- res_mod()
    
    testados_por_faixa <- BASE_FINAL_V1 %>%
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
        text = "<b><i></i> Descrição por Faixa-etária</b>",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)
      ) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
    
    
  })
  
  
  #grafico de colhei de dados por unidade sanitaria
  output$des_hospitaliz<-renderHighchart({
    ## Plotagem : Vigilancia por estado do paciente
    BASE_FINAL_V1 <- res_mod()
    BASE_FINAL_V1$hospitalizacao <- toupper(BASE_FINAL_V1$hospitalizacao)
    casos_internamento <- BASE_FINAL_V1 %>%
      mutate(hospitalizacao = recode(hospitalizacao,
                                     "SIM" = "Internado",
                                     "NAO" = "Ambulatório")) %>% 
      dplyr::filter(!is.na(hospitalizacao)) %>%  # Remover linhas com faixa_etaria NA 
      group_by(hospitalizacao) %>%
      dplyr::summarise(Frequencia = n()) 
    
    Plot_casos_internamento <- hchart(
      casos_internamento, 
      'bar', 
      hcaes(x = hospitalizacao, y = Frequencia, group = hospitalizacao),
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
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
    
  })
  
  
  
  #grafico de colhei de dados por unidade sanitaria
  output$num_casos_associados<-renderHighchart({
    ## Plotagem : Vigilancia por estado do paciente
    BASE_FINAL_V1 <- res_mod()
    
    casos_provincia <- BASE_FINAL_V1 %>%
      dplyr::filter(!is.na(vigilancia)) %>%  # Remover linhas com faixa_etaria NA 
      group_by(vigilancia) %>%
      dplyr::summarise(Frequencia = n()) 
    
    
    # Criar o gráfico de colunas empilhadas
    Plot_casos_provincia <- hchart(
      casos_provincia, 
      'column', 
      hcaes(x = vigilancia, y = Frequencia, group = vigilancia),
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
        text = "<b><i></i>Número de casos associados a outras patologias
</b>",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)
      ) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
    
    
  })
  
  
  #Plotagem de graficos sobre motivos de Hospitalizacao
  output$sist_neorolog <- renderHighchart({
    BASE_FINAL_V1 <- res_mod()
    
    AA<-BASE_FINAL_V1
    
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
      Frequencia=c(ss$Meningite,ss$Encefalite,ss$Malaria_cerebral,ss$Sindrome_convulsivo,ss$Alteracao_do_nivel_de_consciencia_incluindo_coma,ss$Lesao_ocupante_de_espaco,ss$Asfixia_neonatal)
    )
    
    # Criar o gráfico de colunas empilhadas
    Plot_casos_sist_neorol <- hchart(
      casos_neorol, 
      'bar', 
      hcaes(x = Desc_caso, y = Frequencia, group = Desc_caso),
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
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
    
  })
  
  
  output$sist_respirat <- renderHighchart({
    
    BASE_FINAL_V1 <- res_mod()
    
    SR<-BASE_FINAL_V1
    
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
      hc_title(
        text = "<b><i></i>Sistema Respiratório</b>",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)
      ) %>%
      hc_tooltip(crodadoshairs = TRUE,
                 shared = TRUE, borderWidth = 5) %>%
      
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
    
  })
  
  output$sist_cadiovasc <- renderHighchart({
    BASE_FINAL_V1 <- res_mod()
    
    AA<-BASE_FINAL_V1
    
    kk <- bind_cols(
      AA %>% 
        dplyr::filter(!is.na(Acidente_Vascular_Cerebral) & Acidente_Vascular_Cerebral == "sim") %>% 
        dplyr::summarise(Acidente_Vascular_Cerebral = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Febre_reumatica) & Febre_reumatica == "sim") %>% 
        dplyr::summarise(Febre_reumatica = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Insuficiencia_cardiaca_congestiva) & Insuficiencia_cardiaca_congestiva == "sim") %>% 
        dplyr::summarise(Insuficiencia_cardiaca_congestiva = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Endocardite) & Endocardite == "sim") %>% 
        dplyr::summarise(Endocardite = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Pericardite) & Pericardite == "sim") %>% 
        dplyr::summarise(Pericardite = n())
    )
    
    casos_cardiovascular<-data.frame(
      Sistema_Cardiovascular=c("Acidente Vascular Cerebral","Febre reumática","Insuficiência cardíaca congestiva","Endocardite","Pericardite"),
      Frequencia=c(kk$Acidente_Vascular_Cerebral,kk$Febre_reumatica,kk$Insuficiencia_cardiaca_congestiva,kk$Endocardite,kk$Pericardite)
    )
    
    # Criar o gráfico de colunas empilhadas
    Plot_casos_sist_cardiovascular <- hchart(
      casos_cardiovascular, 
      'bar', 
      hcaes(x = Sistema_Cardiovascular, y = Frequencia, group = Sistema_Cardiovascular),
      dataLabels = list(enabled = TRUE, style = list(fontSize = "11px")),
      stacking = "normal"
    ) %>%
      hc_colors(c( "#32A594","#EFC000FF", "#203d7d","#a0a0ed","#203d7e")) %>%
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
      hc_tooltip(crokkhairs = TRUE,
                 shared = TRUE, borderWidth = 5) %>%
      hc_title(
        text = "<b><i></i>Sistema Cardiovascular</b>",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)
      ) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
    
    
  })
  
  output$sist_gastr <- renderHighchart({
    
    BASE_FINAL_V1 <- res_mod()
    
    AA<-BASE_FINAL_V1
    
    dd <- bind_cols(
      AA %>% 
        dplyr::filter(!is.na(diarreia_aguda) & diarreia_aguda == "sim") %>% 
        dplyr::summarise(diarreia_aguda = n()),
      
      AA %>% 
        dplyr::filter(!is.na(diarreia_cronica) & diarreia_cronica == "sim") %>% 
        dplyr::summarise(diarreia_cronica = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Gastroenterite_aguda) & Gastroenterite_aguda == "sim") %>% 
        dplyr::summarise(Gastroenterite_aguda = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Peritonite) & Peritonite == "sim") %>% 
        dplyr::summarise(Peritonite = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Apendicite) & Apendicite == "sim") %>% 
        dplyr::summarise(Apendicite = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Colecistite) & Colecistite == "sim") %>% 
        dplyr::summarise(Colecistite = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Ascite) & Ascite == "sim") %>% 
        dplyr::summarise(Ascite = n())
    )
    
    casos_gastroIntestinal<-data.frame(
      Sistema_GastroIntestinal=c("Diarreia aguda","Diarreia Cronica","Gastroenterite aguda","Peritonite","Apendicite","Colecistite","Ascite"),
      Frequencia=c(dd$diarreia_aguda,dd$diarreia_cronica,dd$Gastroenterite_aguda,dd$Peritonite,dd$Apendicite,dd$Colecistite,dd$Ascite)
    )
    
    # Criar o gráfico de colunas empilhadas
    Plot_casos_sist_gastroIntestinal <- hchart(
      casos_gastroIntestinal, 
      'bar', 
      hcaes(x = Sistema_GastroIntestinal, y = Frequencia, group = Sistema_GastroIntestinal),
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
        text = "<b><i></i>Sistema Gastrointestinal</b>",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)
      ) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
    
  })
  
  output$sist_genitorian <- renderHighchart({
    BASE_FINAL_V1 <- res_mod()
    
    AA<-BASE_FINAL_V1
    
    dd <- bind_cols(
      AA %>% 
        dplyr::filter(!is.na(Infeccao_Urinaria) & Infeccao_Urinaria == "sim") %>% 
        dplyr::summarise(Infeccao_Urinaria = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Incontinencia_Urinaria) & Incontinencia_Urinaria == "sim") %>% 
        dplyr::summarise(Incontinencia_Urinaria = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Calculos_Renais) & Calculos_Renais == "sim") %>% 
        dplyr::summarise(Calculos_Renais = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Insuficiencia_Renal_Aguda) & Insuficiencia_Renal_Aguda == "sim") %>% 
        dplyr::summarise(Insuficiencia_Renal_Aguda = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Doenca_Renal_Cronica) & Doenca_Renal_Cronica == "sim") %>% 
        dplyr::summarise(Doenca_Renal_Cronica = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Doenca_Inflamatoria_Pelvica_DIP) & Doenca_Inflamatoria_Pelvica_DIP == "sim") %>% 
        dplyr::summarise(Doenca_Inflamatoria_Pelvica_DIP = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Abcesso_tubo_ovarico) & Abcesso_tubo_ovarico == "sim") %>% 
        dplyr::summarise(Abcesso_tubo_ovarico = n())
    )
    
    casos_Sistema_genitourinario<-data.frame(
      Sistema_Genitourinario=c("Infecção Urinária","Incontinência Urinária","Cálculos Renais","Insuficiência Renal Aguda","Doença Renal Crónica","Doença Inflamatória Pélvica (DIP)","Abcesso tubo-ovárico"),
      Frequencia=c(dd$Infeccao_Urinaria,dd$Incontinencia_Urinaria,dd$Calculos_Renais,dd$Insuficiencia_Renal_Aguda,dd$Doenca_Renal_Cronica,dd$Doenca_Inflamatoria_Pelvica_DIP,dd$Abcesso_tubo_ovarico)
    )
    
    # Criar o gráfico de colunas empilhadas
    Plot_casos_sist_Genitourinario <- hchart(
      casos_Sistema_genitourinario, 
      'bar', 
      hcaes(x = Sistema_Genitourinario, y = Frequencia, group = Sistema_Genitourinario),
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
        text = "<b><i></i>Sistema Genitourinário</b>",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)
      ) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
    
  })
  
  
  output$sis_pele_tecidos <- renderHighchart({
    BASE_FINAL_V1 <- res_mod()
    
    
    AA<-BASE_FINAL_V1
    
    dd <- bind_cols(
      AA %>% 
        dplyr::filter(!is.na(Dermatite) & Dermatite == "sim") %>% 
        dplyr::summarise(Dermatite = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Fascite_necrotizante) & Fascite_necrotizante == "sim") %>% 
        dplyr::summarise(Fascite_necrotizante = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Gangrena) & Gangrena == "sim") %>% 
        dplyr::summarise(Gangrena = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Sarcoma_de_Kaposi) & Sarcoma_de_Kaposi == "sim") %>% 
        dplyr::summarise(Sarcoma_de_Kaposi = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Piodermite_incl._Impetigo) & Piodermite_incl._Impetigo == "sim") %>% 
        dplyr::summarise(Piodermite_incl._Impetigo = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Erisipela_celulite) & Erisipela_celulite == "sim") %>% 
        dplyr::summarise(Erisipela_celulite = n()),
      
      AA %>% 
        dplyr::filter(!is.na(ulceras_de_pressao) & ulceras_de_pressao == "sim") %>% 
        dplyr::summarise(ulceras_de_pressao = n())
    )
    
    casos_Sistema_pele_tecidos<-data.frame(
      Sistema_pele_tecidos=c("Dermatite","Fascite necrotizante","Gangrena","Sarcoma de Kaposi","Piodermite incl. Impetigo","Erisipela/celulite","Úlceras de pressão"),
      Frequencia=c(dd$Dermatite,dd$Fascite_necrotizante,dd$Gangrena,dd$Sarcoma_de_Kaposi,dd$Piodermite_incl._Impetigo,dd$Erisipela_celulite,dd$ulceras_de_pressao)
    )
    
    # Criar o gráfico de colunas empilhadas
    Plot_casos_sist_peletecidos <- hchart(
      casos_Sistema_pele_tecidos, 
      'bar', 
      hcaes(x = Sistema_pele_tecidos, y = Frequencia, group = Sistema_pele_tecidos),
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
      hc_exporting(enabled = TRUE) %>%
      hc_legend(enabled = FALSE)
    
  })
  
  
  output$sis_osteomioa <- renderHighchart({
    
    BASE_FINAL_V1 <- res_mod()
    
    AA<-BASE_FINAL_V1
    
    kk <- bind_cols(
      AA %>% 
        dplyr::filter(!is.na(Artrite_septica) & Artrite_septica == "sim") %>% 
        dplyr::summarise(Artrite_septica = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Osteomielite) & Osteomielite == "sim") %>% 
        dplyr::summarise(Osteomielite = n()),
      
      AA %>% 
        dplyr::filter(!is.na(TB_ossea_Mal_de_Pott) & TB_ossea_Mal_de_Pott == "sim") %>% 
        dplyr::summarise(TB_ossea_Mal_de_Pott = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Miosite) & Miosite == "sim") %>% 
        dplyr::summarise(Miosite = n())
    )
    
    casos_osteomolite<-data.frame(
      Sistema_Osteomioarticular=c("Artrite séptica","Osteomielite","TB óssea (Mal de Pott)","Miosite"),
      Frequencia=c(kk$Artrite_septica,kk$Osteomielite,kk$TB_ossea_Mal_de_Pott,kk$Miosite)
    )
    
    # Criar o gráfico de colunas empilhadas
    Plot_casos_osteomolite <- hchart(
      casos_osteomolite, 
      'bar', 
      hcaes(x = Sistema_Osteomioarticular, y = Frequencia, group = Sistema_Osteomioarticular),
      dataLabels = list(enabled = TRUE, style = list(fontSize = "11px")),
      stacking = "normal"
    ) %>%
      hc_colors(c( "#32A594","#EFC000FF", "#203d7d","#a0a0ed","#203d7e")) %>%
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
      hc_tooltip(crokkhairs = TRUE,
                 shared = TRUE, borderWidth = 5) %>%
      hc_title(
        text = "<b><i></i>Sistema Osteomioarticular</b>",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)
      ) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
    
  })
  
  output$sis_otorrino <- renderHighchart({
    BASE_FINAL_V1 <- res_mod()
    
    AA<-BASE_FINAL_V1
    
    kk <- bind_cols(
      AA %>% 
        dplyr::filter(!is.na(Sinusite) & Sinusite == "sim") %>% 
        dplyr::summarise(Sinusite = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Rinite) & Rinite == "sim") %>% 
        dplyr::summarise(Rinite = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Amigdalite) & Amigdalite == "sim") %>% 
        dplyr::summarise(Amigdalite = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Laringite) & Laringite == "sim") %>% 
        dplyr::summarise(Laringite = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Faringite) & Faringite == "sim") %>% 
        dplyr::summarise(Faringite = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Dor_na_Garganta) & Dor_na_Garganta == "sim") %>% 
        dplyr::summarise(Dor_na_Garganta = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Otite) & Otite == "sim") %>% 
        dplyr::summarise(Otite = n()),
      
      AA %>% 
        dplyr::filter(!is.na(Vertigem) & Vertigem == "sim") %>% 
        dplyr::summarise(Vertigem = n())
    )
    
    casos_otorrinol<-data.frame(
      Sistema_otorrino=c("Sinusite","Rinite","Amigdalite","Laringite","Faringite","Dor na Garganta","Otite","Vertigem"),
      Frequencia=c(kk$Sinusite,kk$Rinite,kk$Amigdalite,kk$Laringite,kk$Faringite,kk$Dor_na_Garganta,kk$Otite,kk$Vertigem)
    )
    
    # Criar o gráfico de colunas empilhadas
    Plot_casos_otorrinologolog <- hchart(
      casos_otorrinol, 
      'bar', 
      hcaes(x = Sistema_otorrino, y = Frequencia, group = Sistema_otorrino),
      dataLabels = list(enabled = TRUE, style = list(fontSize = "11px")),
      stacking = "normal"
    ) %>%
      hc_colors(c( "#32A594","#EFC000FF", "#203d7d","#a0a0ed","#203d7e")) %>%
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
      hc_tooltip(crokkhairs = TRUE,
                 shared = TRUE, borderWidth = 5) %>%
      hc_title(
        text = "<b><i></i>Otorrinolaringologia</b>",
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)
      ) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
  })
  
  output$condic_clinicas <- renderHighchart({
    
    BASE_FINAL_V1 <- res_mod()
    
    AA<-BASE_FINAL_V1
    
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
        text = "<b><i></i>condições clínicas associadas</b>",#alterar cor
        margin = 20,
        align = "left",
        style = list(color = "#22A884", useHTML = TRUE)
      ) %>%
      hc_add_theme(hc_theme_google()) %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_legend(enabled = FALSE)
    
    
  })
  
  
  
  output$desfecho_plot <- renderHighchart({
    
    
  })
  
}



# Run the application 
shinyApp(ui, server)
