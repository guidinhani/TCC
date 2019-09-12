# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
# ==============================================================================
# BIBLIOTECAS
# ==============================================================================
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(stringr)
library(dashboardthemes)
# ==============================================================================
# LEITURA DOS DADOS PADRONIZADOS
# ==============================================================================
estudos <- read.csv(file = "EstudosPadronizado.csv", header = TRUE, stringsAsFactors = FALSE)
empregos <- read.csv(file = "EmpregosPadronizado.csv", header = TRUE, stringsAsFactors = FALSE)
# ==============================================================================
# LEITURA DAS FUNÇÕES
# ==============================================================================
# GERAL
source("Funcoes/Geral/EgressosADS.r", encoding = "UTF-8")
source("Funcoes/Geral/EgressosGPI.r", encoding = "UTF-8")
source("Funcoes/Geral/QuantidadeEgressosEstudos.r", encoding = "UTF-8")
source("Funcoes/Geral/QuantidadeEgressosProfissional.r", encoding = "UTF-8")
source("Funcoes/Geral/QuantidadeEmpresasAtuais.r", encoding = "UTF-8")
source("Funcoes/Geral/QuantidadePerfisEgressos.r", encoding = "UTF-8")
source("Funcoes/Geral/TotalDeEgressos.r", encoding = "UTF-8")
# TEMA DA DASHBOARD
source("Funcoes/TemaDashboard/TemaDashboard.r", encoding = "UTF-8")
# CONSULTAS/GRÁFICOS ACADÊMICOS
source("Funcoes/Consultas/Academico/ConsultaCursosAntesGraduacao.r", encoding = "UTF-8")
source("Funcoes/Graficos/Academico/GraficoCursosAntesGraduacao.r", encoding = "UTF-8")
source("Funcoes/Consultas/Academico/ConsultaAnoDeEgresso.r", encoding = "UTF-8")
source("Funcoes/Graficos/Academico/GraficoAnoDeEgresso.r", encoding = "UTF-8")
source("Funcoes/Consultas/Academico/ConsultaProsseguiuEstudos.r", encoding = "UTF-8")
source("Funcoes/Graficos/Academico/GraficoProsseguiuEstudos.r", encoding = "UTF-8")
source("Funcoes/Consultas/Academico/ConsultaAreasCursosProsseguidos.r", encoding = "UTF-8")
source("Funcoes/Graficos/Academico/GraficoAreasCursosProsseguidos.r", encoding = "UTF-8")
source("Funcoes/Consultas/Academico/ConsultaCursosProsseguidos.r", encoding = "UTF-8")
source("Funcoes/Graficos/Academico/GraficoCursosProsseguidos.r", encoding = "UTF-8")
# CONSULTAS/GRÁFICOS PROFISSIONAIS
source("Funcoes/Consultas/Profissional/ConsultaCargosAtuais.r", encoding = "UTF-8")
source("Funcoes/Graficos/Profissional/GraficoCargosAtuais.r", encoding = "UTF-8")
source("Funcoes/Consultas/Profissional/ConsultaLocalizacaoEmpresas.r", encoding = "UTF-8")
source("Funcoes/Graficos/Profissional/GraficoLocalizacaoEmpresas.r", encoding = "UTF-8")
source("Funcoes/Consultas/Profissional/ConsultaNomeEmpresaAtual.r", encoding = "UTF-8")
source("Funcoes/Graficos/Profissional/GraficoNomeEmpresaAtual.r", encoding = "UTF-8")
source("Funcoes/Consultas/Profissional/ConsultaTipoEmpresa.r", encoding = "UTF-8")
source("Funcoes/Graficos/Profissional/GraficoTipoEmpresa.r", encoding = "UTF-8")
source("Funcoes/Consultas/Profissional/ConsultaAreasFormacaoEmprego.r", encoding = "UTF-8")
source("Funcoes/Graficos/Profissional/GraficoAreaAtualEmprego.r", encoding = "UTF-8")
source("Funcoes/Graficos/Profissional/GraficoTrabalhaAreaFormacao.r", encoding = "UTF-8")
# ==============================================================================
# DASHBOARD
# ==============================================================================
# INTERFACE DO USUÁRIO
ui <- dashboardPage(
  # CABEÇALHO
  dashboardHeader(
    title = "Egressos IFSP",
    # CURSOS/MENU SUPERIOR
    tags$li(
      class = "dropdown",
      radioButtons("id_curso", "",
        c("ANÁLISE E DESENVOLVIMENTO DE SISTEMAS" = "id_ads", "GESTÃO DA PRODUÇÃO INDUSTRIAL" = "id_gpi"),
        inline = TRUE
      )
    ),
    # FAVICON DO IFSP
    tags$li(
      class = "dropdown link-ifsp",
      tags$a(
        href = "http://slt.ifsp.edu.br/portal/", target = "_blank",
        tags$img(height = "30px", alt = "IFSP Logo", src = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/15/Logotipo_IFET.svg/220px-Logotipo_IFET.svg.png")
      )
    )
  ),
  # TIPOS DE DADOS/MENU LATERAL ESQUERDO
  dashboardSidebar(
    sidebarMenu(
      menuItem("DADOS ACADÊMICOS", tabName = "estudos", icon = icon("graduation-cap")),
      menuItem("DADOS PROFISSIONAIS", tabName = "empregos", icon = icon("briefcase"))
    )
  ),
  # CORPO DA DASHBOARD
  dashboardBody(
    tags$head(tags$link(rel = "shortcut icon", href = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/15/Logotipo_IFET.svg/220px-Logotipo_IFET.svg.png")),
    tags$style(".sidebar-toggle{display:none !important}"),
    tags$style(".navbar-custom-menu, .main-header .navbar-right {float: none !important; display: flex !important; justify-content:space-around !important;}"),
    tags$style(".link-ifsp{position:absolute !important; right:0 !important;}"),
    tema_dashboard,
    tabItems(
      # DASHBOARD DE DADOS ACADÊMICOS
      tabItem(
        tabName = "estudos",
        fluidRow(
          # QUANTIDADE DE EGRESSOS COM PERFIL NO LINKEDIN
          valueBoxOutput("quantidade_perfis_egressos"),
          # QUANTIDADE DE EGRESSOS NA PARTE ACADÊMICA
          valueBoxOutput("quantidade_egressos_estudos")
        ),
        # GRÁFICOS
        fluidRow(
          box(title = "Cursos concluídos antes da graduação", status = "primary", solidHeader = T, plotOutput("cursos_antes_graduacao"), width = 6, collapsible = TRUE),
          box(title = "Ano de egresso do curso de graduação", status = "primary", solidHeader = T, plotlyOutput("ano_de_egresso"), width = 6, collapsible = TRUE),
          box(title = "Prosseguiu nos estudos após a graduação", status = "primary", solidHeader = T, plotlyOutput("prosseguiu_estudos"), width = 6, collapsible = TRUE),
          box(title = "Áreas dos cursos prosseguidos após a graduação", status = "primary", solidHeader = T, plotlyOutput("areas_cursos_prosseguidos"), width = 6, collapsible = TRUE),
          box(title = "Cursos prosseguidos após a graduação", status = "primary", solidHeader = T, plotlyOutput("cursos_prosseguidos"), width = 12, collapsible = TRUE)
        )
      ),
      # DASHBOARD DE DADOS PROFISSIONAIS
      tabItem(
        tabName = "empregos",
        fluidRow(
          valueBoxOutput("quantidade_egressos_profissional"),
          valueBoxOutput("quantidade_empresas_atuais")
        ),
        # GRÁFICOS
        fluidRow(
          box(title = "Egressos que trabalham na área de formação", status = "primary", solidHeader = T, plotlyOutput("trabalha_area_formacao"), width = 6, collapsible = TRUE),
          box(title = "Áreas em que os egressos trabalham", status = "primary", solidHeader = T, plotlyOutput("area_atual_emprego"), width = 6, collapsible = TRUE),
          box(title = "Classificação das empresas onde os egressos trabalham", status = "primary", solidHeader = T, plotlyOutput("tipo_da_empresa", height = 600), width = 6, collapsible = TRUE),
          box(title = "Empresas onde os egressos trabalham", status = "primary", solidHeader = T, plotlyOutput("nome_empresa_atual", height = 600), width = 6, collapsible = TRUE),
          box(title = "Cidades onde os egressos trabalham", status = "primary", solidHeader = T, plotlyOutput("localizacao_empresas", height = 600), width = 6, collapsible = TRUE),
          box(title = "Atuais cargos de trabalho", status = "primary", solidHeader = T, plotlyOutput("cargos_atuais", height = 600), width = 6, collapsible = TRUE)
        )
      )
    )
  )
)

# ==============================================================================
# SERVIDOR
# ==============================================================================
# ==============================================================================
# CONSULTAS ACADÊMICAS
# ==============================================================================
server <- function(input, output) {
  # ==============================================================================
  # VARIÁVEIS GLOBAIS PARA ARGUMETNOS
  # ==============================================================================
  egressos <- reactive(switch(input$id_curso,
    id_ads = EgressosADS(),
    id_gpi = EgressosGPI()
  ))


  flag <- reactive(switch(input$id_curso,
    id_ads = "ehADS",
    id_gpi = "ehGPI"
  ))


  sigla_curso <- reactive(switch(input$id_curso,
    id_ads = "ADS",
    id_gpi = "GPI"
  ))

  # ==============================================================================
  # CONSULTAS ACADÊMICAS
  # ==============================================================================
  # QUANTIDADE DE PERFIS ENCONTRADOS NO LINKEDIN
  output$quantidade_perfis_egressos <- renderValueBox({
    valueBox(
      paste0(QuantidadePerfisEgressos(sigla_curso()), " de ", TotalDeEgressos(sigla_curso())), "Perfis encontrados no LinkedIn",
      icon = icon("address-book"),
      color = "light-blue"
    )
  })
  
  # QUANTIDADE DE EGRESSOS COM DADOS ACADÊMICOS
  output$quantidade_egressos_estudos <- renderValueBox({
    valueBox(
      paste0(QuantidadeEgressosEstudos(flag()), " de ", TotalDeEgressos(sigla_curso())), "Perfis com dados acadêmicos",
      icon = icon("address-book"),
      color = "light-blue"
    )
  })

  # CURSOS CONCLUÍDOS ANTES DA GRADUAÇÃO
  output$cursos_antes_graduacao <- renderPlot({
    dataframe <- ConsultaCursosAntesGraduacao(egressos(), flag(), sigla_curso())
    GraficoCursosAntesGraduacao(dataframe)
  })

  # ANO DE EGRESSO DOS CURSOS
  output$ano_de_egresso <- renderPlotly({
    dataframe <- ConsultaAnoDeEgresso(egressos(), sigla_curso())
    GraficoAnoDeEgresso(dataframe)
  })

  # MOSTRA SE O EGRESSO FEZ MAIS ALGUM CURSO APÓS A GRADUAÇÃO
  output$prosseguiu_estudos <- renderPlotly({
    dataframe <- ConsultaProsseguiuEstudos(egressos(), flag())
    GraficoProsseguiuEstudos(dataframe)
  })

  # QUAIS AS ÁREAS DOS CURSOS O EGRESSO FEZ APÓS A GRADUAÇÃO
  output$areas_cursos_prosseguidos <- renderPlotly({
    dataframe <- ConsultaAreasCursosProsseguidos(egressos(), flag())
    GraficoAreasCursosProsseguidos(dataframe)
  })

  # QUAIS CURSOS O EGRESSO FEZ APÓS A GRADUAÇÃO
  output$cursos_prosseguidos <- renderPlotly({
    dataframe <- ConsultaCursosProsseguidos(egressos(), flag())
    GraficoCursosProsseguidos(dataframe)
  })

  # ==============================================================================
  # CONSULTAS PROFISSIONAIS
  # ==============================================================================
  # QUANTIDADE DE EGRESSOS NA PARTE PROFISSIONAL
  output$quantidade_egressos_profissional <- renderValueBox({
    valueBox(
      paste0(QuantidadeEgressosProfissional(egressos()), " de ", TotalDeEgressos(sigla_curso())), "Perfis com dados profissionais",
      icon = icon("address-book"),
      color = "light-blue"
    )
  })

  # QUANTIDADE DE EMPRESAS QUE OS EGRESOS TRABALHAMA ATUALMENTE
  output$quantidade_empresas_atuais <- renderValueBox({
    valueBox(
      paste0(QuantidadeEmpresasAtuais(egressos()), " empresas"), "Empregam os egressos",
      icon = icon("building"),
      color = "light-blue"
    )
  })

  # MOSTRA SE O EGRESSO TRABALHA NA ÁREA DE FORMAÇÃO DO CURSO DE GRADUAÇÃO
  output$trabalha_area_formacao <- renderPlotly({
    dataframe <- ConsultaAreasFormacaoEmprego(egressos(), sigla_curso())
    GraficoTrabalhaAreaFormacao(dataframe)
  })

  # QUAL ÁREA O EGRESSO TRABALHA NO ÚLTIMO/ATUAL EMPREGO
  output$area_atual_emprego <- renderPlotly({
    dataframe <- ConsultaAreasFormacaoEmprego(egressos(), sigla_curso())
    GraficoAreaAtualEmprego(dataframe)
  })

  # MOSTRA SE A EMPRESA ATUAL DE TRABALHO É PÚBLICA OU PRIVADA
  output$tipo_da_empresa <- renderPlotly({
    dataframe <- ConsultaTipoEmpresa(egressos())
    GraficoTipoEmpresa(dataframe)
  })

  # NOME DA EMPRESA ATUAL DE TRABALHO
  output$nome_empresa_atual <- renderPlotly({
    dataframe <- ConsultaNomeEmpresaAtual(egressos())
    GraficoNomeEmpresaAtual(dataframe)
  })

  # LOCALIZAÇÃO DA EMPRESA NO ÚLTIMO/ATUAL EMPREGO
  output$localizacao_empresas <- renderPlotly({
    dataframe <- ConsultaLocalizacaoEmpresas(egressos())
    GraficoLocalizacaoEmpresas(dataframe)
  })

  # ATUAL CARGO NA EMPRESA NO ÚLTIMO/ATUAL EMPREGO
  output$cargos_atuais <- renderPlotly({
    dataframe <- ConsultaCargosAtuais(egressos())
    GraficoCargosAtuais(dataframe)
  })
}
# ==============================================================================
# EXECUÇÃO
# ==============================================================================
tryCatch({
  if (!exists("estudos")) {
    stop("Objeto estudos não encontrado.")
  }
  if (ncol(estudos)!=9) {
    stop("O arquivo de estudos não segue os padrões.")
  }
  if (!exists("empregos")) {
    stop("Objeto empregos não encontrado.")
  }
  if (ncol(empregos)!=7) {
    stop("O arquivo de empregos não segue os padrões.")
  }
  runApp(shinyApp(ui, server, options = list(port = 8086)))
})
