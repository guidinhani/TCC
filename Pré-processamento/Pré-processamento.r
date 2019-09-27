# ============================================================================== #
#               ALGORITMO DESENVOLVIDO POR GUILHERME DINHANI                     #
# ============================================================================== #
# ==============================================================================
# BIBLIOTECAS
# ==============================================================================
library(dplyr)
library(stringr)
# ==============================================================================
# LEITURA E PRÉ-PROCESSAMENTO
# ==============================================================================
# ==============================================================================
# PADRONIZAÇÃO EMPREGOS
# ==============================================================================
empregos <- read.csv(file = "Empregos.csv", header = TRUE) %>%
  # ==============================================================================
  # REMOÇÃO DE ACENTOS
  # ==============================================================================
  mutate(
    Cargo = iconv(Cargo, to = "ASCII//TRANSLIT"),
    Cidade = iconv(Cidade, to = "ASCII//TRANSLIT"),
    Empresa = iconv(Empresa, to = "ASCII//TRANSLIT"),
    Nome = iconv(Nome, to = "ASCII//TRANSLIT")
  ) %>%
  # ==============================================================================
  # LETRAS MAIÚSCULAS
  # ==============================================================================
  mutate_all(str_to_upper) %>%
  mutate(
    # ==============================================================================
    # REMOÇÃO DE TEXTOS INDESEJADOS
    # ==============================================================================
    # CARGO
    Cargo = str_replace_all(Cargo, "/| & ", " "),
    Cargo = str_replace_all(Cargo, "-|,", ""),
    Cargo = str_replace_all(Cargo, "JR.$|JR$|JUNIOR$|^JUNIOR|
                            |PL.$|PLENO$|PLENO I|
                            |SENIOR$|SN.$|
                            |\\(DEV1\\)", ""),
    # CIDADE
    Cidade = str_replace_all(Cidade, "-|/", ""),
    Cidade = str_replace_all(Cidade, ", |/.*|, |/.*", " "),
    Cidade = str_replace_all(Cidade, "AREA, BRAZIL|AREA BRAZIL|AREA|BRAZIL|SP|
                             |E REGIAO.*|
                             |POLO SHOPPING|
                             |,.+(BRASIL|CAMPINAS)", ""),
    Cidade = str_replace_all(Cidade, ".*SALTO.*", "SALTO"),
    Cidade = str_replace_all(Cidade, "(.+)(SAO PAULO)", "\\1"),
    # EMPRESA
    Empresa = str_replace_all(Empresa, "\\.$| - ", " "),
    Empresa = str_replace_all(Empresa, ",|
                              |/SP|
                              |S\\.A|S\\.A\\.|
                              |\\(IFSP\\)", ""),
    # NOME
    Nome = str_replace_all(Nome, "\"", " "),
    Nome = str_replace_all(Nome, "PROFESSORA|INFORMATICA", ""),

    # ==============================================================================
    # CORREÇÃO DE PALAVRAS INCORRETAS
    # ==============================================================================
    Cargo = str_replace_all(Cargo, "ESTAGIARIOA", "ESTAGIARIO"),
    Cargo = str_replace_all(Cargo, "EGENHARIA", "ENGENHARIA"),
    Cargo = str_replace_all(Cargo, "ADMINSITRATIVO", "ADMINISTRATIVO"),
    Cargo = str_replace_all(Cargo, "LIGISTICA", "LOGISTICA"),

    # ==============================================================================
    # EMPRESAS
    # ==============================================================================
    # A
    Empresa = str_replace_all(Empresa, ".*ACOKORTE.*", "ACOKORTE"),
    Empresa = str_replace_all(Empresa, ".*ALLPET.*", "ALLPET"),
    # B
    Empresa = str_replace_all(Empresa, ".*BANCO CARREFOUR.*", "BANCO CARREFOUR"),
    Empresa = str_replace_all(Empresa, ".*BENTELER.*", "BENTELER"),
    Empresa = str_replace_all(Empresa, ".*BIRO.*", "BIRO 2000"),
    Empresa = str_replace_all(Empresa, ".*BRASSUCO.*", "BRASSUCO"),
    # C
    Empresa = str_replace_all(Empresa, ".*CASA DO PRODUTOR RURAL.*", "CASA DO PRODUTOR RURAL"),
    Empresa = str_replace_all(Empresa, ".*CENTRO UNIVERSITARIO NOSSA SENHORA DO PATROCINIO.*", "CEUNSP"),
    Empresa = str_replace_all(Empresa, ".*CHARLES CAMBUR.*", "CHARLES CAMBUR"),
    Empresa = str_replace_all(Empresa, ".*COMET LEMASA.*", "COMET LEMASA"),
    # D
    Empresa = str_replace_all(Empresa, ".*DENTAL MORELLI.*", "DENTAL MORELLI"),
    # E
    Empresa = str_replace_all(Empresa, ".*E-DEPLOY.*", "E-DEPLOY"),
    Empresa = str_replace_all(Empresa, ".*EMFILS.*", "EMFILS"),
    Empresa = str_replace_all(Empresa, ".*ESPECIFER.*", "ESPECIFER"),
    # F
    Empresa = str_replace_all(Empresa, ".*FIDELITY.*", "FIDELITY"),
    Empresa = str_replace_all(Empresa, ".*FREELANCER.*", "FREELANCER"),
    # G
    Empresa = str_replace_all(Empresa, ".*GRANOVA.*", "GRANOVA PRATA"),
    Empresa = str_replace_all(Empresa, ".*GEPOL.*|.*GRUPO DE ESTUDOS E PRATICAS EM OLERICULTURA.*", "GEPOL"),
    Empresa = str_replace_all(Empresa, ".*GUARANY.*", "GUARANY"),
    Empresa = str_replace_all(Empresa, ".*GUHRING.*", "GUHRING"),
    # H
    Empresa = str_replace_all(Empresa, ".*HELPAY GRUPO.*", "HELPAY"),
    # I
    Empresa = str_replace_all(Empresa, ".*IFSP", paste("IFSP", Cidade)),
    Empresa = str_replace_all(Empresa, ".*IFSP.+INSTITUTO FEDERAL.*|.*SAO PAULO FEDERAL INSTITUTE.*", paste("IFSP", Cidade)),
    Empresa = str_replace_all(Empresa, "^INSTITUTO FEDERAL.+SAO PAULO", paste("IFSP", Cidade)),
    Empresa = str_replace_all(Empresa, ".*ISS INTEGRATED FACILITY SERVICES.*", "ISS INTEGRATED FACILITY SERVICES"),
    Empresa = str_replace_all(Empresa, ".*ICMC.*", "ICMC"),
    # L
    Empresa = str_replace_all(Empresa, ".*LOJAS AMERICANAS.*", "LOJAS AMERICANAS"),
    Empresa = str_replace_all(Empresa, ".*LOJAS CEM.*", "LOJAS CEM"),
    # N
    Empresa = str_replace_all(Empresa, ".*NISSIN FOODS.*", "NISSIN FOODS"),
    # P
    Empresa = str_replace_all(Empresa, "PREFEITURA DA ESTANCIA TURISTICA DE SALTO|PREFEITURA DE SALTO SP", "PREFEITURA DE SALTO"),
    Empresa = str_replace_all(Empresa, ".*PREFEITURA.+SALTO", "PREFEITURA DE SALTO"),
    Empresa = str_replace_all(Empresa, ".*PRINT ONE.*", "PRINT ONE"),
    Empresa = str_replace_all(Empresa, ".*PUROTEK.*", "PUROTEK"),
    # S
    Empresa = str_replace_all(Empresa, ".*SABOO INDUSTRIA.*", "SABOO INDUSTRIA"),
    Empresa = str_replace_all(Empresa, ".*SEW.+EURODRIVE.*", "SEW-EURODRIVE"),
    Empresa = str_replace_all(Empresa, ".*SIMBAL.*", "SIMBAL"),
    Empresa = str_replace_all(Empresa, ".*SIS.*", "SIS"),
    Empresa = str_replace_all(Empresa, ".*STEFANINI.*", "STEFANINI"),
    # T
    Empresa = str_replace_all(Empresa, ".*TAXIVIRACOPOS.*", "TAXI VIRACOPOS"),
    # U
    Empresa = str_replace_all(Empresa, ".*UPL.*", "UPL"),
    # V
    Empresa = str_replace_all(Empresa, ".*VENKO MOTORS.*", "VENKO MOTORS"),

    # ==============================================================================
    # CARGOS
    # ==============================================================================
    # A
    Cargo = str_replace_all(Cargo, ".*INICIACAO CIENTIFICA.*|.*SCIENTIFIC INITIATION.*", "ALUNO DE INICIACAO CIENTIFICA"),
    Cargo = str_replace_all(Cargo, ".*CONTROLLING ANALYST.*", "ANALISTA DE CONTROLE"),
    Cargo = str_replace_all(Cargo, ".*FULFILLMENT.+ANALYST.*", "ANALISTA DE CUMPRIMENTO"),
    Cargo = str_replace_all(Cargo, ".*FINANCIAL ANALYST COST.*", "ANALISTA DE CUSTO FINANCEIRO"),
    Cargo = str_replace_all(Cargo, ".*FRAUD ANALYST.*|.*FRAUD RISK.*", "ANALISTA DE FRAUDE"),
    Cargo = str_replace_all(Cargo, ".*SOCIAL MEDIA ANALYST.*", "ANALISTA DE MIDIA SOCIAL"),
    Cargo = str_replace_all(Cargo, ".*ANALISTA DE LOGISTICA.*", "ANALISTA DE LOGISTICA"),
    Cargo = str_replace_all(Cargo, ".*FINANCIAL PLANNING ANALYST.*", "ANALISTA DE PLANEJAMENTO FINANCEIRO"),
    Cargo = str_replace_all(Cargo, ".*PROCESS ANALYST.*", "ANALISTA DE PROCESSOS"),
    Cargo = str_replace_all(Cargo, ".*ANALISTA DE SISTEMAS.*|.*SYSTEM ANALYST.*", "ANALISTA DE SISTEMAS"),
    Cargo = str_replace_all(Cargo, ".*ANALISTA DE SUPORTE.*", "ANALISTA DE SUPORTE"),
    Cargo = str_replace_all(Cargo, ".*ANALISTA DE TESTE.*", "ANALISTA DE TESTE"),
    Cargo = str_replace_all(Cargo, ".*ASSISTENTE ADMINISTRATIVO.*|.*AUX ADM.*|.*AUX.+ADM.*", "ASSISTENTE ADMINISTRATIVO"),
    Cargo = str_replace_all(Cargo, ".*ASSISTENTE DE PLANEJAMENTO.*", "ASSISTENTE DE PLANEJAMENTO"),
    Cargo = str_replace_all(Cargo, ".*PROCESS ASSISTANT.*", "ASSISTENTE DE PROCESSOS"),
    Cargo = str_replace_all(Cargo, ".*SUPPORT ASSISTANT.*", "ASSISTENTE DE SUPORTE"),
    Cargo = str_replace_all(Cargo, ".*ASSISTENTE TECNICO.*", "ASSISTENTE TECNICO"),
    Cargo = str_replace_all(Cargo, ".*SYSTEM AUDITS.*", "AUDITOR DE SISTEMA"),
    # B
    Cargo = str_replace_all(Cargo, ".*BIOMEDICO.*", "BIOMEDICO"),
    # C
    Cargo = str_replace_all(Cargo, ".*TAPERA.*", "CAPITA DA EQUIPE TAPERA BABY"),
    Cargo = str_replace_all(Cargo, ".*HEAD OF NEW BUSINESS.*", "CHEFE DE NOVOS NEGOCIOS"),
    Cargo = str_replace_all(Cargo, ".*COMPRADOR.*", "COMPRADOR"),
    Cargo = str_replace_all(Cargo, ".*CONSULTOR.+(AVON|NATURA)", "CONSULTOR DE VENDAS"),
    Cargo = str_replace_all(Cargo, ".*WAREHOUSE CONTROLLER.*", "CONTROLADOR DE ARMAZEM"),
    Cargo = str_replace_all(Cargo, ".*SYSTEM DEVELOPMENT COORDINATOR.*", "COORDENADOR DE DESENVOLVIMENTO DE SISTEMAS"),
    # D
    Cargo = str_replace_all(Cargo, ".*ANALISTA/DESENVOLVEDOR.*|.*PROGRAMMER.*|PROGRAMADORA ANALISTA|^PROGRAMADOR$|
                            |PROGRAMADOR DE SISTEMAS|.*DESENVOLVEDOR DE SISTEMAS.*|.*SOFTWARE DEVELOPER.*|
                            |.*MOBILE DEVELOPER.*", "DESENVOLVEDOR DE SOFTWARE"),
    Cargo = str_replace_all(Cargo, ".*DESENVOLVEDOR JAVA.*|.*JAVA DEVELOPER.*|.*PROGRAMADOR JAVA.*|.*JAVA.*", "DESENVOLVEDOR JAVA"),
    Cargo = str_replace_all(Cargo, ".*SHAREPOINT DEVELOPER.*", "DESENVOLVEDOR SHAREPOINT"),
    Cargo = str_replace_all(Cargo, ".*DESENVOLVEDOR DE WEB.*|PROGRAMADOR WEB", "DESENVOLVEDOR WEB"),
    Cargo = str_replace_all(Cargo, ".*DESIGNER GRAFICO.*", "DESIGNER GRAFICO"),
    Cargo = str_replace_all(Cargo, ".*UI UX DESIGNER.*", "DESIGN DE UI"),
    Cargo = str_replace_all(Cargo, ".*DIAGRAMADOR.*", "DIAGRAMADOR"),
    Cargo = str_replace_all(Cargo, ".*DOCUMENTACAO.*", "DOCUMENTADOR"),
    # E
    Cargo = str_replace_all(Cargo, "^INTERNSHIP$|.*ESTAGIO.*|.*ESTAGIARIA.*", "ESTAGIARIO"),
    Cargo = str_replace_all(Cargo, ".*ADMINISTRATIVE INTERN.*", "ESTAGIARIO ADMINISTRATIVO"),
    Cargo = str_replace_all(Cargo, ".*INTERNENVIRONMENT.*", "ESTAGIARIO DE AMBIENTE"),
    Cargo = str_replace_all(Cargo, ".*PROCESS ENGINEER INTERNSHIP.*", "ESTAGIARIO DE ENGENHARIA DE PROCESSOS"),
    Cargo = str_replace_all(Cargo, ".*FINANCIAL INTERN.*", "ESTAGIARIO DE FINANCAS"),
    Cargo = str_replace_all(Cargo, ".*QUALITY ASSURANCE INTERN.*", "ESTAGIARIO DE GARANTIA DE QUALIDADE"),
    Cargo = str_replace_all(Cargo, ".*INTERNACIONAL LOGISTICS INTERN.*", "ESTAGIARIO DE LOGISTICA INTERNACIONAL"),
    Cargo = str_replace_all(Cargo, ".*DATA CENTER OPERATIONS INTERN.*", "ESTAGIARIO DE OPERACOES DO DATA CENTER"),
    Cargo = str_replace_all(Cargo, "ESTAGIARIO DE TECNOLOGIA DA INFORMACAO|ESTAGIARIO DESENVOLVEDOR WEB|IT TRAINEE|
                                  |ESTAGIARIO\\(PROGRAMADOR\\)|ESTAGIO DE PROGRAMACAO|ESTAGIO EM ANALISE E DESENVOLVIMENTO DE SISTEMAS|
                                  |.*IT INTERN.*", "ESTAGIARIO DE TI"),
    Cargo = str_replace_all(Cargo, ".*CUSTOMER QUALITY ENGINEER.*|.*ENGENHEIRO DE QUALIDADE.*", "ENGENHEIRO DE QUALIDADE"),
    Cargo = str_replace_all(Cargo, ".*SOFTWARE ENGINEER.*|.*QA AUTOMATION INTERN.*", "ENGENHEIRO DE SOFTWARE"),
    Cargo = str_replace_all(Cargo, ".*MECHANICAL TRAINEE.*", "ESTAGIARIO MECANICO"),
    Cargo = str_replace_all(Cargo, ".*ENGENHARIA DE PROCESSOS.*", "ENGENHARIA DE PROCESSOS"),
    Cargo = str_replace_all(Cargo, ".*SUPPORT ENGINEER.*", "ENGENHEIRO DE SUPORTE"),
    Cargo = str_replace_all(Cargo, ".*IT SPECIALIST.*", "ESPECIALISTA EM TI"),
    # F
    Cargo = str_replace_all(Cargo, ".*TOOLMAKER.*|.*TOOL MAKER.*", "FABRICANTE DE FERRAMENTAS"),
    Cargo = str_replace_all(Cargo, ".*FREELANCER.*|.*FREE LANCER.*", "FREELANCER"),
    # G
    Cargo = str_replace_all(Cargo, ".*PROJECT MANAGEMENT OEM.*", "GESTOR DE PROJETOS OEM"),
    Cargo = str_replace_all(Cargo, ".*RACIO PROJECT MANAGEMENT.*", "GESTOR DE PROJETOS RACIO"),
    # I
    Cargo = str_replace_all(Cargo, ".*GESTAO DA QUALIDADE.*|.*INSPETOR DE QUALIDADE.*|.*INSPETOR DE CONTROLE DE QUALIDADE.*|.*QUALITY CONTROL.*|
                            |.*QUALITY ASSURANCE ANALYST.*|.*CONTROLE DE QUALIDADE.*", "INSPETOR DE QUALIDADE"),
    Cargo = str_replace_all(Cargo, ".*INSTRUTOR DE CODIGO.*|.*CODE INSTRUCTOR.*", "INSTRUTOR DE CODIGO"),
    # L
    Cargo = str_replace_all(Cargo, ".*LIDER DE EQUIPE.*|.*LIDER DE GRUPO.*", "LIDER DE EQUIPE"),
    Cargo = str_replace_all(Cargo, ".*LEAD GAME DESIGENR.*", "LIDER DE DESIGN"),
    # M
    Cargo = str_replace_all(Cargo, ".*MECANICO DE MANUTENCAO.*", "MECANICO DE MANUTENCAO"),
    Cargo = str_replace_all(Cargo, ".*MECANICO DE REFRIGERACAO.*", "MECANICO DE REFRIGERACAO"),
    # O
    Cargo = str_replace_all(Cargo, ".*WIRE EDM OPERATOR.*", "OPERADOR DE ELETROEROSAO"),
    Cargo = str_replace_all(Cargo, ".*OPERADOR DE MAQUINA.*", "OPERADOR DE MAQUINA"),
    Cargo = str_replace_all(Cargo, ".*OPERADOR DE PRODUCAO.*", "OPERADOR DE PRODUCAO"),
    # P
    Cargo = str_replace_all(Cargo, ".*RESEARCH SCIENTIST.*|.*RESEARCHER SCIENTIST.*", "PESQUISADOR CIENTIFICO"),
    Cargo = str_replace_all(Cargo, ".*PROFESSOR.*", "PROFESSOR"),
    Cargo = str_replace_all(Cargo, ".*PROGRAMADOR.+(CNC.*|CONTROLE NUMERICO.*)", "PROGRAMADOR CNC"),
    Cargo = str_replace_all(Cargo, ".*TI GRADUATE PROGRAM.*", "PROGRAMA DE GRADUACAO EM TI"),
    # S
    Cargo = str_replace_all(Cargo, ".*SHIFT SUPERVISOR*", "SUPERVISOR DE TURNO"),
    Cargo = str_replace_all(Cargo, "^SUPORTE.*", "SUPORTE"),
    Cargo = str_replace_all(Cargo, "IT SUPPORT.*", "SUPORTE DE TI"),
    # T
    Cargo = str_replace_all(Cargo, ".*TECHNICAL ANALYST.*|TECNICO ANALISTA", "TECNICO ANALISTA"),
    Cargo = str_replace_all(Cargo, ".*TEC. MULTIFUNCIONAL DE MANUTENCAO.*|.*TECNICO DE MANUTENCAO MECANICA.*", "TECNICO DE MANUTENCAO"),
    Cargo = str_replace_all(Cargo, ".*TECNICO MECANICO.*", "TECNICO MECANICO"),
    Cargo = str_replace_all(Cargo, ".*DATA CENTER OPERATIONS TECHNICIAN.*", "TECNICO DE OPERACOES DO DATA CENTER"),
    Cargo = str_replace_all(Cargo, ".*PROCESS TECHNICIAN.*", "TECNICO DE PROCESSOS"),
    Cargo = str_replace_all(Cargo, ".*TECNICO DE PRODUCAO.*", "TECNICO DE PRODUCAO"),
    Cargo = str_replace_all(Cargo, ".*TECNICO (DE|EM) INFORMATICA.*|.*TECNICO.+INFORMATICA", "TECNICO EM INFORMATICA"),
    Cargo = str_replace_all(Cargo, "^SDET$|^TESTER$", "TESTADOR"),
    Cargo = str_replace_all(Cargo, ".*QUALITY ASSURANCE TESTER.*", "TESTADOR DE GARANTIA DE QUALIDADE"),
    Cargo = str_replace_all(Cargo, "T\\.I\\.$|T\\.I$", "TI"),
    # V
    Cargo = str_replace_all(Cargo, ".*SALES OEM.*", "VENDEDOR"),

    # ==============================================================================
    # CARGOS ESPECÍFICOS (BASEADO NO CONHECIMENTO DO DESENVOLVEDOR)
    # ==============================================================================
    Cargo = case_when(str_detect(Empresa, ".*TAPERA BABY.*") ~ "CAPITA DA EQUIPE TAPERA BABY", TRUE ~ Cargo),
    Cargo = case_when(str_detect(Cargo, "^ESTAGIARIO$") & str_detect(Empresa, ".*E-DEPLOY.*") ~ paste("ESTAGIARIO DE TI"), TRUE ~ Cargo),

    # ==============================================================================
    # EMPRESAS ESPECÍFICAS (BASEADO NO CONHECIMENTO DO DESENVOLVEDOR)
    # ==============================================================================
    Empresa = case_when(str_detect(Empresa, "AUXILIAR ADMINISTRATIVO/FINANCEIRO") & str_detect(Nome, "FÁTIMA LOURENÇO DE LIMA") ~ paste("TREVO SEGURADORA S/A"), TRUE ~ Empresa),
    Empresa = case_when(str_detect(Empresa, "IFSP NA") ~ "IFSP SALTO", TRUE ~ Empresa),
    Empresa = case_when(str_detect(Empresa, ".*AUTONOMO.*") ~ paste("AUTONOMO"), TRUE ~ Empresa),

    # ==============================================================================
    # COMPLENTANDO NOMES DAS CIDADES NÃO ESPECÍFICADAS PELOS NOME DAS EMPRESAS
    # ==============================================================================
    # BARUERI
    Cidade = case_when(str_detect(Empresa, ".*E-DEPLOY.*|.*TITAN SOFTWARE.*") ~ "BARUERI", TRUE ~ Cidade),
    # CAMPINAS
    Cidade = case_when(str_detect(Empresa, ".*VIRACOPOS.*|.*THE GIDEONS INTERNATIONAL.*|.*MATERA SYSTEMS.*") ~ "CAMPINAS", TRUE ~ Cidade),
    # CASA BRANCA
    Cidade = case_when(str_detect(Empresa, ".*MASSEY FERGUSON.*") ~ "CASA BRANCA", TRUE ~ Cidade),
    # DUBLIN
    Cidade = case_when(str_detect(Empresa, ".*DIGITAL CREW.*") ~ "DUBLIN", TRUE ~ Cidade),
    # HORTOLÂNDIA
    Cidade = case_when(str_detect(Empresa, ".*IBM.*") ~ "HORTOLANDIA", TRUE ~ Cidade),
    # INDAIATUBA
    Cidade = case_when(str_detect(Empresa, ".*BALILLA FIAT.*|.*SEW-EURODRIVE.*|.*SPECIAL PACK.*|.*ESPECIFER.*|.*HUMMEL GROUP.*|.*TUBERFIL.*|.*BIRO 2000.*|.*BRIO INFORMATICA.*|.*STEFANINI.*") ~ "INDAIATUBA", TRUE ~ Cidade),
    # ITU
    Cidade = case_when(str_detect(Empresa, ".*STARRETT.*|.*EMFILS.*|.*FIDELITY.*|.*FLUIDO DIGITAL.*|.*ITU GARDEN SPA.*") ~ "ITU", TRUE ~ Cidade),
    # MUMBAI
    Cidade = case_when(str_detect(Empresa, ".*BEHUMAN FOUNDATION.*") ~ "MUMBAI", TRUE ~ Cidade),
    # JUNDIAÍ
    Cidade = case_when(str_detect(Empresa, ".*FOXCONN") ~ "JUNDIAI", TRUE ~ Cidade),
    # PINHAIS
    Cidade = case_when(str_detect(Empresa, ".*INFOCASE INFORMATICA.*") ~ "PINHAIS", TRUE ~ Cidade),
    # PORTO FELIZ
    Cidade = case_when(str_detect(Empresa, ".*COOPER POWER SYSTEMS.*") ~ "PORTO FELIZ", TRUE ~ Cidade),
    # SALTO
    Cidade = case_when(str_detect(Empresa, ".*TAPERA BABY.*|.*MICROCAMP.*|.*SALTOS ALIMENTOS.*|.*AUTO GERAL.*|.*CASA & BASE.*|.*DELOGIC.*|.*GRANOVA PRATA.*|.*PREFEITURA.+SALTO.*|.*IFSP SALTO.*") ~ "SALTO", TRUE ~ Cidade),
    # SÃO BERNARDO
    Cidade = case_when(str_detect(Empresa, ".*MOBITEL S/A.*") ~ "SAO BERNARDO", TRUE ~ Cidade),
    # SÃO PAULO
    Cidade = case_when(str_detect(Empresa, ".*EDITORA IBEP.*|.*FAPESP.*|.*MICROPRO.*|.*MILLENIUM INFORMATICA.*|.*BASE2 TECNOLOGIA.*|.*COMPWAY INFORMATICA.*|.*ESX.*|.*TREVO SEGURADORA S/A.*|.*DIADEIS.*|.*GOVERNO DO ESTADO DE SAO PAULO.*|.*BLITSI.*") ~ "SAO PAULO", TRUE ~ Cidade),
    # SOROCABA
    Cidade = case_when(str_detect(Empresa, ".*BOSCH.*|.*ZF LEMFORDER.*") ~ "SOROCABA", TRUE ~ Cidade),
    # TABOÃO DA SERRA
    Cidade = case_when(str_detect(Empresa, ".*HELPAY.*|.*ALL NET.*|.*ADVANCE TECNOLOGIA.*") ~ "TABOAO DA SERRA", TRUE ~ Cidade),

    # ==============================================================================
    # VERIFICA SE A EMPRESA É PÚBLICA OU PRIVADA
    # ==============================================================================
    ehPublica = str_detect(Empresa, "(.*IFSP.*|.*PREFEITURA.*|.*TRIBUNAL.*|.*ESTADUAL.*)"),

    # ==============================================================================
    # ÁREAS DOS CARGOS
    # ==============================================================================
    AreaCargo = Cargo,
    # AUTÔNOMO
    AreaCargo = str_replace_all(AreaCargo, ".*FREELANCER.*|.*AUTONOMO.*", "AUTONOMO"),
    # ADMINISTRAÇÃO
    AreaCargo = str_replace_all(AreaCargo, "FUNCIONARIO PUBLICO|.*SEGURANCA DO TRABALHO.*|ASSISTENTE COMERCIAL|ASSISTENTE DE APOIO TECNICO|
                            |.*ANALISTA DE INTERCAMBIO.*|ANALISTA DE PROJETOS|.*MELHORIA CONTINUA.*|.*ADMINISTRATIVA.*|.*STRATEGIC DECISION.*|
                            |.*BUSINESS ADMINISTRATION.*|PROJECT MANAGEMENT|GESTAO EM LOGISTICA|.*OPERADOR DE LOGISTICA.*|
                            |.*TECNICO EM ADMINISTRACAO.*|.*ADMINISTRATIVO.*|^APRENDIZ$|.*APRENDIZ.*|.*LOGISTICA.*|ASSISTENTE DE SUPRIMENTOS|
                            |ASSISTENTE DE TRAFEGO|SUPERVISOR DE TRAFEGO|ASSISTENTE TECNICO|.*AUXILIAR DE ESCRITORIO.*|AUXILIAR DE POSVENDA|
                            |.*PRODUTOS E FORNECEDORES.*|.*GESTOR DE PROJETOS.*|.*LABORATORISTA DE ESCOLAS MUNICIPAIS.*|.*SERVIDOR PUBLICO.*|
                            |AUXILIAR DE GERENCIAMENTO DE PROJETOS|^SUPORTE$|^ESTAGIARIO$", "ADMINISTRACAO"),
    # ARTES
    AreaCargo = str_replace_all(AreaCargo, ".*ARTES.*|.*FOTOGRAFIA.*|.*FOTOGRAFA.*|.*ARTE.*", "ARTES"),
    # COMUNICAÇÃO
    AreaCargo = str_replace_all(AreaCargo, ".*COMUNICACAO.*|.*IDIOMAS.*|.*IDIOMA.*|.*LANGUAGES.*|.*LANGUAGE.*|.*LETRAS.*|
                                |LINGUA.+(PORTUGUESA|JAPONESA|ESPANHOLA|INGLESA)", "COMUNICACAO"),
    # DESIGN
    AreaCargo = str_replace_all(AreaCargo, ".*DESIGN.*|.*DESENHISTA.*|DIAGRAMADOR", "DESIGN"),
    # DIREITO
    AreaCargo = str_replace_all(AreaCargo, ".*ADVOGADO.*|.*JURIDICO.*", "DIREITO"),
    # ECÔNOMIA
    AreaCargo = str_replace_all(AreaCargo, ".*ECONOMIA.*|.*CIENCIAS ECONOMICAS.*", "ECONOMIA"),
    # EDUCAÇÃO
    AreaCargo = str_replace_all(AreaCargo, ".*INICIACAO CIENTIFICA.*|.*PROFESSOR.*|.*REPRESENTANTE DA COMISSAO DE GRADUACAO.*|
                                |.*MEMBRO DO.*|.*MEMBRO DA.*|.*MONITOR.*|.*ESTUDANTE.*|.*PESQUISA.*", "EDUCACAO"),
    # EDUCAÇÃO FÍSICA
    AreaCargo = str_replace_all(AreaCargo, ".*EDUCACAO FISICA*", "EDUCACAO FISICA"),
    # FARMÁCIA
    AreaCargo = str_replace_all(AreaCargo, ".*ENFERMEIRO.*|.*FARMACEUTICA.*|.*FARMACIA.*|BIOMEDICO", "FARMACIA"),
    # FINANÇAS
    AreaCargo = str_replace_all(AreaCargo, ".*ANALISTA DE PREVENCAO E SEGURANCA.*|ANALISTA RISCO DE CREDITO|.*FINANCEIRA.*|
                                |ANALISTA FINANCEIRO PLENO|.*FRAUDE.*|.*FINANCEIRO.*|.*CAIXA EXECUTIVO.*|.*FINANCAS.*|
                                |.*ANALISTA DE COBRANCA.*|.*INFORMACAO DE COBRANCA.*", "FINANCAS"),
    # GESTÃO DE PESSOAS
    AreaCargo = str_replace_all(AreaCargo, ".*OPERADORA DE ATENDIMENTO.*|.*RECEPCIONISTA.*|SUPERVISOR DE TURNO|COMPRADOR|
                                |CHEFE DE NOVOS NEGOCIOS|ASSISTENTE DE PLANEJAMENTO|ANALISTA DE PLANEJAMENTO DE DISTRIBUICAO|
                                |.*CLIENTE.*|.*VISTORIADOR.*|REPRESENTANTE.*|.*GESTORA DE PESSOAS.*|.*FUNCIONARIA PUBLICA ESTADUAL.*|
                                |.*FISCAL DE LOJA.*|.*GESTAO DE PESSOAS.*|ATENDENTE|AUXILIAR GERAL|BALCONISTA", "GESTAO DE PESSOAS"),

    # GESTÃO DE EMPRESAS
    AreaCargo = str_replace_all(AreaCargo, "SETOR DE EXPEDICAO E FATURAMENTO|ESTOQUISTA|.*LIDER.*|^GESTOR DE PROJETOS.*|DOCUMENTADOR|
                                |.*ANALISTA DE PREVENCAO A FRAUDE.*|.*CUMPRIMENTO.*|^PROPRIETARIO$|.*EMPRESAS.*|^GERENTE$|
                                |.*ADMINISTRACAO DE EMPRESAS.*|.*BUSINESS MANAGEMENT.*|.*NEGOCIOS.*|.*GESTAO EMPRESARIAL.*|
                                |ASSISTENTE DE PROJETOS|^ASSISTENTE DE SUPORTE$|ASSISTENTE DE IMPLEMENTACAO|
                                |CONSULTORA TECNICA|^CONFERENTE$|^CONSULTOR$|.*CONTROL.*|CONTROLADOR DE ARMAZEM|.*COORDENADOR.*|
                                |.*GESTAO DE EMPRESAS.*|.*DIRETOR.*|.*EXPEDIDOR.*|MOTORISTA", "GESTAO DE EMPRESAS"),
    # INFORMAÇÃO
    AreaCargo = str_replace_all(AreaCargo, "ANALISTA.+REDE.*|ANALISTA DE PERFORMANCE|.*INTEGRACAO SOA.*|.*INFORMATICA.*|.*MOBILE DEVELOPER.*|
                                |.*ANALISTA DE SISTEMAS.*|.*SUPPORT ANALYST.*|.*ANALISTA DE SUPORTE.*|.*ANALISTA DE TI.*|
                                |.*ANALISTA DE TESTE.*|.*ANALISTA DE TREINAMENTO.*|.*DESENVOLVEDOR.*|.*ANALISTA JUNIOR.*|
                                |.*ENGENHEIRO DE SOFTWARE.*|.*ESTAGIARIO DE TI.*|ESTAGIARIO.+PROGRAMA.*|.*INSTRUTOR DE CODIGO.*|
                                |.*TECNOLOGIA.*|.*COMPUTACAO.*|PROGRAMADOR.+(JAVA|WEB|JUNIOR)|.*LIDER TECNICO.*|.*MANUTENCAO E SUPORTE.*|
                                |.*COMPUTACAO.*|.*ANALISTA FUNCIONAL.*|.*TECNICO (EM|DE) INFORMATICA.*|.*(GESTOR|GERENTE) DE TI.*|
                                |.*ESPECIALISTA EM TI.*|^PROGRAMADOR.*$|.*ASSISTENTE DE TI.*|.*FREE LANCER.*|.*ENGENHEIRO DE SUPORTE.*|
                                |.*FULL STACK DEVELOPER.*|.*JUNIOR SUPPORT ANALYST.*|PROGRAMADOR C#|.*TESTADOR.*|.*WEB DEVELOPER.*|
                                |^ANALISTA $|APRENDIZ.+(PROGRAMACAO|SOFTWARE|HARDWARE)|AUDITOR DE SISTEMA|AUXILIAR DE DESENVOLVIMENTO|
                                |AUXILIAR DE TI|.*DESENVOLVIMENTO WEB.*|.*DATA CENTER.*|.*SUPORTE DE TI.*|
                                |.*ASSISTENTE DE SUPORTE DE SISTEMAS.*|.*TI$|^TECNICO ANALISTA$|ANALISTA PROGRAMADOR.*|
                                |AUDITOR INTERNO DE QUALIDADE", "INFORMACAO"),
    # INFRAESTRUTURA
    AreaCargo = str_replace_all(AreaCargo, ".*EDIFICACOES.*|.*ENGENHARIA CIVIL.*|.*ARQUITETA.*|.*ENGENHEIRO CIVIL.*", "INFRAESTRUTURA"),
    # LIMPEZA
    AreaCargo = str_replace_all(AreaCargo, "AUXILIAR DE SERVICOS", "LIMPEZA"),
    # MARKETING
    AreaCargo = str_replace_all(AreaCargo, "SUPERVISOR DE POS VENDAS|ESTAGIARIO COMERCIAL|.*VENDEDOR.*|.*AUXILIAR DE LOJA.*|.*VENDAS.*|
                                |.*DESENVOLVIMENTO DE PRODUTOS.*|.*MARKETING.*|.*ECOMMERCE.*|.*EXPORTACAO.*|CONSULTOR DE VENDAS|
                                |.*SINISTRO AUTO.*|CORRETOR DE SEGUROS.*|.*TECNICO DE SEGUROS.*|ESPECIALISTA EM SEGUROS", "MARKETING"),
    # MIDIA
    AreaCargo = str_replace_all(AreaCargo, ".*MIDIA.*|.*REDATOR E EDITOR.*|.*CINEMA.*", "MIDIA"),

    # MILITAR
    AreaCargo = str_replace_all(AreaCargo, ".*AERONAUTICA.*|.*AVIATION.*|.*SOLDADO.*", "MILITAR"),

    # PROCESSOS INDUSTRIAIS
    AreaCargo = str_replace_all(AreaCargo, "ESTAGIARIO DA AREA DE QUALIDADE|.*METALURGICO.*|.*GARANTIA.*|.*GARANTIA DE QUALIDADE.*|
                                |.*ANALISTA DA QUALIDADE.*|.*INDUSTRIAL.*|.*PRODUCAO.*|.*MECANICA.*|^ELETRO.*|.*ELETROELETRONICA.*|
                                |.*MECHANICAL ENGINEERING.*|.*MECANICO.*|.*MECATRONICA.*|.*INDUSTRIAL.*|.*USINAGEM.*|.*MECANICO.*|
                                |.*PRODUCAO.*|ANALISTA DE PROCESSOS|ASSISTENTE DE PCP|AUXILIAR DE PCP|CAPITA DA EQUIPE TAPERA BABY|
                                |.*PROCESSOS DE FABRICACAO.*|.*ENGENHARIA DE MANUFATURA.*|.*ENGENHARIA DE PRODUTO.*|
                                |.*ENGENHARIA DE QUALIDADE.*|.*QUALIDADE E PROCESSOS.*|FABRICANTE DE FERRAMENTAS|FACILITADOR DE TURNO|
                                |.*FRESADOR.*|.*FRESA.*|.*TORNEIRO.*|INSPETOR DE QUALIDADE|METROLOGISTA|TECNICO DA QUALIDADE|
                                |TECNICO DE MANUTENCAO|TECNICO DE PROCESSOS|TECNICO DE TESTES|AJUDANTE DE PRODUCAO|
                                |ENGENHEIRO COMERCIAL DE APLICACAO|.*CNC.*|OPERADOR.+[MAQUINA.*|INJETORA|MULTINACIONAL|TECNICO|CPD]|
                                |.*TREFILA.*|^OPERADOR$|ASSISTENTE DE MELHORIA CONTINUA|ASSISTENTE DE PROCESSOS|AUXILIAR DE ALMOXARIFADO|
                                |AUXILIAR DE CONFERENTE DE CARGAS.*|.*ELETRICISTA.*|ENGENHEIRO DE QUALIDADE|.*ENGENHARIA DE PROCESSOS.*|
                                |LIDER DE ACABAMENTO|.*ANALISTA OPERACIONAL.*", "PROCESSOS INDUSTRIAIS"),
    # RECURSOS NATURAIS
    AreaCargo = str_replace_all(AreaCargo, ".*AMBIENTE.*|.*AGRONOMICA.*|.*AGRICOLA.*|AUXILIAR FLORESTAL", "RECURSOS NATURAIS"),

    # QUÍMICA
    AreaCargo = str_replace_all(AreaCargo, ".*QUIMICO.*", "QUIMICA"),

    # OUTROS
    AreaCargo = str_replace_all(AreaCargo, "BRASIL KIRIN|DESEMPREGADO|OFFICE BOY|^TECNICO$", "OUTROS"),
    # ==============================================================================
    # ÁREAS CARGOS ESPECÍFICOS (BASEADO NO CONHECIMENTO DO DESENVOLVEDOR)
    # ==============================================================================
    AreaCargo = case_when(str_detect(AreaCargo, ".*INSTRUTOR.*") & str_detect(Empresa, "MICRO.*") ~ paste("INFORMACAO"), TRUE ~ AreaCargo)
  ) %>%
  mutate(
    # ==============================================================================
    # REMOVE ESPAÇOS EM BRANCO NO INÍCIO E FINAL
    # ==============================================================================
    AreaCargo = str_trim(AreaCargo, side = c("both")),
    AreaCargo = str_squish(AreaCargo),
    Cargo = str_trim(Cargo, side = c("both")),
    Cargo = str_squish(Cargo),
    Cidade = str_trim(Cidade, side = c("both")),
    Cidade = str_squish(Cidade),
    Empresa = str_trim(Empresa, side = c("both")),
    Empresa = str_squish(Empresa),
    Nome = str_trim(Nome, side = c("both")),
    Nome = str_squish(Nome)
  )
# ==============================================================================
# PADRONIZAÇÃO ESTUDOS
# ==============================================================================
estudos <- read.csv(file = "Estudos.csv", header = TRUE) %>%
  # ==============================================================================
  # REMOÇÃO DE ACENTOS
  # ==============================================================================
  mutate(
    Area = iconv(Area, to = "ASCII//TRANSLIT"),
    Escola = iconv(Escola, to = "ASCII//TRANSLIT"),
    Curso = iconv(Curso, to = "ASCII//TRANSLIT"),
    Nome = iconv(Nome, to = "ASCII//TRANSLIT")
  ) %>%
  # ==============================================================================
  # LETRAS MAIÚSCULAS
  # ==============================================================================
  mutate_all(str_to_upper) %>%
  mutate(
    # ==============================================================================
    # REMOÇÃO DE TEXTOS INDESEJADOS
    # ==============================================================================
    # CURSO
    Curso = str_replace_all(Curso, "-", " "),
    # ESCOLA
    Escola = str_replace_all(Escola, "\"", ""),
    Escola = str_replace_all(Escola, " - ", " "),
    # NOME
    Nome = str_replace_all(Nome, "\"", " "),
    Nome = str_replace_all(Nome, "PROFESSORA|INFORMATICA", ""),

    # ==============================================================================
    # CORREÇÃO DE PALAVRAS INCORRETAS
    # ==============================================================================
    # AREA
    Area = str_replace_all(Area, "COMPUTER SCIENCE", "CIENCIAS DA COMPUTACAO"),
    Area = str_replace_all(Area, ".*EDUCATIONAL GAMIFICATION.*", "GAMIFICACAO EDUCACIONAL"),
    Area = str_replace_all(Area, "TECHNOLOGY", "TECNOLOGIA"),
    # CURSO
    Curso = str_replace_all(Curso, "IDUSTRIAL", "INDUSTRIAL"),
    Curso = str_replace_all(Curso, "ENSIONO", "ENSINO"),
    Curso = str_replace_all(Curso, "ADMINSTRACAO", "ADMINISTRACAO"),
    Curso = str_replace_all(Curso, ".*TECHNICAL COURSE", "TECNICO EM"),

    # ==============================================================================
    # ESCOLAS
    # ==============================================================================
    # A
    Escola = str_replace_all(Escola, ".*ANHANGUERA.*", "ANHANGUERA"),
    # C
    Escola = str_replace_all(Escola, ".*CEUNSP.*|.*UNIVERSIDADE CRUZEIRO DO SUL.*", "CEUNSP"),
    Escola = str_replace_all(Escola, ".*COTUCA.*|.*COLEGIO TECNICO CAMPINAS.*", "COTUCA"),
    # E
    Escola = str_replace_all(Escola, ".*EMBASSY SCHOOL HASTINGS UK.*", "EMBASSY SCHOOL HASTINGS"),
    Escola = str_replace_all(Escola, ".*ENIAC.*", "ENIAC"),
    Escola = str_replace_all(Escola, ".*ESALQ.*|.*ESCOLA SUPERIOR DE AGRICULTURA LUIZ DE QUEIROZ.*", "ESALQ"),
    Escola = str_replace_all(Escola, ".*ETEC.*|.*ESCOLA TECNICA ESTADUAL.*", "ETEC"),
    # F
    Escola = str_replace_all(Escola, ".*FAAT.*|.*FACULDADES ATIBAIA.*", "FAAT"),
    Escola = str_replace_all(Escola, ".*FACENS.*", "FACENS"),
    Escola = str_replace_all(Escola, ".*FATEC.*|.*FACULDADE DE TECNOLOGIA DE SAO PAULO.*", "FATEC"),
    Escola = str_replace_all(Escola, ".*FIEC.*|.*FUNDACAO INDAIATUBANA.*", "FIEC"),
    Escola = str_replace_all(Escola, ".*FISK.*", "FISK"),
    # I
    Escola = str_replace_all(Escola, ".*ICMC.*|.*INSTITUTO.+CIENCIAS MATEMATICAS.*", "ICMC"),
    Escola = str_replace_all(Escola, ".*IFSP.*|^INSTITUTO FEDERAL.+SAO PAULO.*|^INSTITUTO FEDERAL.+SALTO.*", "IFSP"),
    Escola = str_replace_all(Escola, ".*IPCA.*|.*INSTITUTO POLITECNICO DO CAVADO E DO AVE.*", "IPCA"),
    # L
    Escola = str_replace_all(Escola, ".*LEONOR FERNANDES DA SILVA.*", "LEONOR FERNANDES DA SILVA"),
    # M
    Escola = str_replace_all(Escola, ".*METROCAMP.*|.*METROPOLITANA CAMPINAS.*", "METROCAMP"),
    Escola = str_replace_all(Escola, ".*MAX PLANCK.*", "MAX PLANCK"),
    # S
    Escola = str_replace_all(Escola, ".*SENAC.*|.*SERVICO NACIONAL DE APRENDIZAGEM COMERCIAL.*", "SENAC"),
    Escola = str_replace_all(Escola, ".*SENAI.*|.*SERVICO NACIONAL DE APRENDIZAGEM INDUSTRIAL.*", "SENAI"),
    # U
    Escola = str_replace_all(Escola, ".*UFSCAR.*|.*UNIVERSIDADE FEDERAL DE SAO CARLOS.*", "UFSCAR"),
    Escola = str_replace_all(Escola, ".*UFPR.*|.*UNIVERSIDADE FEDERAL DO PARANA*", "UFPR"),
    Escola = str_replace_all(Escola, ".*UNIFEI.*|.*UNIVERSIDADE FEDERAL DE ITAJUBA.*", "UNIFEI"),
    Escola = str_replace_all(Escola, ".*UFRRJ.*|.*UNIVERSIDADE FEDERAL RURAL DO RIO DE JANEIRO.*", "UFRRJ"),
    Escola = str_replace_all(Escola, ".*UGF.*|.*UNIVERSIDADE GAMA FILHO.*", "UGF"),
    Escola = str_replace_all(Escola, ".*UNESP.*|.*JULIO DE MESQUITA.*", "UNESP"),
    Escola = str_replace_all(Escola, ".*UNIMEP.*|.*UNIVERSIDADE METODISTA DE PIRACICABA.*", "UNIMEP"),
    Escola = str_replace_all(Escola, ".*UNINOVE.*|.*UNINOVE UNIVERSIDADE NOVE DE JULHO.*", "UNINOVE"),
    Escola = str_replace_all(Escola, ".*UNINTER.*", "UNINTER"),
    Escola = str_replace_all(Escola, ".*UNIP.*|.*UNIVERSIDADE PAULISTA.*", "UNIP"),
    Escola = str_replace_all(Escola, ".*UNIRP.*|.*CENTRO UNIVERSITARIO DE RIO PRETO.*", "UNIRP"),
    Escola = str_replace_all(Escola, ".*UNITAU.*|.*UNIVERSIDADE DE TAUBATE.*", "UNITAU"),
    Escola = str_replace_all(Escola, ".*STANFORD UNIVERSITY.*", "UNIVERSIDADE DE STANFORD"),
    Escola = str_replace_all(Escola, "UNIVERSITY OF CALIFORNIA.*", "UNIVERSIDADE DA CALIFORNIA"),

    # ==============================================================================
    # CURSOS PELO NOME DO EGRESSO (BASEADO NO CONHECIMENTO DO DESENVOLVEDOR)
    # ==============================================================================
    Curso = case_when(str_detect(Nome, "ALEXANDRE SCAVACINI") & str_detect(Data, "2007 - 2009") ~ "TECNICO EM INFORMATICA", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "AIME PEINADO") & str_detect(Escola, "IFSP") ~ "GPI", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "ALLAN DE LIMA BELO") & str_detect(Escola, "IFSP") ~ "ADS", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "ANDRE DE PAULA CASTRO") & str_detect(Escola, "IFSP") ~ "ADS", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "CARLOS ROBERTO GUIMARAES") & str_detect(Escola, "IFSP") ~ "ADS", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "CRISTINA MARIA DA SILVA") & str_detect(Escola, "IFSP") ~ "GPI", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "DANIEL OSIRO") & str_detect(Escola, "IFSP") ~ "ADS", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "ENZO CARRERI") & str_detect(Data, "2013 – 2015") ~ "ADS", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "ENZO CARRERI") & str_detect(Data, "2011 - 2012") ~ "TECNICO EM INFORMATICA", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "FABIANA CASTELLI") & str_detect(Escola, "IFSP") ~ "GPI", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "FABIO EDUARDO DE ARRUDA") & str_detect(Escola, "UNIVERSIDADE DA CALIFORNIA") ~ "CURSO DE INFORMATICA", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "GIOVANI BERTOLLA SAMPAIO") & str_detect(Escola, "ETEC") ~ "ENSINO MEDIO", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "GUSTAVO OSORIO") & str_detect(Escola, "IFSP") ~ "ADS", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "JULIA DEGAM") & str_detect(Escola, "IFSP") ~ "ADS", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "JULIO POENTEDURA") & str_detect(Escola, "IFSP") ~ "ADS", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "MICAEL GOMES CORREIA") & str_detect(Curso, "SUPERIOR") ~ "ADS", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "NEIOMAR SANTOS") & str_detect(Escola, "IFSP") ~ "GPI", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "STEFANY NOVAES NEVES") & str_detect(Escola, "IFSP") ~ "GPI", TRUE ~ Curso),
    Curso = case_when(str_detect(Nome, "YMBERE.*") & str_detect(Escola, "FIEC") ~ "TECNICO", TRUE ~ Curso),

    # ==============================================================================
    # ÁREAS PELO NOME DO EGRESSO (BASEADO NO CONHECIMENTO DO DESENVOLVEDOR)
    # ==============================================================================
    Area = case_when(str_detect(Nome, "CRISTINA MARIA DA SILVA") & str_detect(Escola, "IFSP") ~ "PROCESSOS INDUSTRIAIS", TRUE ~ Area),
    Area = case_when(str_detect(Nome, "ENZO CARRERI") & str_detect(Escola, "IFSP") ~ "INFORMACAO", TRUE ~ Area),
    Area = case_when(str_detect(Nome, "FABIANA CASTELLI") & str_detect(Escola, "IFSP") ~ "PROCESSOS INDUSTRIAIS", TRUE ~ Area),
    Area = case_when(str_detect(Nome, "LEANDRO LEME") & str_detect(Escola, "FIAP") ~ "INFORMACAO", TRUE ~ Area),
    Area = case_when(str_detect(Nome, "JULIO POENTEDURA") & str_detect(Escola, "IFSP") ~ "INFORMACAO", TRUE ~ Area),
    Area = case_when(str_detect(Nome, "NEIOMAR SANTOS") & str_detect(Escola, "IFSP") ~ "PROCESSOS INDUSTRIAIS", TRUE ~ Area),
    Area = case_when(str_detect(Nome, "STEFANY NOVAES NEVES") & str_detect(Escola, "IFSP") ~ "PROCESSOS INDUSTRIAIS", TRUE ~ Area),
    Area = case_when(str_detect(Curso, ".*LOGISTICA.*") ~ paste("GESTAO E NEGOCIOS"), TRUE ~ Area),

    # ==============================================================================
    # CURSOS
    # ==============================================================================
    # A
    Curso = str_replace_all(Curso, ".*BACHARELADO EM ADMINISTRACAO.*|.*ADMINISTRACAO DE EMPRESAS.*", "ADMINISTRACAO"),
    Curso = str_replace_all(Curso, ".*TECNOLOGIA EM ANALISE E DESENVOLVIMENTO DE SISTEMAS.*|.*ANALISE E DESENVOLVIMENTO DE SISTEMA.*|
                            |.*TECNOLOGO EM ADS.*|.*SYSTEM ANALYSIS.*|.*TECHNOLOGIST ON SYSTEMS ANALYSIS.*|
                            |.*TECNOLOGIA ANALISE.*|.*ANALISTA E DESENVOLVEDOR DE SISTEMAS.*|.*ANALYSIS OF SYSTEM.*", "ADS"),
    Curso = str_replace_all(Curso, ".*BACHAREL EM AGRONOMIA.*", "AGRONOMIA"),
    # C
    Curso = str_replace_all(Curso, ".*CONTROLADOR DIMENCIONAL.*", "CONTROLADOR DIMENSIONAL"),
    # G
    Curso = str_replace_all(Curso, ".*GESTAO EMPRESARIAL.*", "GESTAO EMPRESARIAL"),
    Curso = str_replace_all(Curso, ".*(GESTAO|GESTOR).+PRODUCAO INDUSTRIAL.*|.*INDUSTRIAL PRODUCTION MANAGEMENT.*", "GPI"),
    # I
    Curso = str_replace_all(Curso, ".*INGLES.*|.*ENGLISH.*", "INGLES"),
    # M
    Curso = str_replace_all(Curso, ".*MASTER OF BUSINESS.*|.*SPECIALIZATION COURSE.*ADMINISTRATION.*|MBA.*", "MBA"),
    Curso = str_replace_all(Curso, ".*MASTER'S DEGREE*|MESTRADO*", "MESTRADO"),
    # T
    Curso = str_replace_all(Curso, ".*TECNICO*", "TECNICO"),
    Curso = str_replace_all(Curso, "^TECNICO.+(.*AUTOMACAO INDUSTRIAL.*)|
                            |.*TECNOLOGIA EM PROCESSOS.*", "TECNICO EM AUTOMACAO INDUSTRIAL"),
    Curso = str_replace_all(Curso, "^TECNICO.+(.*ANALISE E DESENVOLVIMENTO.*|.*PROGRAMACAO DE SISTEMAS.*|.*SYSTEM PROGRAMMING AND NETWORKS|.*TECHNICAL ON INFORMATION TECHNOLOGY.*|.*INFORMATICA.*)", "TECNICO EM INFORMATICA"),
    Curso = str_replace_all(Curso, ".*TECHNICAL ON INFORMATION TECHNOLOGY.*", "TECNICO EM TI"),
    # S
    Curso = str_replace_all(Curso, ".*SISTEMAS INTEGRADOS DE GESTAO DA QUALIDADE.*, .*AMBIENTE E SEGURANCA.*", "SIG-QAS"),
    Curso = str_replace_all(Curso, ".*INFORMATION SYSTEMS.*", "SISTEMA DE INFORMACAO"),

    # ==============================================================================
    # CURSOS UTILIZANDO ÁREAS PADRONIZADAS
    # ==============================================================================
    Curso = case_when(str_detect(Curso, "^GRADUACAO") & str_detect(Area, ".*ANALISE.+SISTEMAS.*") ~ paste("ADS"), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, ".*ENGENHARIA.*") & str_detect(Area, ".*AGRONOMICA.*") ~ paste("ENGENHARIA AGRONOMICA"), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, ".*ENGENHARIA.*|^GRADUACAO") & str_detect(Area, ".*MECANICA.*") ~ paste("ENGENHARIA MECANICA"), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, ".*ENGENHARIA.*") & str_detect(Area, ".*PRODUCAO.*") ~ paste("ENGENHARIA DE PRODUCAO"), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^ESPECIALIZACAO") & str_detect(Area, "ENGENHARIA AERONAUTICA") ~ paste("ESPECIALIZACAO EM", Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^GRADUACAO") & str_detect(Area, ".*SISTEMAS.+INFORMACAO.*") ~ paste("GESTAO DE SISTEMAS DE INFORMACAO"), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^GRADUACAO") & str_detect(Area, "PROCESSOS INDUSTRIAIS") ~ paste("GPI"), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^TECNICO") & str_detect(Area, ".*ELETROELETRONICA.*") ~ paste("TECNICO EM", Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^TECNICO") & str_detect(Area, ".*TECNICO EM ELETROELETRONICA.*") ~ paste(Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^TECNICO") & str_detect(Area, ".*INFORMATICA.*") ~ paste("TECNICO EM INFORMATICA"), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^MESTRADO$|.*MESTRADO IN.*") ~ paste("MESTRADO EM", Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^POS GRADUACAO$") ~ paste("POS GRADUACAO EM", Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^DOUTORADO$") ~ paste("DOUTORADO EM", Area), TRUE ~ Curso),

    # ==============================================================================
    # ÁREAS
    # ==============================================================================
    # C
    Area = str_replace_all(Area, ".*IDIOMAS*|.*LANGUAGES.*", "COMUNICACAO"),
    # E
    Area = str_replace_all(Area, ".*ENSINO MEDIO.*", "EDUCACAO"),
    # G
    Area = str_replace_all(Area, ".*FINANCAS.*|.*ADMINISTRACAO DE EMPRESAS.*|.*STRATEGIC DECISION.*|.*QUALIDADE.*|.*BUSINESS MANAGEMENT.*|
                           |.*BUSINESS ADMINISTRATION.*", "GESTAO E NEGOCIOS"),

    # I
    Area = str_replace_all(Area, ".*COMPUTACAO.*|.*INFORMACAO.*|.*ANALISE DE SISTEMAS.*|.*ANALISE E DESENVOLVIMENTO DE SISTEMAS.*|
                           |.*BANCO DE DADOS.*|.*BIG DATA.*|.*INFORMATICA.*|.*SOFTWARE.*|.*COMPUTER.*|
                           |.*SYSTEM ANALYSIS.*|.*T[.]I[.]*|.*CIENCIA DA APRENDIZAGEM.*|.*REDES DE COMPUTADORES.*|.*INFORMATION.*|
                           |.*GAMIFICACAO EDUCACIONAL.*|.*DESENVOLVIMENTO DE GAMES.*", "INFORMACAO"),
    Area = str_replace_all(Area, ".*ENGENHARIA CIVIL.*", "INFRAESTRUTURA"),
    # M
    Area = str_replace_all(Area, ".*AERONAUTICA.*|.*AVIATION.*", "MILITAR"),
    # P
    Area = str_replace_all(Area, ".*INDUSTRIAL.*|.*PRODUCAO.*|.*MECANICA.*|^ELETRO.*|.*ELETROELETRONICA.*", "PROCESSOS INDUSTRIAIS"),
    # R
    Area = str_replace_all(Area, ".*AGRONOMICA.*|.*AGRICOLA.*", "RECURSOS NATURAIS"),

    # ==============================================================================
    # CURSOS UTILIZANDO ÁREAS PADRONIZADAS
    # ==============================================================================
    Curso = case_when(str_detect(Curso, ".*TECNOLOGO.*|.*TECHNOLOGIST.*") & str_detect(Area, "INFORMACAO") ~ paste("ADS"), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "^GRADUACAO") & str_detect(Area, ".*ANALISE.+SISTEMAS.*") ~ paste("ADS"), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, ".*CURSO.+EXTENSAO.*") & str_detect(Area, "MILITAR") ~ paste("CURSO DE EXTENSAO NA AREA", Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, ".*CURSO.+EXTENSAO.*") & str_detect(Area, "GESTAO E NEGOCIOS") ~ paste("CURSO DE EXTENSAO NA AREA DE", Area), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, ".*TECNOLOGO.*|.*TECHNOLOGIST.*") & str_detect(Area, "PROCESSOS INDUSTRIAIS") ~ paste("GPI"), TRUE ~ Curso),
    Curso = case_when(str_detect(Curso, "(.*SUPERIOR.*|^GRADUACAO)") & str_detect(Area, "PROCESSOS INDUSTRIAIS") ~ paste("GPI"), TRUE ~ Curso),

    # ==============================================================================
    # ÁREAS UTILIZANDO OS CURSOS PADRONIZADOS
    # ==============================================================================
    Area = case_when(str_detect(Area, "TECNOLOGIA") & str_detect(Curso, ".*ADS.*") ~ paste("INFORMACAO"), TRUE ~ Area),
    Area = case_when(str_detect(Area, "TECNOLOGIA|ENGENHARIA") & str_detect(Curso, ".*GPI.*") ~ paste("PROCESSOS INDUSTRIAIS"), TRUE ~ Area),
    Area = case_when(str_detect(Curso, ".*ENSINO MEDIO.*") ~ paste("EDUCACAO"), TRUE ~ Area),
    Area = case_when(str_detect(Curso, ".*SOCIOLOGIA.*") ~ paste("CIENCIAS SOCIAIS"), TRUE ~ Area),
    Area = case_when(str_detect(Curso, ".*LETRAS.*|.*FISK.*") ~ paste("COMUNICACAO"), TRUE ~ Area),
    Area = case_when(str_detect(Curso, ".*TECNICO EM ADMINISTRACAO.*|.*GESTAO EMPRESARIAL.*|.*CONTABILIDADE.*") ~ paste("GESTAO E NEGOCIOS"), TRUE ~ Area),
    Area = case_when(str_detect(Curso, ".*ADS.*|.*TECNICO EM INFORMATICA.*") ~ paste("INFORMACAO"), TRUE ~ Area),
    Area = case_when(str_detect(Curso, ".*TECNICO EM AUTOMACAO.*|.*GPI.*|.*AUTOMACAO INDUSTRIAL.*|.*ENGENHARIA MECANICA.*|.*SISTEMAS INTEGRADOS DE GESTAO.*|.*ENGENHARIA DE PRODUCAO.*") ~ paste("PROCESSOS INDUSTRIAIS"), TRUE ~ Area),

    # ==============================================================================
    # CURSOS UTILIZANDO ESCOLAS PADRONIZADAS
    # ==============================================================================
    Curso = case_when(str_detect(Curso, ".*ENGENHARIA.*") & str_detect(Escola, "UNIVESP") ~ paste("ENGENHARIA DE PRODUCAO"), TRUE ~ Curso),

    # ==============================================================================
    # OUTROS
    # ==============================================================================
    Curso = case_when(str_detect(Escola, "IFSP") & str_detect(Area, "PROCESSOS INDUSTRIAIS") & is.na(Curso) ~ paste("GPI"), TRUE ~ Curso),

    # ==============================================================================
    # EXTRAI OS ANOS DE INÍCIO E FIM DE CADA CURSO
    # ==============================================================================
    AnoIngresso = str_extract(Data, "^\\d{4}"),
    AnoEgresso = str_extract(Data, "\\d{4}$"),

    # ==============================================================================
    # VERIFICA SE O CURSO É ADS [flag]
    # ==============================================================================
    ehADS = str_detect(Curso, "ADS"),

    # ==============================================================================
    # VERIFICA SE O CURSO É GPI [flag]
    # ==============================================================================
    ehGPI = str_detect(Curso, "GPI")
  ) %>%
  mutate(
    # ==============================================================================
    # REMOVE ESPAÇOS EM BRANCO NO INÍCIO E FINAL
    # ==============================================================================
    Area = str_trim(Area, side = c("both")),
    Area = str_squish(Area),
    Escola = str_trim(Escola, side = c("both")),
    Escola = str_squish(Escola),
    Curso = str_trim(Curso, side = c("both")),
    Curso = str_squish(Curso),
    Nome = str_trim(Nome, side = c("both")),
    Nome = str_squish(Nome)
  )
# ==============================================================================
# GRAVAÇÃO
# ==============================================================================
curriculos_csvs_padronizados <- list.files("../Dashboard/", pattern = "*.csv", full.names = TRUE)
file.remove(curriculos_csvs_padronizados)
tryCatch({
  if (!exists("empregos")) {
    stop("Objeto empregos não encontrado.")
  }
  write.csv(empregos, "../Dashboard/EmpregosPadronizado.csv", row.names = FALSE)
  if (!exists("estudos")) {
    stop("Objeto estudos não encontrado.")
  }
  write.csv(estudos, "../Dashboard/EstudosPadronizado.csv", row.names = FALSE)
})
