# Carregue os pacotes necessários
library(tesseract) #OCR
library(pdftools) #converte PDF em PNG
library(magick) #CORRIGE VIES DE FOTOS
library(stringr)
library(stringi)
library(utils) #cria arq txt
library(dplyr)
library(readr)

ocr <- function(pdf){
  tabela_palavras = NULL
  for(i in 1:pdf_info(pdf)$pages){
    imagem <- pdf_render_page(pdf, dpi = 300,page=i)
    input <- image_read(imagem)
    tabela_palavras2 <- input %>%
      image_resize("2000x2000") %>%
      image_convert(type = 'Grayscale') %>%
      image_trim(fuzz = 20) %>%
      tesseract::ocr_data() #tabela com as palavras encontradas
    tabela_palavras = rbind(tabela_palavras,tabela_palavras2)
  }
  texto2 = pdf_text(pdf)
  texto2 = gsub("\n", "", texto2)
  tabela_palavras3 = data.frame("word"=NA)
  
  for(i in 1:length(texto2)){
    texto3 = data.frame(strsplit(texto2[i], " "))
    colnames(texto3)[1] = "word"
    texto3 = filter(texto3, word != "")
    tabela_palavras3 = data.frame(c(tabela_palavras3$word,texto3$word))
  }
  
  tabela_palavras2 = data.frame(c(tabela_palavras$word,tabela_palavras3$word))
  colnames(tabela_palavras2) = "word"
  
  return(tabela_palavras2)
}

#########COELBA##############
doc_cb <- function(tabela_palavras){ 
  ###IDENTIFICA TIPOS DE DOCS
  doc = c("CCIR","CNPJ","CPF","CTRU","IRR","PROD","NIS","PRMA","PRONAF","RURL",
          "IE","VIDA","RMMV","NIRF","RG", "AUTODECLARAÇÃO","LIAM", "OUTA")
  palavras_chave = matrix(data=NA,nrow = length(doc),ncol = 11)
  ## PALAVRAS CHAVE
  #CCIR
  palavras_chave[1,] = c("INCRA","CCIR","CNS","REGISTRO","MULTA", "COMPROBAT","INCRA.GOV",rep(NA,4))
  #CNPJ
  palavras_chave[2,] = c("RFB","EMPRESARIAL","CNPJREVA","ESTABELECIMENTO","FANTASIA","QSA",
                         "1.634","1.470",rep(NA,3))
  #CPF
  palavras_chave[3,]= c("GOVBR","VERIFICADOR","SSL","ATCTA","/CPF/",rep(NA,6))
  #CTRU
  palavras_chave[4,] = c("TRABALHO","EMPREGO","CARTEIRA","PREVD","PIS","PASEP",
                         "9.049","ELEITOR","ZONA","CNH","SRTE")
  #IRR
  palavras_chave[5,] = c("ANEXO","414","IRRIGA","AQUICULTUR","DESCONTO", "CONSUMO","ANEEL",
                         "ICMS","PRODUTOR","VERDE","FORNECIMENTO")
  #PROD
  palavras_chave[6,] = c("DIAC","ITR","NIRF","RETIFICADORA","IMUNE","1.581","TRIBUTARIA",
                         "PRODUTOR","DITR",rep(NA,2))
  #NIS
  palavras_chave[7,] = c("BUSCAR","FAMILIA","UNICO","PROGRAMA","MIGRADO","ELO","PARENTESCO","CAIXA",
                         rep(NA,3))
  #PRMA
  palavras_chave[8,] = c("CODEVASF","VALES","FRANCISCO","SAPCANA","ABASTECIMENTO","USINA","DCAA",
                         "6.129", rep(NA,3))
  #PRONAF
  palavras_chave[9,] = c("PRONAF","RENDA","ESTABELECIENTO","MDA","ATESTADO",
                         "DAPWEB","EMPREGADOS",rep(NA,4))
  #RURL
  palavras_chave[10,] = c("SINDICA","SINDICA","TRABALHA","POSSEIRO","NIRF",
                          "CAEM","PORTADOR","CTPS","ZONA","ASSOCIADO",rep(NA,1))
  #IE
  palavras_chave[11,] = c("ICMS","FANTASIA","DTE","CRC","SEFAZ","SINTEGRA","SEPD",rep(NA,4))
  #VIDA
  palavras_chave[12,] = c("COMPROMISSO","PLEITEADO","TRATAMENTO","EQUIPE","ESSENCIAL","VIDA",rep(NA,5))
  #RMMV
  palavras_chave[13,] = c("CREMEB","HORAS","PACIENTE","SUS", "VIDA", "HORAS", rep(NA,5))
  #NIRF
  palavras_chave[14,] = c("ITR","NIRF","PGFN","NEGATIVA","RESSALVADO", rep(NA,6))
  #RG
  palavras_chave[15,] = c("TERRITORIO","CARTEIRA","VALIDA",rep(NA,8))
  #AUTODECLARAÇÃO
  palavras_chave[16,] = c("AUTODECLARAGAO","IRRIGAGAO","BENEFICIO","TARIFARIO", "OUTORGA",
                          "AQUICULTURA", rep(NA,5))
  #LIAM
  palavras_chave[17,] = c("INEMA","DISPENSA","LICENCIAMENTO","WWW.SEIA.BA", "SEIA",
                          rep(NA,6))
  #OUTA
  palavras_chave[18,] = c("OUTORGA","INEMA","DISPENSA","RECURSOS","CAPTACAO", "NORMATIVA",
                          rep(NA,5))
  
  ## CONTA PALAVRAS CHAVE E PROCURA NA TABELA DE PALAVRAS
  achou =  rep(0,length(doc))
  porcentagem =  rep(0,length(doc))
  verifica = data.frame(doc,achou,porcentagem) 
  
  for(i in 1:nrow(palavras_chave)){
    procura = sapply(palavras_chave[i,][which(is.na(palavras_chave[i,]) == FALSE)], 
                     grep, toupper(tabela_palavras$word), value = TRUE) 
    achou = 0
    total = 0
    for(j in 1:length(palavras_chave[i,][which(is.na(palavras_chave[i,]) == FALSE)])){
      achou = achou + length(procura[[palavras_chave[i,j]]])
      if(length(procura[[palavras_chave[i,j]]]) > 0){ total = total + 1}
    }
    verifica$achou[i] = achou
    verifica$porcentagem[i] = total/length(palavras_chave[i,][which(is.na(palavras_chave[i,]) == FALSE)])*100
    
  }
  #max palavras chave é o doc
  documento = verifica$doc[which(verifica$porcentagem == max(verifica$porcentagem))] 
  return(documento)
}
#########CELPE##############
doc_cp <- function(tabela_palavras){ 
  ###IDENTIFICA TIPOS DE DOCS
  ## PALAVRAS CHAVE
  doc = c("CNPJ","CONS","IE","NB","NIS","PRIC","PRIE","PROD","RURL","ITR","RG","IRR","FSS",
          "CPF","OUTA","LIAM","CTRU","AUTODECLARAÇÃO")
  palavras_chave = matrix(data=NA,nrow = length(doc),ncol = 11)
  #CNPJ
  palavras_chave[1,] = c("RFB","EMPRESARIAL","CNPJREVA","ESTABELECIMENTO","FANTASIA","QSA",
                         "1.634","1.470",rep(NA,3))
  #CONS
  palavras_chave[2,] = c("DESPACHO","ICMS","DV","PROCESSO","INTERESSADA","CONSULA","14.876","XXX","DTO",
                         "GELP",rep(NA,1))
  #IE
  palavras_chave[3,] = c("CERTID","CACEPE","REGIME","SITUA","SEFAZ",
                         "BLOQUEIO","SINTEGRA",rep(NA,4))
  #NB
  palavras_chave[4,] = c("DESENVOLVIMENTO","BPC","DETALHES","NB","PROCURADOR","REPRESENTANTE",
                         "LEGAL","BENEF","DATAPREV","CNIS","INFBEN")
  
  #NIS
  palavras_chave[5,] = c("DIREITO","HUMANO","CRAS","BURA","BOLSA","RENDA","CAPITA","PARENTESCO",
                         "RANI","CASAMENTO","CAIXA")
  #PRIC
  palavras_chave[6,] = c= c("INCRA","CCIR","CNS","REGISTRO","MULTA",rep(NA,6))
  #PRIE
  palavras_chave[7,] = c("CACEPE","DIAC","RAZ","MJ","REGIME","FAIXA",
                         "PROTOCOLO","ATUALIZA",rep(NA,3))
  #PROD
  palavras_chave[8,] = c("REQUERIMENTO","ICMS","PRODUTOR","ATIVIDADE","APICULTURA","CULTIVO","UNIDADE",
                         "CONSUMIDORA",rep(NA,3))
  
  #RURL
  palavras_chave[9,] = c("ASSOCIA","RURAL","CARTEIRA","AGRIC","SINDIC"
                         ,"CIC","STR","RENDA","TRABALH","SINTRAF",rep(NA,1))
  #ITR
  palavras_chave[10,] = c("DIAC","RECIBO","ENTREGA","ITR","NIRF", 
                          "INFORMA","RETIFICADORA","CAFIR","DARF",rep(NA,2))
  #RG
  palavras_chave[11,] = c("TERRITORIO","CARTEIRA","VALIDA",rep(NA,8))
  #IRR
  palavras_chave[12,] = c("ANEXO","DESCONTO","BENEF", "ICMS", rep(NA,7))
  #FSS
  palavras_chave[13,] = c("MEDIDOR","VIZINHO","KIT","CARGA", "REATIVA", "RAMAL", rep(NA,5))
  #CPF
  palavras_chave[14,]= c("GOVBR","VERIFICADOR","SSL","ATCTA","/CPF/",rep(NA,6))
  #OUTA
  palavras_chave[15,] = c("AGUAS","OUTORGA","CLIMA","APAC", "SANEAMENTO", rep(NA,6))
  #LIAM
  palavras_chave[16,] = c("LICENCA","LICENGA","AMBIENTAL","REGULARIZAGAO", "OPERACAO",
                          rep(NA,6))
  #CTRU
  palavras_chave[17,] = c("TRABALHO","EMPREGO","CARTEIRA","SALARIO","PROFISSIONAL","POLEGAR",
                          rep(NA,5))
  #AUTODECLARAÇÃO
  palavras_chave[18,] = c("AUTODECLARAGAO","IRRIGAGAO","BENEFICIO","TARIFARIO", "OUTORGA",
                          "AQUICULTURA", rep(NA,5))
  
  ## CONTA PALAVRAS CHAVE E PROCURA NA TABELA DE PALAVRAS
  
  achou =  rep(0,length(doc))
  porcentagem =  rep(0,length(doc))
  verifica = data.frame(doc,achou,porcentagem) 
  
  for(i in 1:nrow(palavras_chave)){
    procura = sapply(palavras_chave[i,][which(is.na(palavras_chave[i,]) == FALSE)], 
                     grep, toupper(tabela_palavras$word), value = TRUE) 
    achou = 0
    total = 0
    for(j in 1:length(palavras_chave[i,][which(is.na(palavras_chave[i,]) == FALSE)])){
      achou = achou + length(procura[[palavras_chave[i,j]]])
      if(length(procura[[palavras_chave[i,j]]]) > 0){ total = total + 1}
    }
    verifica$achou[i] = achou
    verifica$porcentagem[i] = total/length(palavras_chave[i,][which(is.na(palavras_chave[i,]) == FALSE)])*100
    
  }
  
  #max palavras chave é o doc
  documento = verifica$doc[which(verifica$porcentagem == max(verifica$porcentagem))] 
  return(documento)
}
#########COSERN##############
doc_cs <- function(tabela_palavras){ 
  ###IDENTIFICA TIPOS DE DOCS
  doc = c("CNPJ","CTRU","ENBE","IE","NB","NIS","PRIC","PRIE","RURL","ITR","RG","VIDA",
          "CPF","AUTODECLARAÇÃO", "LIAM","OUTA")
  palavras_chave = matrix(data=NA,nrow = length(doc),ncol = 11)
  ## PALAVRAS CHAVE
  #CNPJ
  palavras_chave[1,] = c("RFB","ESTABELECIMENTO","EMPRESARIAL","FANTASIA",
                         "NATUREZA","EFR","1.634","RECEITA","QSA","CAPITAL","1.470")
  #CTRU
  palavras_chave[2,] = c("TRABALH","EMPREGADOR","CBO","REGISTRO","CARGO","REMUNERA","ESPECIFICADA",
                         "EMPREGO","CARTEIRA",rep(NA,2))
  #ENBE
  palavras_chave[3,] = c("CONSELHO","CNAS","8.742","2.536","RENOVADO","ENTIDADE","BENEFICENTE",
                         rep(NA,4))
  #IE
  palavras_chave[4,] = c("TRIBUTA","EMPRESARIAL","CNAE","NATUREZA","PAGAMENTO",
                         "SIGAT","CREDENCIADO","CONTRIBUINTE","SITUAGAO","CADASTRAL",rep(NA,1))
  #NB
  palavras_chave[5,] = c("BCP","NB","PROCURADOR","REPRESENTANTE","DETALHES","LEGAL","NIT","CTPS",
                         "MPAS","HISCRE","PASSAPORTE")
  
  #NIS
  palavras_chave[6,] = c("PROGRAMA","FAMILIAR","RENDA","MIGRADO","ELO","PARENTESCO","RANI","CAIXA",
                         "CAIXA","BOLSA",rep(NA,1))
  #PRIC
  palavras_chave[7,] = c("MDA","INCRA","PROJETO","ASSENTAMENTO","SIPRA","INCRA","HOMOLOGA","ESPELHO",
                         "IDIARN","CCIR","CCIR")
  #PRIE
  palavras_chave[8,] = c("SET","CNAE","PAF","ECF","EFD","UVT2",rep(NA,5))
  
  #RURL
  palavras_chave[9,] = c("PROFISSIONAL","PREVID","SINDICATO","SIND","TRAB","RURAIS","CARTEIRA",
                         "ASSOCIADO",rep(NA,3))
  #ITR
  palavras_chave[10,] = c("DIAC","RECIBO","ENTREGA","ITR","EXERC","NIRF","INFORMA","RETIFICADORA",
                          "DARF",rep(NA,2))
  #RG
  palavras_chave[11,] = c("TERRITORIO","CARTEIRA","VALIDA",rep(NA,8))
  
  #VIDA
  palavras_chave[12,] = c("EQUIPAMENTOS","SOBREVIV","HUMANA",rep(NA,8))
  
  #CPF
  palavras_chave[13,]= c("GOVBR","VERIFICADOR","SSL","ATCTA","/CPF/",rep(NA,6))
  
  #AUTODECLARAÇÃO
  palavras_chave[14,] = c("AUTODECLARAGAO","IRRIGAGAO","BENEFICIO","TARIFARIO", "OUTORGA",
                          "AQUICULTURA", rep(NA,5))
  
  #LIAM
  palavras_chave[15,] = c("IDEMIA","IDEMA","LICENCIAMENTO","FLORESTAL", "IDEMA", "EMPREENDEDOR",
                          rep(NA,5))
  
  #OUTA
  palavras_chave[16,] = c("IGARN","IGARN,","OUTORGA","RECURSOS", "ASSEJUR/IGARN",
                          rep(NA,6))
  
  ## CONTA PALAVRAS CHAVE E PROCURA NA TABELA DE PALAVRAS
  achou =  rep(0,length(doc))
  porcentagem =  rep(0,length(doc))
  verifica = data.frame(doc,achou,porcentagem) 
  
  for(i in 1:nrow(palavras_chave)){
    procura = sapply(palavras_chave[i,][which(is.na(palavras_chave[i,]) == FALSE)], 
                     grep, toupper(tabela_palavras$word), value = TRUE) 
    achou = 0
    total = 0
    for(j in 1:length(palavras_chave[i,][which(is.na(palavras_chave[i,]) == FALSE)])){
      achou = achou + length(procura[[palavras_chave[i,j]]])
      if(length(procura[[palavras_chave[i,j]]]) > 0){ total = total + 1}
    }
    verifica$achou[i] = achou
    verifica$porcentagem[i] = total/length(palavras_chave[i,][which(is.na(palavras_chave[i,]) == FALSE)])*100
    
  }
  
  #max palavras chave é o doc
  documento = verifica$doc[which(verifica$porcentagem == max(verifica$porcentagem))] 
  return(documento)
}

empresas = c("Coelba","Cosern","Pernambuco")
# Defina a interface do usuário
ui <- fluidPage(
  
  # Adicione um menu suspenso para permitir a seleção da empresa
  #selectInput("empresa", "Selecione a empresa", empresas),
  radioButtons("empresa", "Selecione a empresa:", choices = empresas),
  
  # Adicione um botão de upload para permitir o upload de um arquivo PDF
  fileInput("file", "Selecione um arquivo PDF"),
  
  # Adicione um espaço em branco para mostrar o texto extraído
  verbatimTextOutput("text")
)

# Defina o servidor
server <- function(input, output) {
  # Use o evento de upload para extrair o texto do arquivo PDF
  text <- eventReactive(input$file, {
    # Leia o arquivo PDF usando o pacote pdftools
    #pdf <- pdf_text(input$file$datapath)
    # Combine todas as páginas do PDF em um único vetor de texto
    text <- ocr(input$file$datapath)
    if(input$empresa == "Coelba"){
      text <-  doc_cb(text)}
    if(input$empresa == "Cosern"){
      text <-  doc_cb(text)}
    if(input$empresa == "Pernambuco"){
      text <-  doc_cb(text)}
    
    # Retorne o texto
    return(paste("Resultado do classificador: ",text))
  })
  # Mostre o texto extraído na interface do usuário
  output$text <- renderPrint({
    text()
  })
}

# Rode a aplicação
shinyApp(ui, server)