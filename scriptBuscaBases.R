library(bizdays)
library(dplyr)
library(lubridate)
library(stringr)
library(PortfolioAnalytics)
##############################CONFIGURAÇÃO DE PROXY######################################
http_proxy = "10.10.120.59:80"
https_proxy = "10.10.120.59:80"
options(download.file.method="auto", timeout = 6000)

###########################################DOWNLOAD DOS ARQUIVOS CVM PARA ATUALIZAÇÃO DA BASE INTERNA#####################################


dirBases <- "" #setar o caminho aonde as bases de dados ser�o salvas
cadHist <- "http://dados.cvm.gov.br/dados/FI/CAD/DADOS/HIST/"
cadAtual <- "http://dados.cvm.gov.br/dados/FI/CAD/DADOS/"
docsFundos <- "http://dados.cvm.gov.br/dados/FI/DOC/EVENTUAL/DADOS/"
dadosFinan <- "http://dados.cvm.gov.br/dados/FI/DOC/INF_DIARIO/DADOS/"
dadosExtrato <- 'http://dados.cvm.gov.br/dados/FI/DOC/EXTRATO/DADOS/'
laminaFI <- 'http://dados.cvm.gov.br/dados/FI/DOC/LAMINA/DADOS/'
perfilMensal <- 'http://dados.cvm.gov.br/dados/FI/DOC/PERFIL_MENSAL/DADOS/'
urlLAMNIAHIST <- "http://dados.cvm.gov.br/dados/FI/DOC/LAMINA/DADOS/HIST/"
infCadastral <- file.path(dirBases,"arquivosInf")

if(!dir.exists(file.path(dirBases,"arquivosInf"))){
  dir.create(infCadastral)
}
    
  cal <- create.calendar(holidaysANBIMA, weekdays=c('saturday', 'sunday'), name='ANBIMA')
  bizdays.options$set(default.calendar=cal)
  dt <- add.bizdays(Sys.Date(),-2)

  download.file(url = paste0(cadAtual,"cad_fi.csv"),
                destfile = file.path(dirBases,"TEMP",paste0("cad_fi.csv")),
                cacheOK = F)
  download.file(url = paste0(cadAtual,"cad_fi_hist.zip"),
                destfile = file.path(dirBases,"TEMP",paste0("cad_fi_hist.zip")),
                cacheOK = F)
  unzip(zipfile = file.path(dirBases,"TEMP",paste0("cad_fi_hist.zip")), exdir = file.path(dirBases,"TEMP"))
  
  
  download.file(url = paste0(dadosExtrato,"extrato_fi.csv"),
                destfile = file.path(dirBases,"TEMP",paste0("extrato_fi.csv")),
                cacheOK = F)
  
  download.file(url = paste0(laminaFI,'lamina_fi_',format(Sys.Date()-30, "%Y%m"),".zip"),
                destfile = file.path(dirBases,"TEMP",paste0("lamina_fi.zip")),
                cacheOK = F)
  unzip(zipfile = file.path(dirBases,"TEMP",paste0("lamina_fi.zip")), exdir = file.path(dirBases,"TEMP"))
  
  
  download.file(url = paste0(perfilMensal,"perfil_mensal_fi_",format(Sys.Date()-60, "%Y%m"),".csv"),
                destfile = file.path(dirBases,"TEMP",paste0("perfil_mensal_fi_",format(Sys.Date()-30, "%Y%m"),".csv")),
                cacheOK = F)
  
  
  download.file(url = paste0(docsFundos,paste0("eventual_fi_",format(dt,"%Y"),".csv")),
                              destfile = file.path(dirBases,"TEMP",paste0("eventual_fi_",format(dt,"%Y"),".csv")),
                              cacheOK = F)
  
  download.file(url = paste0(dadosFinan,"inf_diario_fi_",format(dt,"%Y%m"),".csv"),
                destfile = file.path(dirBases,"TEMP",paste0("inf_diario_fi_",format(dt,"%Y%m"),".csv")),
                cacheOK = F)
  
  
  
#####SCRIPTS PARA RODAR NA PRIMEIRA VEZ, PARA BUSCAR O HIST�RICO DE BASES############
  
  # for(i in 2014:year(Sys.Date())){
  # download.file(url = paste0(docsFundos,paste0("eventual_fi_",i,".csv")),
  #              destfile = file.path(dirBases,"TEMP",paste0("eventual_fi_",i,".csv")),
  #              cacheOK = F)}
  
  # for(i in 2014:year(Sys.Date())){
  #  download.file(url = paste0(docsFundos,paste0("eventual_fi_",i,".csv")),
  #                destfile = file.path(dirBases,"TEMP",paste0("eventual_fi_",i,".csv")),
  #                cacheOK = F)}
  
  
    # for(i in 2020:year(Sys.Date())){
  #  for(j in 1:12){
  #    if(j < 10){
  #      j <- paste0("0",j)
  #    }
  #  download.file(url = paste0(dadosFinan,paste0("inf_diario_fi_",i,j,".csv")),
  #                destfile = file.path(dirBases,"TEMP",paste0("inf_diario_fi_",i,j,".csv")),
  #                cacheOK = F)}}
  
  
  
  #ESPECIAL LAMINAS PASSADAS
  # for(i in 2014:2021){
  #   
  #   if(i < 2019){
  #     for(j in 1:12){
  #       if(j < 10){
  #         j <- paste0("0",j)
  #       }
  #       #LAMINA FI
  #       download.file(url = paste0(urlLAMNIAHIST,paste0("lamina_fi_",i,j,".zip")),
  #                   destfile = file.path(dirBases,"TEMP",paste0("lamina_fi_",i,j,".zip")),
  #                   cacheOK = F)
  #       unzip(zipfile = file.path(dirBases,"TEMP", paste0("lamina_fi_",i,j,".zip")),
  #           exdir = file.path(dirBases,"TEMP"))
  #   }}
  #   
  #   if(i >= 2019){
  #     for(j in 1:12){
  #       if(j < 10){
  #         j <- paste0("0",j)
  #       }
  #       #LAMINA FI
  #       download.file(url = paste0(laminaFI,paste0("lamina_fi_",i,j,".zip")),
  #                     destfile = file.path(dirBases,"TEMP",paste0("lamina_fi_",i,j,".zip")),
  #                     cacheOK = F)
  #       unzip(zipfile = file.path(dirBases,"TEMP", paste0("lamina_fi_",i,j,".zip")),
  #             exdir = file.path(dirBases,"TEMP"))
  #     }}
  #   
  # }
  


#############################################GERAÇÃO DOS ARQUIVOS CSV E RDS PARA USOS POSTERIORES###################################
arquivosTemp <- list.files(file.path(dirBases,"TEMP"), full.names = T)



#dadosFinan <- list()
#dadosFinan <- sapply(dadosDiarios, function(x) read.csv2(x, header = T, sep = ";", dec = ".", as.is = T, quote = ""))
dadosFinan <- readRDS(file.path(dirBases,"TEMP","dadosFinan.rds"))
ultDadosFinan <- read.csv2(file.path(dirBases,"TEMP",paste0("inf_diario_fi_",format(dt,"%Y%m"),".csv")), header = T, sep = ";", dec = ".", as.is = T, quote = "")
dadosFinan <- rbind(dadosFinan, ultDadosFinan)
#dadosFinan <- bind_rows(dadosFinan)
#write.csv2(file = file.path(dirBases,"basesAgregadas.csv"), row.names = F, x = dadosFinan)

saveRDS(dadosFinan, file = file.path(dirBases,"TEMP","dadosFinan.rds"))

arquivosFundo <- arquivosTemp[str_detect(arquivosTemp, "eventual_fi_")]
arquivosFundo <- sapply(arquivosFundo, function(x) read.csv2(x, header = T, sep = ";", dec = ".", as.is = T, quote = ""))
arquivosFundo <- bind_rows(arquivosFundo)
saveRDS(arquivosFundo, file = file.path(dirBases,"TEMP","arquivosFundo.rds"))

dadosCadastro <- arquivosTemp[str_detect(arquivosTemp, "cad_fi.csv")]
dadosCadastro <- read.csv2(dadosCadastro, header = T, sep = ";", dec = ".", as.is = T, quote = "")
saveRDS(dadosCadastro, file = file.path(dirBases,"TEMP","dadosCadastro.rds"))

dadosCadFiHistClasse <- arquivosTemp[str_detect(arquivosTemp, "cad_fi_hist_classe")]
dadosCadFiHistClasse <- read.csv2(dadosCadFiHistClasse, header = T, sep = ";", dec = ".", as.is = T, quote = "")
saveRDS(dadosCadFiHistClasse, file = file.path(dirBases,"TEMP","dadosCadastroHistClasse.rds"))

dadosExtrato <- arquivosTemp[str_detect(arquivosTemp, "extrato_fi")]
dadosExtrato <- read.csv2(dadosExtrato, header = T, sep = ";", dec = ".", as.is = T, quote = "")
saveRDS(dadosExtrato, file = file.path(dirBases,"TEMP","dadosExtrato.rds"))

dadosPerfilMensal <- arquivosTemp[str_detect(arquivosTemp, paste0("perfil_mensal_fi_"))]

teste <- list()
for(i in c(1:NROW(dadosPerfilMensal))){
  teste[[i]] <- read.csv2(dadosPerfilMensal[i], header = T, sep = ";", dec = ".", as.is = T, quote = "")
}

dadosPerfilMensalConcat <- bind_rows(teste)
dadosPerfilMensalConcat <- dadosPerfilMensalConcat %>% mutate(DT_COMPTC = ymd(DT_COMPTC)) %>%
  arrange(desc(DT_COMPTC)) %>% mutate(DUP = duplicated(CNPJ_FUNDO)) %>% filter(DUP == F)

saveRDS(dadosPerfilMensalConcat, file = file.path(dirBases,"TEMP","dadosPerfilMensal.rds"))

dadoslaminaFI <- NULL
dadoslaminaFI <- arquivosTemp[str_detect(arquivosTemp, "lamina_fi_")]
dadoslaminaFI <- dadoslaminaFI[!str_detect(dadoslaminaFI, "rentab")]
dadoslaminaFI <- dadoslaminaFI[!str_detect(dadoslaminaFI, "carteira")]
dadoslaminaFI <- dadoslaminaFI[str_detect(dadoslaminaFI, "csv")]

teste <- list()
for(i in c(1:NROW(dadoslaminaFI))){
    teste[[i]] <- read.csv2(dadoslaminaFI[i], header = T, sep = ";", dec = ".", as.is = T, quote = "")
}

dadoslaminaFIconcat <- bind_rows(teste)
dadoslaminaFIconcat <- dadoslaminaFIconcat %>% mutate(DT_COMPTC = ymd(DT_COMPTC)) %>%
  arrange(desc(DT_COMPTC)) %>% mutate(DUP = duplicated(CNPJ_FUNDO)) %>% filter(DUP == F)

saveRDS(dadoslaminaFIconcat, file = file.path(dirBases,"TEMP","dadoslaminaFI.rds"))
