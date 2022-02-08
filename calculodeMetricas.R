library(bizdays)
library(dplyr)
library(lubridate)
library(stringr)
library(PortfolioAnalytics)

#################################GERA√á√ÉO DA TABELA DE DADOS CADASTRAIS PARA O PRODINVEST#####################################

dirBases <- "" #setar o diretÛrio aonde as bases foram salvas
dadosFinan <<- readRDS(file.path(dirBases,"TEMP","dadosFinan.rds"))
arquivosFundo <<-readRDS(file.path(dirBases,"TEMP","arquivosFundo.rds"))
dadosCadastro <<-readRDS(file.path(dirBases,"TEMP","dadosCadastro.rds"))
dadosExtrato <<-readRDS(file.path(dirBases,"TEMP","dadosExtrato.rds"))
dadosPerfilMensal <<-readRDS(file.path(dirBases,"TEMP","dadosPerfilMensal.rds"))
dadoslaminaFI <<-readRDS(file.path(dirBases,"TEMP","dadoslaminaFI.rds"))
dadosCadFiHistClasse <- readRDS(file.path(dirBases,"TEMP","dadosCadastroHistClasse.rds"))

listaFundos <- file.path(dirBases,"listaFundosv2.xlsx")

listaFundos <- readxl::read_excel(listaFundos)


dadosCadastroSec <- dadosCadastro %>% filter(CNPJ_FUNDO %in% listaFundos$`CNPJ do Fundo`) %>% filter(SIT == "EM FUNCIONAMENTO NORMAL") %>%
  select(CNPJ_FUNDO, DENOM_SOCIAL, GESTOR, CPF_CNPJ_GESTOR, ADMIN, CNPJ_ADMIN, CUSTODIANTE, CNPJ_CUSTODIANTE, AUDITOR, CNPJ_AUDITOR,
         DT_CONST, DT_INI_ATIV, TAXA_ADM, TAXA_PERFM, CONDOM)

dadosCadFiHistClasseSec <- dadosCadFiHistClasse %>% filter(CNPJ_FUNDO %in% dadosCadastroSec$CNPJ_FUNDO) %>% mutate(DT_FIM_CLASSE =ymd(DT_FIM_CLASSE)) %>%
  filter(is.na(DT_FIM_CLASSE)) %>% select(CNPJ_FUNDO, CLASSE)

dadosExtratoSec <- dadosExtrato %>% filter(CNPJ_FUNDO %in% dadosCadastroSec$CNPJ_FUNDO) %>%
  select(CNPJ_FUNDO, DISTRIB, CLASSE_ANBIMA, TAXA_CUSTODIA_MAX, TAXA_INGRESSO_PR, TAXA_SAIDA_PR, APLIC_MIN)

dadoslaminaFISEC <- dadoslaminaFI %>% filter(CNPJ_FUNDO %in% dadosCadastroSec$CNPJ_FUNDO) %>%
  select(CNPJ_FUNDO, INDICE_REFER, INVEST_ADIC, RESGATE_MIN, VL_MIN_PERMAN, CONVERSAO_COTA_CANC, QT_DIA_PAGTO_RESGATE,
         QT_DIA_CONVERSAO_COTA_COMPRA, QT_DIA_CONVERSAO_COTA_RESGATE, QT_DIA_PAGTO_RESGATE, TP_DIA_PAGTO_RESGATE,
         QT_DIA_CAREN, HORA_APLIC_RESGATE)

dadosFinancSec <- dadosFinan %>% filter(CNPJ_FUNDO %in% dadosCadastroSec$CNPJ_FUNDO) %>% mutate(DT_COMPTC = ymd(DT_COMPTC)) %>%
  select(CNPJ_FUNDO, DT_COMPTC, VL_QUOTA, VL_PATRIM_LIQ, CAPTC_DIA, RESG_DIA, NR_COTST) %>% arrange(DT_COMPTC)

dtMax <- max(unique(dadosFinancSec$DT_COMPTC))

dadosFinancSec <- dadosFinancSec %>% filter(DT_COMPTC == dtMax)

dadosPerfilMensalSec <- dadosPerfilMensal %>% filter(CNPJ_FUNDO %in% dadosCadastroSec$CNPJ_FUNDO) %>% select(CNPJ_FUNDO, NR_COTST_RPPS)

dadosTotal <-left_join(dadosCadastroSec, dadosExtratoSec) %>% left_join(dadosFinancSec) %>% left_join(dadosPerfilMensalSec) %>%
  left_join(dadoslaminaFISEC) %>% left_join(dadosCadFiHistClasseSec) %>%
  left_join(listaFundos[,c("CNPJ do Fundo", "ENQUADRAMENTO_3922", "SEGMENTO_3922", "ENQUADRAMENTO_4963", "SEGMENTO_4963", "FONTE")], by = c("CNPJ_FUNDO"="CNPJ do Fundo")) 

dadosTotal$DUP <-  duplicated(dadosTotal$CNPJ_FUNDO)
dadosTotal <- dadosTotal %>% filter(DUP==F)
dadosTotal$DUP <-  NULL

write.csv2(dadosTotal, file = file.path(dirBases,"dadosTotal.csv"), row.names = F)

###############################C√ÅLCULO DAS PERFORMANCES DOS FUNDOS###########################################

dadosFinanCalc <- dadosFinan %>% filter(CNPJ_FUNDO %in% dadosCadastroSec$CNPJ_FUNDO) %>% mutate(DT_COMPTC = ymd(DT_COMPTC)) %>%
  select(CNPJ_FUNDO, DT_COMPTC, VL_QUOTA) %>% arrange(DT_COMPTC)

assimetria <-function(x, na.rm = T){  ## coeficiente de assimetria
  if(na.rm == T){
    x <- na.omit(x)
  }
  n<-length(x)
  s<-sd(x)
  m<-mean(x)
  n/((n-1)*(n-2))*sum((x-m)^3)/s^3
}

indiceSharpe <- function(x, rf = 0){
  return((mean(x)*252 - rf)/(sd(x)*sqrt(252)))
}

varHistorico <- function(ret, p=0.05){
  n <- length(ret)
  pos <- floor(n*p)
  if(pos==0){
    pos <- 1
  }
  ret <- ret[order(ret)]
  return(ret[pos])
}

cal <- create.calendar(holidaysANBIMA, weekdays=c('saturday', 'sunday'), name='ANBIMA')
bizdays.options$set(default.calendar=cal)
dt <- add.bizdays(Sys.Date(),-2)

#CALCULAR RETORNO ANO ATUAL
datas <- dadosFinan %>% select(DT_COMPTC) %>% unique(.) %>% mutate(DT_COMPTC = ymd(DT_COMPTC)) %>%
 filter(DT_COMPTC < dmy(paste0("01/01/",year(Sys.Date())))) %>% arrange(DT_COMPTC)

# datas <- dadosFinan %>% select(DT_COMPTC) %>% unique(.) %>% mutate(DT_COMPTC = ymd(DT_COMPTC)) %>% 
#     filter(DT_COMPTC < dmy("01/01/2021")) %>% arrange(DT_COMPTC)

dadosRetAnoAtual <- dadosFinanCalc %>% filter(ymd(DT_COMPTC) >= datas$DT_COMPTC[NROW(datas)]) %>% arrange(CNPJ_FUNDO) %>%
  group_by(CNPJ_FUNDO) %>% mutate(VL_QUOTA = ifelse(VL_QUOTA == 0, lag(VL_QUOTA), VL_QUOTA), Retorno = lag(VL_QUOTA),
                                  Retorno = (VL_QUOTA - Retorno)/Retorno) %>% na.omit()

metricasGerais <- dadosRetAnoAtual %>% group_by(CNPJ_FUNDO) %>% summarise(volAnoAtualAnualizada = round(sd(Retorno)*sqrt(252),4)*100,
                    retornoMedioAnualizado = round(mean(Retorno)*(252),4)*100, retornoMinimoAnoAtual = round(min(Retorno),4)*100,
                    retornoMaximoAnoAtual = round(max(Retorno),4)*100, assimetriaAnoAtual = assimetria(VL_QUOTA),
                    varAnoAtual095 = varHistorico(Retorno)*100, sharpeAnoAtualAnualizado = indiceSharpe(Retorno))

retAcum <- dadosRetAnoAtual %>% mutate(RetAcumAnoAtual = round(cumprod(Retorno+1)-1,4)*100) %>%
  filter(DT_COMPTC == max(dadosRetAnoAtual$DT_COMPTC))

metricasAnoAtual <-  left_join(retAcum, metricasGerais)

#CALCULAR RETORNO M√äS ATUAL
datas <- dadosFinan %>% select(DT_COMPTC) %>% unique(.) %>% mutate(DT_COMPTC = DT_COMPTC) %>%
 filter(DT_COMPTC < dmy(paste0("01/",month(dt),"/",year(dt)))) %>% arrange(DT_COMPTC)

# datas <- dadosFinan %>% select(DT_COMPTC) %>% unique(.) %>% mutate(DT_COMPTC = DT_COMPTC) %>% 
#   filter(DT_COMPTC < dmy("01/12/2021")) %>% arrange(DT_COMPTC)

dadosRetMesAtual <- dadosFinanCalc %>% filter(DT_COMPTC >= datas$DT_COMPTC[NROW(datas)]) %>% arrange(CNPJ_FUNDO) %>%
  group_by(CNPJ_FUNDO) %>% mutate(VL_QUOTA = ifelse(VL_QUOTA == 0, lag(VL_QUOTA), VL_QUOTA), Retorno = lag(VL_QUOTA),
                                  Retorno = (VL_QUOTA - Retorno)/Retorno) %>% na.omit()

metricasGerais <- dadosRetMesAtual %>% group_by(CNPJ_FUNDO) %>% summarise(volMesAtualAnualizada = round(sd(Retorno)*sqrt(252),4)*100,
                    retornoMedioMesAtualAnualizado = round(mean(Retorno)*(252),4)*100, retornoMinimoMesAtual = round(min(Retorno),4)*100,
                    retornoMaximoMesAtual = round(max(Retorno),4)*100, assimetriaMesAtual = assimetria(VL_QUOTA),
                    varMesAtual095 = varHistorico(Retorno)*100, sharpeMesAtualAnualizado = indiceSharpe(Retorno))

retAcum <- dadosRetMesAtual %>% mutate(RetAcumMesAtual = round(cumprod(Retorno+1)-1,4)*100) %>%
  filter(DT_COMPTC == max(dadosRetMesAtual$DT_COMPTC))

metricasMesAtual <-  left_join(retAcum, metricasGerais)

#CALCULAR RETORNO √öLTIMOS 12 MESES
dadosRet12m <- dadosFinanCalc %>% filter(DT_COMPTC >= datas$DT_COMPTC[NROW(datas)-252]) %>% arrange(CNPJ_FUNDO) %>%
  group_by(CNPJ_FUNDO) %>% mutate(VL_QUOTA = ifelse(VL_QUOTA == 0, lag(VL_QUOTA), VL_QUOTA), Retorno = lag(VL_QUOTA),
                                  Retorno = (VL_QUOTA - Retorno)/Retorno) %>% na.omit()

metricasGerais <- dadosRet12m %>% group_by(CNPJ_FUNDO) %>% summarise(volUlt12MAnualizada = round(sd(Retorno)*sqrt(252),4)*100,
                    retornoMedioUlt12MAnualizado = round(mean(Retorno)*(252),4)*100, retornoMinimoUlt12M = round(min(Retorno),4)*100,
                    retornoMaximoUlt12M = round(max(Retorno),4)*100, assimetriaUlt12M = assimetria(VL_QUOTA),
                    varUlt12M095 = varHistorico(Retorno)*100, sharpeUlt12MAnualizado = indiceSharpe(Retorno))

retAcum <- dadosRet12m %>% mutate(RetAcumUlt12M = round(cumprod(Retorno+1)-1,4)*100) %>% filter(DT_COMPTC == max(dadosRet12m$DT_COMPTC))

metricasUlt12M = left_join(retAcum, metricasGerais)

#CALCULAR RETORNO ANO ANTERIOR
dtLimInf <- datas %>% filter(DT_COMPTC < dmy(paste0("01/01/",(year(dt)-1)))) %>% arrange()
#dtLimInf <- datas %>% filter(DT_COMPTC < dmy("01/01/2020")) %>% arrange()
dtLimInf <- datas$DT_COMPTC[NROW(dtLimInf)]
dtLimSup <- datas %>% filter(DT_COMPTC <= dmy(paste0("31/01/",(year(Sys.Date())-1)))) %>% arrange()
#dtLimSup <- datas %>% filter(DT_COMPTC <= dmy("31/12/2020")) %>% arrange()
dtLimSup <- datas$DT_COMPTC[NROW(dtLimSup)]

dadosAnoAnterior <- dadosFinanCalc %>% filter(DT_COMPTC >= dtLimInf) %>% filter(DT_COMPTC <= dtLimSup) %>% arrange(CNPJ_FUNDO) %>%
  group_by(CNPJ_FUNDO) %>% mutate(VL_QUOTA = ifelse(VL_QUOTA == 0, lag(VL_QUOTA), VL_QUOTA), Retorno = lag(VL_QUOTA),
                                  Retorno = (VL_QUOTA - Retorno)/Retorno) %>% na.omit()

metricasGerais <- dadosAnoAnterior %>% group_by(CNPJ_FUNDO) %>% summarise(volAnoAnteriorAnualizada = round(sd(Retorno)*sqrt(252),4)*100,
                    retornoMedioAnoAnteriorAnualizado = round(mean(Retorno)*(252),4)*100, retornoMinimoAnoAnterior = round(min(Retorno),4)*100,
                    retornoMaximoAnoAnterior = round(max(Retorno),4)*100, assimetriaAnoAnterior = assimetria(VL_QUOTA),
                    varAnoAnterior095 = varHistorico(Retorno)*100,  sharpeAnoAnteriorAnualizado = indiceSharpe(Retorno))

retAcum <- dadosAnoAnterior %>% mutate(RetAcumAnoAnterior = round(cumprod(Retorno+1)-1,4)*100) %>%
  filter(DT_COMPTC == max(dadosAnoAnterior$DT_COMPTC))

metricasAnoAnterior <-  left_join(retAcum, metricasGerais)

#JUN√á√ÉO DE TODAS AS M√âTRICAS
metricasTotais <- left_join(metricasMesAtual[,c(-2,-3,-4)], metricasAnoAtual[,c(-2,-3,-4)], by = "CNPJ_FUNDO") %>%
                    left_join(metricasUlt12M[,c(-2,-3,-4)], by = "CNPJ_FUNDO") %>%
                    left_join(metricasAnoAnterior[,c(-2,-3,-4)], by = "CNPJ_FUNDO")

baseTotal <- left_join(dadosTotal, metricasTotais, by = "CNPJ_FUNDO")

##############CRIANDO A RESOLUCAO 3922###########################

baseTotal3922 <- baseTotal %>% select(CNPJ_FUNDO, DENOM_SOCIAL, GESTOR, CPF_CNPJ_GESTOR, ADMIN, CNPJ_ADMIN, CUSTODIANTE,
                                      CNPJ_CUSTODIANTE, TAXA_ADM, TAXA_PERFM, TAXA_INGRESSO_PR, TAXA_SAIDA_PR, VL_PATRIM_LIQ, QT_DIA_PAGTO_RESGATE,
                                      QT_DIA_CONVERSAO_COTA_COMPRA, QT_DIA_CONVERSAO_COTA_RESGATE, TP_DIA_PAGTO_RESGATE,ENQUADRAMENTO_3922, SEGMENTO_3922,
                                      FONTE, RetAcumMesAtual, RetAcumAnoAtual, RetAcumUlt12M, RetAcumAnoAnterior)

names(baseTotal3922)[names(baseTotal3922)=="SEGMENTO_3922"] <- "SEGMENTO"
names(baseTotal3922)[names(baseTotal3922)=="ENQUADRAMENTO_3922"] <- "ENQUADRAMENTO"

baseTotal3922$RESOLUCAO <- "ResoluÁ„o 3922"


##############CRIANDO A RESOLUCAO 4963###########################

baseTotal4963 <- baseTotal %>% select(CNPJ_FUNDO, DENOM_SOCIAL, GESTOR, CPF_CNPJ_GESTOR, ADMIN, CNPJ_ADMIN, CUSTODIANTE,
                                      CNPJ_CUSTODIANTE, TAXA_ADM, TAXA_PERFM, TAXA_INGRESSO_PR, TAXA_SAIDA_PR, VL_PATRIM_LIQ, QT_DIA_PAGTO_RESGATE,
                                      QT_DIA_CONVERSAO_COTA_COMPRA, QT_DIA_CONVERSAO_COTA_RESGATE, TP_DIA_PAGTO_RESGATE,ENQUADRAMENTO_4963, SEGMENTO_4963,
                                      FONTE, RetAcumMesAtual, RetAcumAnoAtual, RetAcumUlt12M, RetAcumAnoAnterior)

names(baseTotal4963)[names(baseTotal4963)=="SEGMENTO_4963"] <- "SEGMENTO"
names(baseTotal4963)[names(baseTotal4963)=="ENQUADRAMENTO_4963"] <- "ENQUADRAMENTO"

baseTotal4963$RESOLUCAO <- "ResoluÁ„o 4963"

table(baseTotal4963$SEGMENTO)
baseArquivo <- rbind(baseTotal3922, baseTotal4963)


#############################UNIDOS AS BASES#############################
source(file = file.path(dirBases, "explosaofundos.R"))
explosao_fundos <- readRDS(file.path(dirBases,"TEMP","bases_empilhadas.rds"))
explosao_fundos <-  explosao_fundos %>% filter(CNPJ_FUNDO %in% baseArquivo$CNPJ_FUNDO)
explosao_fundos$PESO <- ifelse(explosao_fundos$VL_MERC_POS_FINAL==0,0,explosao_fundos$PESO)
arquivosExcel <- list()

arquivosExcel[[1]] <- baseTotal[,c(1:NCOL(dadosTotal))]
arquivosExcel[[1]]$DTGeracao <- Sys.Date()
  
arquivosExcel[[2]] <- baseTotal[,c(1,(NCOL(dadosTotal)+1):NCOL(baseTotal))]
arquivosExcel[[2]]$DTGeracao <- Sys.Date()

#arquivosExcel[[3]] <- baseTotal %>% select(CNPJ_FUNDO, DENOM_SOCIAL, GESTOR, CPF_CNPJ_GESTOR, ADMIN, CNPJ_ADMIN, CUSTODIANTE,
##                        CNPJ_CUSTODIANTE, TAXA_ADM, TAXA_PERFM, TAXA_INGRESSO_PR, TAXA_SAIDA_PR, VL_PATRIM_LIQ, QT_DIA_PAGTO_RESGATE,
#                        QT_DIA_CONVERSAO_COTA_COMPRA, QT_DIA_CONVERSAO_COTA_RESGATE, TP_DIA_PAGTO_RESGATE,RESOLUCAO, ENQUADRAMENTO_3922, SEGMENTO_3922,
#                        ENQUADRAMENTO_4963, SEGMENTO_4963, FONTE, RetAcumMesAtual, RetAcumAnoAtual, RetAcumUlt12M, RetAcumAnoAnterior)

arquivosExcel[[3]] <- baseArquivo
arquivosExcel[[3]]$DTGeracao <- Sys.Date()

arquivosExcel[[4]] <- explosao_fundos
arquivosExcel[[4]]$DTGeracao <- Sys.Date()

openxlsx::write.xlsx(arquivosExcel, file.path(dirBases,"BaseProdInvest.xlsx"), asTable = c(T,T,T,T), 
                     sheetName = c("Dados Administrativos", "MÈtricas Financeiras", "Tabela Prod Invest", "Carteira Fundos"), overwrite = T)
