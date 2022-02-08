library(bizdays)
library(lubridate)
library(stringr)
library(dplyr)

dados_cvm <- "http://dados.cvm.gov.br/dados/FI/DOC/CDA/DADOS/"
dirBases <- "//madreperola/GOP/COP/01 - PRODUTOS/02 - Semanal - Prod-Invest® (terca-feira)"

cal <- create.calendar(holidaysANBIMA, weekdays=c('saturday', 'sunday'), name='ANBIMA')
bizdays.options$set(default.calendar=cal)
dt <- add.bizdays(Sys.Date(),-2)

arquivo <- "cda_fi_"
extensao <- ".zip"

datas <- dt %m-% months(4:2)


for (i in (1:length(datas))) {
  download.file(url = paste0(dados_cvm,arquivo, format(datas[i], "%Y%m"),extensao),
                destfile = file.path(dirBases, "TEMP", paste0(arquivo, format(datas[i], "%Y%m"), extensao)),
                cacheOK = F)
  unzip(zipfile = file.path(dirBases, "TEMP", paste0(arquivo, format(datas[i], "%Y%m"), extensao)), exdir = file.path(dirBases,"TEMP"))
}


arquivosTemp <- list.files(file.path(dirBases,"TEMP"), full.names = T)

dadosCDA_BLC1 <- arquivosTemp[str_detect(arquivosTemp, "cda_fi_BLC_1")]
arquivos_BLC1 <- lapply(dadosCDA_BLC1, function(x) read.csv2(x, header = T, sep = ";", dec = ".", as.is = T, quote = ""))
arquivos_BLC1 <- bind_rows(arquivos_BLC1)
arquivos_BLC1 <- arquivos_BLC1 %>% mutate(DT_COMPTC = ymd(DT_COMPTC), CNPJ_ISIN = paste0(CNPJ_FUNDO, CD_ISIN), DUP = duplicated(CNPJ_ISIN)) %>%
                  arrange(desc(DT_COMPTC)) %>% filter(DUP==F) %>% mutate(DUP = NULL, DESC_ATIVO = paste(TP_TITPUB, DT_VENC, sep = "_")) %>%
                  select(CNPJ_FUNDO, DENOM_SOCIAL, DT_COMPTC, TP_APLIC, TP_NEGOC, VL_MERC_POS_FINAL, DESC_ATIVO)

##

dadosCDA_BLC2 <- arquivosTemp[str_detect(arquivosTemp, "cda_fi_BLC_2")]
arquivos_BLC2 <- lapply(dadosCDA_BLC2, function(x) read.csv2(x, header = T, sep = ";", dec = ".", as.is = T, quote = ""))
arquivos_BLC2 <- bind_rows(arquivos_BLC2)
arquivos_BLC2 <- arquivos_BLC2 %>% mutate(DT_COMPTC = ymd(DT_COMPTC), CNPJ_ISIN = paste0(CNPJ_FUNDO, CNPJ_FUNDO_COTA), DUP = duplicated(CNPJ_ISIN)) %>%
  arrange(desc(DT_COMPTC)) %>% filter(DUP==F) %>% mutate(DUP = NULL, DESC_ATIVO = NM_FUNDO_COTA) %>%
  select(CNPJ_FUNDO, DENOM_SOCIAL, DT_COMPTC, TP_APLIC, TP_NEGOC, VL_MERC_POS_FINAL, DESC_ATIVO)

##

dadosCDA_BLC3 <- arquivosTemp[str_detect(arquivosTemp, "cda_fi_BLC_3")]
arquivos_BLC3 <- lapply(dadosCDA_BLC3, function(x) read.csv2(x, header = T, sep = ";", dec = ".", as.is = T, quote = ""))
arquivos_BLC3 <- bind_rows(arquivos_BLC3)
arquivos_BLC3 <- arquivos_BLC3 %>% mutate(DT_COMPTC = ymd(DT_COMPTC), CNPJ_ISIN = paste0(CNPJ_FUNDO, CD_SWAP, DS_SWAP), DUP = duplicated(CNPJ_ISIN)) %>%
  arrange(desc(DT_COMPTC)) %>% filter(DUP==F) %>% mutate(DUP = NULL, DESC_ATIVO = paste(CD_SWAP, DS_SWAP, sep = "_")) %>%
  select(CNPJ_FUNDO, DENOM_SOCIAL, DT_COMPTC, TP_APLIC, TP_NEGOC, VL_MERC_POS_FINAL, DESC_ATIVO)

##

dadosCDA_BLC4 <- arquivosTemp[str_detect(arquivosTemp, "cda_fi_BLC_4")]
arquivos_BLC4 <- lapply(dadosCDA_BLC4, function(x) read.csv2(x, header = T, sep = ";", dec = ".", as.is = T, quote = ""))
arquivos_BLC4 <- bind_rows(arquivos_BLC4)
arquivos_BLC4 <- arquivos_BLC4 %>% mutate(DT_COMPTC = ymd(DT_COMPTC), CNPJ_ISIN = paste0(CNPJ_FUNDO, CD_ISIN), DUP = duplicated(CNPJ_ISIN)) %>%
  arrange(desc(DT_COMPTC)) %>% filter(DUP==F) %>% mutate(DUP = NULL, DESC_ATIVO = paste(CD_ATIVO, DS_ATIVO, sep = "_")) %>%
  select(CNPJ_FUNDO, DENOM_SOCIAL, DT_COMPTC, TP_APLIC, TP_NEGOC, VL_MERC_POS_FINAL, DESC_ATIVO)

##

dadosCDA_BLC5 <- arquivosTemp[str_detect(arquivosTemp, "cda_fi_BLC_5")]
arquivos_BLC5 <- lapply(dadosCDA_BLC5, function(x) read.csv2(x, header = T, sep = ";", dec = ".", as.is = T, quote = ""))
arquivos_BLC5 <- bind_rows(arquivos_BLC5)
arquivos_BLC5 <- arquivos_BLC5 %>% mutate(DT_COMPTC = ymd(DT_COMPTC), CNPJ_ISIN = paste0(CNPJ_FUNDO, CNPJ_EMISSOR), DUP = duplicated(CNPJ_ISIN)) %>%
  arrange(desc(DT_COMPTC)) %>% filter(DUP==F) %>% mutate(DUP = NULL, DESC_ATIVO = paste(CNPJ_EMISSOR, EMISSOR, sep = "_")) %>%
  select(CNPJ_FUNDO, DENOM_SOCIAL, DT_COMPTC, TP_APLIC, TP_NEGOC, VL_MERC_POS_FINAL, DESC_ATIVO)

##

dadosCDA_BLC6 <- arquivosTemp[str_detect(arquivosTemp, "cda_fi_BLC_6")]
arquivos_BLC6 <- lapply(dadosCDA_BLC6, function(x) read.csv2(x, header = T, sep = ";", dec = ".", as.is = T, quote = ""))
arquivos_BLC6 <- bind_rows(arquivos_BLC6)
arquivos_BLC6 <- arquivos_BLC6 %>% mutate(DT_COMPTC = ymd(DT_COMPTC), CNPJ_ISIN = paste0(CNPJ_FUNDO, EMISSOR, DT_VENC), DUP = duplicated(CNPJ_ISIN)) %>%
  arrange(desc(DT_COMPTC)) %>% filter(DUP==F) %>% mutate(DUP = NULL, DESC_ATIVO = paste(EMISSOR, DT_VENC, sep = "_")) %>%
  select(CNPJ_FUNDO, DENOM_SOCIAL, DT_COMPTC, TP_APLIC, TP_NEGOC, VL_MERC_POS_FINAL, DESC_ATIVO)

##

dadosCDA_BLC7 <- arquivosTemp[str_detect(arquivosTemp, "cda_fi_BLC_7")]
arquivos_BLC7 <- lapply(dadosCDA_BLC7, function(x) read.csv2(x, header = T, sep = ";", dec = ".", as.is = T, quote = ""))
arquivos_BLC7 <- bind_rows(arquivos_BLC7)
arquivos_BLC7 <- arquivos_BLC7 %>% mutate(DT_COMPTC = ymd(DT_COMPTC), CNPJ_ISIN = paste0(CNPJ_FUNDO, EMISSOR, DT_VENC), DUP = duplicated(CNPJ_ISIN)) %>%
  arrange(desc(DT_COMPTC)) %>% filter(DUP==F) %>% mutate(DUP = NULL, DESC_ATIVO = paste(EMISSOR, DT_VENC, sep = "_")) %>%
  select(CNPJ_FUNDO, DENOM_SOCIAL, DT_COMPTC, TP_APLIC, TP_NEGOC, VL_MERC_POS_FINAL, DESC_ATIVO)

##

dadosCDA_BLC8 <- arquivosTemp[str_detect(arquivosTemp, "cda_fi_BLC_8")]
arquivos_BLC8 <- lapply(dadosCDA_BLC8, function(x) read.csv2(x, header = T, sep = ";", dec = ".", as.is = T, quote = ""))
arquivos_BLC8 <- bind_rows(arquivos_BLC8)
arquivos_BLC8 <- arquivos_BLC8 %>% mutate(DT_COMPTC = ymd(DT_COMPTC), CNPJ_ISIN = paste0(CNPJ_FUNDO, CPF_CNPJ_EMISSOR, EMISSOR), DUP = duplicated(CNPJ_ISIN)) %>%
  arrange(desc(DT_COMPTC)) %>% filter(DUP==F) %>% mutate(DUP = NULL, DESC_ATIVO = paste(CPF_CNPJ_EMISSOR, EMISSOR, sep = "_")) %>%
  select(CNPJ_FUNDO, DENOM_SOCIAL, DT_COMPTC, TP_APLIC, TP_NEGOC, VL_MERC_POS_FINAL, DESC_ATIVO)

##

bases_empilhadas <- bind_rows(arquivos_BLC1, arquivos_BLC2, arquivos_BLC3, arquivos_BLC4, arquivos_BLC5, arquivos_BLC6, arquivos_BLC7, arquivos_BLC8)
base_valor_total <- bases_empilhadas %>% group_by(CNPJ_FUNDO) %>% summarise(VOLAR_TOTAL = sum(VL_MERC_POS_FINAL))
bases_empilhadas <- left_join(bases_empilhadas, base_valor_total)
bases_empilhadas$PESO <- round(bases_empilhadas$VL_MERC_POS_FINAL/bases_empilhadas$VOLAR_TOTAL,4)*100

saveRDS(bases_empilhadas, file = file.path(dirBases,"TEMP","bases_empilhadas.rds"))
