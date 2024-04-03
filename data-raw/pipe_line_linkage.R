library(readstata13)
library(tidyverse)
library(pdftools)
library(data.table)
library(dplyr)
library(readxl)
library(foreign)
library(stringi)
library(SoundexBR) #soundex
library(janitor)
library(lubridate)
library(writexl)
library(vitaltable)
devtools::load_all(".")
options(scipen = 999)
# Record linkage

########
### SIM
########
# leitura da base especifica para cada caso
# Minha pasta com os dados
path <- 'C:/vitalstrategies/data_sicence/TCC/SCRIPTS_LINKAGE/Erik/SIM'

# lista de arquivos com fim .DBF
dbf_files <- list.files(path, pattern = "\\.DBF$", full.names = TRUE)

# Data Frame para adicionar os dados carregados
sim_raw <- data.frame()

# Loop para carregar e concatenar os dbf
for (file in dbf_files) {
  # Lendo arquivo DBF
  sim_temp <- read.dbf(file) |>
    mutate(ANO=substr(DTOBITO,5,8))

  sim_raw <- bind_rows(sim_raw,sim_temp)
  rm(sim_temp)
}

sim <- vitallinkage::padroniza_SIM(sim_raw)
rm(sim_raw)


########
### SIH
########
# leitura da base especifica para cada caso
load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/SIH/sih_final_2016_2021.Rdata')

sih <- vitallinkage::padroniza_SIH(sih_final_csv)
rm(sih_final_csv)
#sih_2 <- sih |> select(ds_nome_pac, nome_original, ds_nome_mae, nome_mae_original)



##########
### SINAN
##########
# leitura da base especifica para cada caso
sinan <- read.dbf('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/SINAN/VIOLENET.DBF', as.is = TRUE)

sinan <- vitallinkage::padroniza_SINAN(sinan)


#################
### CONCATENANDO
#################

list_of_dfs <- list(sih,sim,sinan)  # Add more data frames if needed
concat <- bind_rows(list_of_dfs)

rm(list_of_dfs)

concat <-
  concat |> vitallinkage::ajuste_data(2) |>
  vitallinkage::atualiza_dt_comum() |>
  mutate(
    gemelar=ifelse(gemelar == 0,NA,1),
    dia_nasc = day(dt_nasc),
    mes_nasc = month(dt_nasc),
    mae_11 = substr(ds_nome_mae, 1, 11),
    nu_doc_copia = nu_doc
  )


concat$nu_doc_copia

save(concat, file = "C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/base_concatenada_rn.RData")

concat$nu_cns <- as.integer(concat$nu_cns)

concat$nu_cns <- as.integer(concat$nu_cns)
concat$cd_autorizador_doc <- as.integer(concat$cd_autorizador_doc)

