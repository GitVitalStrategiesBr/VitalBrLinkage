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
devtools::load_all(".")


# Specify the file path
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

sim<-sim_raw

sim_2 <- sim |>
  vitallinkage::limpa_ignorados_sim() |> # Remove textos de ignorado
  vitallinkage::padroniza_variaveis(namestand, "SIM") |> # Padronizando os nomes das variáveis
  vitallinkage::ajuste_data(tipo_data = 1) |> # Ajustando o formato de data
  vitallinkage::ano_sim() |> # Adicionando o ano
  vitallinkage::as_char() |> # Transformando todos em character
  vitallinkage::ajuste_txt() |> # Ajusta as variáveis que contem "nome" na composição
  vitallinkage::gemelar("ds_nome_pac") |>
  vitallinkage::soundex_linkage("ds_nome_pac") |>
  vitallinkage::soundex_linkage("ds_nome_pai") |>
  vitallinkage::soundex_linkage("ds_nome_mae") |>
  vitallinkage::ajuste_res() |> # Ajusta as variáveis que contem "_res" na composição
  vitallinkage::soundex_linkage("ds_bairro_res") |>
  vitallinkage::soundex_linkage("ds_rua_res") |>
  vitallinkage::soundex_linkage("ds_comple_res") |>
  vitallinkage::drop_duplicados_sim_padronizado() |> # Série de lógicas para remover duplicados
  ## NOVAS VARIÁVEIS
  vitallinkage::ds_raca_sim() |> # Ajustando a raça/cor
  vitallinkage::corrige_sg_sexo() |> # Ajustando a variável sg_sexo
  vitallinkage::nu_idade_anos_sim() |> # Ajustanso a idade em anos
  vitallinkage::as_char()  # Tudo como character

# Anonimização
sim_anon <- sim_2  |>
  vitallinkage::sim_anon()

a <- sim_2 |> filter(gemelar==1)

sim_3 <- sim_2 |> vitallinkage::ajuste_res()

sim_a <- sim_2 |> dplyr::select(ds_bairro_res, ds_comple_res, ds_rua_res, cd_cep_res,nu_num_res, cd_mun_res, ds_comple_res)

sim_b <- sim_3 |> dplyr::select(ds_bairro_res, ds_comple_res, ds_rua_res, cd_cep_res,nu_num_res, cd_mun_res, ds_comple_res)

sim_3$ds_comple_res
a <- sim_a[grepl("[^\\x00-\\x7F] ", sim_a$ds_bairro_res), ]
b <- sim_b[grepl("[^\\x00-\\x7F]", sim_b$ds_bairro_res), ]+

usethis::use_data_raw("tratamento_SIM")

