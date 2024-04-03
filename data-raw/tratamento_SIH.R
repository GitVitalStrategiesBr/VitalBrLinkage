library(readstata13)
library(tidyverse)
library(pdftools)
library(data.table)
library(dplyr)
library(readxl)
library(foreign)
library(stringi)
library(SoundexBR) #soundex em portugues
library(janitor)
library(lubridate)
library(writexl)
devtools::load_all(".")


load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/SIH/sih_final_2016_2021.Rdata')


sih <- sih_final_csv


sih_2 <- sih |>
  vitallinkage::upper_case_char() |> # As colunas com texto passam a ficar com letra maiuscula
  vitallinkage::padroniza_variaveis(namestand,'SIH') |> # Padroniza as variáveis
  vitallinkage::ajuste_data(tipo_data = 2) |> # Ajustando o formato de data
  vitallinkage::ano_sih() |> #cria a coluna de ano
  vitallinkage::as_char() |> # Transformando todos em character
  vitallinkage::ajuste_txt() |> # Ajusta os textos de nomes
  #base::unique() |> # Novos valores únicos após o tratamento
  vitallinkage::soundex_linkage("ds_nome_pac") |>
  vitallinkage::soundex_linkage("ds_nome_mae") |>
  vitallinkage::ajuste_res() |>
  vitallinkage::soundex_linkage("ds_bairro_res") |>
  vitallinkage::soundex_linkage("ds_comple_res") |>
  vitallinkage::soundex_linkage("ds_nome_pac_res") |>
  vitallinkage::soundex_linkage("ds_rua_res") |>
  vitallinkage::ds_raca_sih() |> # Cria coluna ds_raca
  vitallinkage::nu_idade_anos_sih() |> # Cria coluna de anos
  vitallinkage::faixa_etaria() |> # Cria coluna de faixa etária
  vitallinkage::dias_int_sih() |> # Dias de internação no SIH
  vitallinkage::as_char() # Transforma todos em character

# Anonimização
sih_anon <-
  sih_2 |>
  vitallinkage::sih_anon() # Anonimiza
sih_anon |> vitaltable::tab_1(faixa_etaria)

a <-  sih[grepl("[A-Za-z]", sih$AH_PACIENTE_NOME), ]
b <-  sih_2[grepl("[A-Za-z]", sih_2$ds_nome_pac), ]

sih_2$ds_rua_res
sih$AH_PACIENTE_LOGR
a <- sih |> dplyr::select(AH_PACIENTE_LOGR) |> rename('bla_res' = AH_PACIENTE_LOGR)
b <- a |> vitallinkage::ajuste_res()
