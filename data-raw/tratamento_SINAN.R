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


# leitura da base especifica para cada caso
sinan <- read.dbf('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/SINAN/VIOLENET.DBF', as.is = TRUE)



# pipeline
sinan_2 <- sinan |>
  vitallinkage::drop_duplicados_sinan_1() |>  # Dropa as colunas duplicadas inicialmente
  vitallinkage::padroniza_variaveis(namestand,"SINAN") |> # Padroniza variáveis baseado no df nomestand - ADICIONAR O CRIA COLUNA ID
  vitallinkage::ajuste_data(tipo_data = 2) |> # Ajustando o formato de data
  vitallinkage::ano_sinan() |>
  vitallinkage::nu_idade_anos_sinan() |> # ANO NASC SINAN
  vitallinkage::as_char() |>  # Transformar todos em texto
  vitallinkage::ajuste_txt() |> # Ajusta os textos de nomes
  vitallinkage::limpa_ignorados_sim() |> # Remove textos de ignorado
  vitallinkage::soundex_linkage("ds_nome_pac") |>
  vitallinkage::soundex_linkage("ds_nome_mae") |>
  vitallinkage::ajuste_res() |> # Ajusta as variáveis que contem "_res" na composição
  vitallinkage::soundex_linkage("ds_bairro_res") |>
  vitallinkage::soundex_linkage("ds_rua_res") |>
  vitallinkage::soundex_linkage("ds_comple_res") |>
  vitallinkage::soundex_linkage("ds_ref_res") |>
  # NOVAS VARIÁVEIS
  vitallinkage::ds_raca_sinan() |> # Cria coluna descrevendo as raça no sinan
  vitallinkage::corrige_sg_sexo() |> # Corrige os registros de sexo Ignorado
  vitallinkage::nu_idade_anos_sinan() |> # Adiciona a coluna com a idade calculada
  vitallinkage::as_char() #|>  # Transformar todos em texto
  # vitallinkage::corrige_datas() |>  # Transforma em data as colunas que comecem com "dt_"
  # vitallinkage::ajuste_cd_to_int()

# remove colunas com dados pessoais
sinan_anon <- sinan |>
  vitallinkage::sinan_anon()

a <- sinan[grepl("AAA", sinan$NM_PACIENT), ]
b <- sinan_2[grepl("LIDIANE DOS SANTOS TERTULIANO", sinan_2$ds_nome_pac), ]
sinan_2$nome
f <- sinan_2 |> dplyr::filter(nu_not == 1869955)
sinan_2$dt_nasc
sinan_2
view(sinan_2 |> dplyr::select(dt_nasc, ano_nasc))
