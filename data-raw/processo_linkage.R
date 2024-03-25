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

load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/base_concatenada_rn.RData')

namestand<-vitallinkage::namestand

concat$nu_doc <- gsub("[^0-9]", NA, concat$nu_doc)
concat$nu_doc[concat$nu_doc == ""] <- NA

# Regra para iniciar o linkage
df <- concat |>
  vitallinkage::start_linkage(c('ds_nome_pac', 'dt_nasc'), "ds_nome_pac")

# Regra 2
df_2 <- df |>
  vitallinkage::regras_linkage(
    c('ds_nome_mae_sound', 'ds_nome_pai_sound', 'dt_nasc'),
    c('ds_nome_pac'),
    2
  )


# Regra 3
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_mae_sound', 'ds_nome_pai_sound', 'dt_nasc'),
    c('ds_nome_pac'),
    3
  )


# Regra 5
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'nu_doc'),
    c('ds_nome_pac_sound'),
    5
  )



gc()
# 1º condição
df <- concat |>
  arrange(ds_nome_pac, dt_nasc) |>
  group_by(ds_nome_pac, dt_nasc) |>
  mutate(N_par = ifelse(!is.na(dt_nasc) & !is.na(ds_nome_pac) &
                          ((ds_nome_pac == lag(ds_nome_pac)) | (ds_nome_pac == lead(ds_nome_pac))), n(), NA))  |>
  mutate(regra1 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |>
  mutate(par_1 = cur_group_id() * (N_par > 1)) |>
  ungroup() |>
  select(-N_par) |>
  mutate(par_c1 = par_1) |>
  arrange(par_1, banco)


# 2º condição
df <- df |>
  arrange(dt_nasc, ds_nome_mae_sound) |>
  group_by(dt_nasc, ds_nome_mae_sound) |>
  mutate(N_par = ifelse(!is.na(dt_nasc) & !is.na(ds_nome_mae_sound) &
                          ((ds_nome_pac == lag(ds_nome_pac)) | (ds_nome_pac == lead(ds_nome_pac))),
                        n(), NA))  |>
  mutate(regra2 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |>
  mutate(par_2 = ifelse(!is.na(N_par) & N_par > 1, 1 + cur_group_id(), NA),
         par_c2 = par_2) |>
  ungroup() |>
  vitallinkage::meio_de_campo()


# 3º condição
df <- df |>
  arrange(ds_nome_mae_sound, ds_nome_pai_sound, dt_nasc) |>
  group_by(ds_nome_mae_sound, ds_nome_pai_sound, dt_nasc) |>
  mutate(N_par = ifelse(!is.na(ds_nome_mae_sound) & !is.na(ds_nome_pai_sound) & !is.na(dt_nasc) &
                          ((ds_nome_pac == lag(ds_nome_pac)) | (ds_nome_pac == lead(ds_nome_pac))),
                        n(), NA))  |>
  mutate(regra3 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |>
  mutate(par_2 = ifelse(!is.na(N_par) & N_par > 1, 1 + cur_group_id(), NA),
         par_c3 = par_2) |>
  ungroup() |>
  vitallinkage::meio_de_campo()


# 4º condição
df <- df |>
  arrange(ds_nome_pac_sound, dt_nasc, nu_doc) |>
  group_by(ds_nome_pac_sound, dt_nasc, nu_doc) |>
  mutate(N_par = ifelse(!is.na(ds_nome_pac_sound) & !is.na(dt_nasc) & !is.na(nu_doc) &
                          ((ds_nome_pac == lag(ds_nome_pac)) | (ds_nome_pac == lead(ds_nome_pac))),
                        n(), NA))  |>
  mutate(regra4 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |>
  mutate(par_2 = ifelse(!is.na(N_par) & N_par > 1, 1 + cur_group_id(), NA),
         par_c4 = par_2) |>
  ungroup() |>
  vitallinkage::meio_de_campo()

# 5º condição
df <- df |>
  arrange(ds_nome_pac_sound, nu_doc) |>
  group_by(ds_nome_pac_sound, nu_doc) |>
  mutate(N_par = ifelse(!is.na(ds_nome_pac_sound) & !is.na(nu_doc) &
                          ((ds_nome_pac_sound == lag(ds_nome_pac_sound)) | (ds_nome_pac_sound == lead(ds_nome_pac_sound))),
                        n(), NA))  |>
  mutate(regra5 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |>
  mutate(par_2 = ifelse(!is.na(N_par) & N_par > 1, 1 + cur_group_id(), NA),
         par_c5 = par_2) |>
  ungroup() |>
  vitallinkage::meio_de_campo()

# 6º condição
df_r6 <- df |>
  arrange(nu_doc) |>
  group_by(nu_doc) |>
  mutate(N_par = ifelse(!is.na(nu_doc) &
                          ((ds_nome_pac_sound == lag(ds_nome_pac_sound)) | (ds_nome_pac_sound == lead(ds_nome_pac_sound))),
                        n(), NA))  |>
  mutate(regra6 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |>
  mutate(par_2 = ifelse(!is.na(N_par) & N_par > 1, 1 + cur_group_id(), NA),
         par_c6 = par_2) |>
  ungroup() |>
  vitallinkage::meio_de_campo()

