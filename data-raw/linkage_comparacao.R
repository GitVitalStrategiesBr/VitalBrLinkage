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

df_2 <- df_2 |> mutate(ignora_maria = ifelse(ds_nome_mae1 == 'MARIA', NA, 1)
)

a <- df_2 |> select(ds_nome_mae, ds_nome_mae1, ignora_maria) |> filter(is.na(ignora_maria))

load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/base_concatenada_rn.RData')


# Preparação de algumas variáveis complementares
dados_linkage <- concat |>
  vitallinkage::ajusta_doc() |>
  mutate(
    nu_cpf = ifelse(str_detect(nu_cpf, "^[0-9.]+$"), as.numeric(nu_cpf), NA),
    ignora_maria = ifelse(ds_nome_mae1 == 'MARIA', NA, 1),
    ignora_francisca = ifelse(ds_nome_mae1 == 'FRANCISCA', NA, 1),
    ignora_josefa = ifelse(ds_nome_mae1 == 'JOSEFA', NA, 1),
    morreu = ifelse(!is.na(dt_obito), 1, NA)

    )

# Regra para iniciar o linkage
df <- dados_linkage |>
  #filter(grepl("^A|^B|^C|^D", ds_nome_pac)) |> # Recortando uma amostra com a letra A ou B
  vitallinkage::start_linkage(c('nu_cpf', 'dt_nasc'), "ds_nome_mae1_sound")

# Regra 2 - Melhorar, olhar o casdo do
df_2 <- df |>
  vitallinkage::regras_linkage(
    c('nu_cns', 'dt_nasc', 'ds_nome_pac_sound'),
    c('nu_cns'),
    2
  )

# Regra 3 Evento único
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('dt_nasc', 'dt_obito', 'ds_nome_pac_sound'),
    c('ds_nome_mae_sound'),
    3
  )


# Regra 4
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('dt_nasc', 'dt_obito', 'cd_mun_res', 'sg_sexo', 'ds_nome_pac1_sound'),
    c('ds_nome_pac_sound'),
    4
  )


# Regra 5
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('dt_nasc', 'dt_obito', 'ds_nome_pac2', 'ds_nome_mae_sound' ),
    c('ds_nome_pac_sound'),
    5
  )

# Regra 6 - PEGOU DUPLICADO
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('dt_obito','ano_nasc', 'mes_nasc','sg_sexo',
      'ds_nome_pac1_sound','ds_nome_pac3','ignora_maria',
      'ignora_francisca', 'ignora_josefa'),
    c('ds_nome_mae_sound'),
    6
  )

# Regra 7
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('nu_idade_anos', 'ds_nome_pac1', 'ds_nome_pac2', 'ds_nome_mae2', 'cd_mun_res' ),
    c('nu_cpf'),
    7
  )

# Regra 8
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'ds_nome_mae2', 'ds_nome_mae_sound', 'nu_idade_anos', 'cd_mun_not'),
    c('ds_nome_pac'),
    8
  )

# Regra 9
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('dt_nasc', 'dt_obito', 'cd_mun_res', 'ds_nome_pac3', 'sg_sexo', 'ds_nome_pac1'),
    c('ds_nome_mae_sound'),
    9
  )


# Regra 10
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac', 'nu_idade_anos', 'cd_mun_res', 'ds_bairro_res', 'dt_nasc'),
    c('ds_nome_pac'),
    10
  )

# Regra 11
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac2', 'nu_idade_anos', 'ds_rua_res', 'sg_sexo', 'dt_nasc'),
    c('ds_nome_mae2'),
    11
  )

# Regra 12
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac1', 'ds_nome_pac2', 'nu_cpf'),
    c('ds_nome_mae1_sound'),
    12
  )


# Regra 13 - pega mãe e filho
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'ds_nome_pac2', 'nu_cpf'),
    c('nu_cpf'),
    13
  )


# Regra 14
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'ds_nome_mae1', 'ds_nome_pac1_sound', 'ds_nome_pac2_sound', 'nu_cpf'),
    c('ds_nome_pac_sound'),
    14
  )


# Regra 15
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('cd_cep_res', 'ds_nome_pac1', 'ds_nome_pac_sound', 'dt_nasc'),
    c('nu_cpf'),
    15
  )

# Regra 16
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('cd_cep_res', 'ds_nome_pac1', 'ds_nome_mae_sound', 'dt_nasc', 'ds_nome_pac3'),
    c('ds_nome_mae1'),
    16
  )

# Regra 17
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('cd_mun_not', 'sg_sexo', 'ds_nome_pac2', 'ds_nome_mae_sound', 'dt_nasc', 'nu_idade_anos', 'ds_bairro_res'),
    c('ds_nome_mae_sound'),
    17
  )

# Regra 18 - Nenhum novo
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('cd_mun_not', 'sg_sexo', 'ds_nome_pac2', 'ds_nome_mae_sound', 'dt_nasc', 'nu_idade_anos', 'ds_bairro_res'),
    c('ds_nome_mae_sound'),
    18
  )

# Regra 19
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('cd_mun_res', 'sg_sexo', 'ds_nome_pac_sound', 'dt_nasc', 'nu_idade_anos', 'ds_bairro_res'),
    c('ds_nome_mae_sound'),
    19
  )


# Regra 20
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('cd_mun_res', 'sg_sexo', 'ds_nome_pac_sound', 'dt_nasc', 'nu_idade_anos', 'ds_bairro_res'),
    c('ds_nome_mae_sound'),
    20
  )



# Regra 21 nada novo
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('dt_nasc', 'sg_sexo', 'ds_nome_mae_sound', 'nu_idade_anos', 'ds_bairro_res', 'dt_obito'),
    c('ds_nome_pac_sound'),
    21
  )

# Regra 22 nada novo
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'sg_sexo', 'dt_nasc', 'ds_nome_pac2', 'cd_mun_res', 'ds_nome_mae1'),
    c('nu_cpf'),
    22
  )

# Regra 23 Nada novo
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac1_sound', 'ds_nome_pac2_sound', 'dt_nasc', 'cd_mun_res', 'ds_nome_mae1_sound','ds_nome_mae3_sound'),
    c('nu_cpf'),
    23
  )


# Regra 24
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac1_sound', 'nu_tel', 'ano_nasc', 'mes_nasc'),
    c('ds_nome_mae_sound'),
    24
  )

# Regra 25
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac1_sound', 'nu_tel', 'dt_nasc'),
    c('ds_nome_mae_sound'),
    25
  )


# Regra 26 Pegou bem
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'dt_nasc', 'mae_11', 'nu_tel'),
    c('ds_nome_pac_sound'),
    26
  )

# Regra 27
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('nu_doc', 'ds_nome_pac1', 'dia_nasc'),
    c('nu_cpf'),
    27
  )

rn_linkada_teste <- df_2

## CHECKPOINT
#save(rn_linkada_teste, file = 'C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada.RData')

load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada.RData')

df_2 <- rn_linkada_teste |>
  mutate(
    nao_gemelar = ifelse(is.na(gemelar), 1, NA),
    nome_pac_6 = substr(ds_nome_pac, 1, 11),
    nome_5_12 = substr(ds_nome_pac, 5, 12),
    #dif_ob_nasc = dt_obito-dt_nasc,
    nome_menos_5d = str_sub(ds_nome_pac, end = -6)
  )

rm(rn_linkada_teste)

###########
# Regra 28
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac1', 'ds_nome_mae_sound', 'dt_nasc', 'nu_idade_anos', 'ds_bairro_res_sound'),
    c('ds_nome_pac_sound'),
    28
  )

# Regra 29
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'ds_nome_mae_sound', 'dia_nasc', 'ano_nasc', 'nu_idade_anos', 'nao_gemelar'),
    c('ds_nome_pac1_sound'),
    29
  )


# Regra 30
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac2_sound', 'nu_tel', 'faixa_etaria', 'ds_nome_pac3', 'dia_nasc', 'mes_nasc', 'nao_gemelar'),
    c('ds_nome_mae1_sound'),
    30
  )


# Regra 31
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac2_sound', 'nome_pac_6', 'nu_tel', 'mae_11', 'ds_nome_pac3', 'dia_nasc', 'mes_nasc', 'nao_gemelar'),
    c('ds_nome_mae1_sound'),
    31
  )

# Regra 32
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_mae', 'dia_nasc', 'mes_nasc', 'nao_gemelar', 'nu_cns' ),
    c('ds_nome_pac3_sound'),
    32
  )


# Regra 33
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac2', 'ds_nome_pac3', 'dt_nasc', 'nao_gemelar', 'nome_5_12'),
    c('ds_nome_mae_sound'),
    33
  )


# Regra 34 Não pegou nada
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'dt_nasc', 'ds_nome_mae_sound', 'nu_cpf'),
    c('nu_cpf', 'nu_cns'),
    34
  )

# Regra 35 - demora bastante
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'ds_nome_pac1', 'sg_sexo', 'morreu', 'nu_idade_anos', 'dia_nasc', 'dt_nasc', 'ds_nome_mae1', 'ds_nome_mae3'),
    c('ds_nome_pac_sound'),
    35
  )

## CHECKPOINT 2
#rn_linkada_teste <- df_2
#save(rn_linkada_teste, file = 'C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada.RData')
load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada.RData')

df_2 <- rn_linkada_teste |>
  mutate(
    ignora_francisca_maria = ifelse(ds_nome_mae == 'FRANCISCA MARIA CONCEICAO', NA, 1),
    mae_last8 = substr(ds_nome_mae, nchar(ds_nome_mae) - 7, nchar(ds_nome_mae)),
    mae_menos5d = substr(ds_nome_mae, 1, nchar(ds_nome_mae) - 5),
    nome_menos_2d = str_sub(ds_nome_pac, end = -2)
    ) |>
  group_by(ds_nome_pac3) |>
  mutate(freq_last_nome = n()) |>
  ungroup() |>
  mutate(freq_last_nome_2000 = ifelse(freq_last_nome <= 2000, 1, NA))

rm(rn_linkada_teste)

# Regra 36
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac1', 'ds_nome_pac2', 'sg_sexo', 'nu_idade_anos', 'mes_nasc', 'ano_nasc', 'dia_nasc', 'morreu', 'ds_nome_mae', 'ignora_francisca_maria'),
    c('ds_nome_mae_sound'),
    36
  )


# Regra 37
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac3', 'dt_obito', 'dt_nasc', 'sg_sexo', 'freq_last_nome_2000'),
    c('ds_nome_mae_sound'),
    37
  )

# Regra 38 - Regra ruim
# df_t <- df_2 |>
#   vitallinkage::regras_linkage(
#     c('ds_nome_pac2', 'ds_nome_pac1', 'nu_idade_anos', 'sg_sexo', 'dt_nasc', 'mae_last8', 'morreu'),
#     c('ds_nome_pac_sound'),
#     38
#   )

# Regra 38 - 43ª do stata
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('dia_nasc', 'mes_nasc', 'nu_idade_anos', 'ds_nome_pac2', 'sg_sexo', 'nome_menos_2d'),
    c('ds_nome_mae'),
    38
  )


# Regra 39 44ª do stata
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('mes_nasc', 'ano_nasc','nu_idade_anos', 'ds_nome_pac3', 'ds_nome_pac1', 'nome_menos_2d', 'ds_bairro_res'),
    c('ds_nome_mae'),
    39
  )


# Regra 40 45ª do stata
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac1', 'ds_nome_pac2', 'ano_nasc', 'faixa_etaria', 'ds_bairro_res', 'mae_11'),
    c('ds_nome_mae'),
    40
  )


# Regra 41 46ª do stata -
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'dia_nasc', 'faixa_etaria', 'ds_bairro_res', 'mae_menos5d', 'ds_nome_mae1', 'sg_sexo'),
    c('ds_nome_mae2', 'ds_nome_mae1'),
    41
  )

## CHECK POINT 3
save(df_2, file = 'C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada_41.RData')
load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada_41.RData')


df_2 <- df_2 |>
  mutate(nome_meio_4_10 = substr(ds_nome_pac2, 4, 10),
         mae1_menos4 = substr(ds_nome_mae1, 1, nchar(ds_nome_mae) - 4))

# Regra 42 47ª do stata
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'ds_nome_pac2', 'faixa_etaria', 'mae_menos5d', 'sg_sexo', 'ignora_maria', 'ano_nasc', 'dia_nasc'),
    c('ds_nome_mae2', 'ano_nasc', 'dia_nasc'),
    42
  )


# Regra 43 48ª do stata
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound',  'ano_nasc', 'faixa_etaria', 'ds_nome_mae2', 'ds_nome_mae1', 'sg_sexo', 'dt_nasc' ),
    c('ds_nome_mae2','ds_nome_mae1'),
    43
  )

# Regra 44 49ª do stata
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac2_sound', 'mae_menos5d','nome_5_12' ,'ds_nome_pac1', 'ano_nasc', 'faixa_etaria', 'dia_nasc', 'sg_sexo', 'dt_nasc', 'morreu', 'ignora_maria'),
    c('ds_nome_mae1','ds_nome_mae3'),
    44
  )


# Regra 45 50ª do stata
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac2', 'ds_nome_pac1', 'dt_nasc', 'faixa_etaria', 'mae1_menos4', 'ignora_maria'),
    c('ds_nome_mae1','ds_nome_mae2', 'mae_last8'),
    45
  )

# Regra 46 51ª do stata
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac1', 'ds_nome_pac_sound','dt_nasc', 'mae_11', 'ignora_maria'),
    c('ds_nome_pac_sound', 'mae_last8'),
    46
  )


# Regra 47 52ª do stata
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac1', 'ds_nome_pac_sound','dia_nasc', 'mes_nasc', 'faixa_etaria', 'ignora_maria', 'mae_11'),
    c('ds_nome_pac_sound', 'ds_nome_mae1_sound'),
    47
  )

# Regra 48
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('nu_cns','ds_nome_pac1_sound', 'ds_nome_pac2_sound','dia_nasc', 'mes_nasc', 'faixa_etaria', 'ignora_maria', 'mae_11'),
    c('nu_cns'),
    48
  )


# Regra 49
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('nu_cns','ds_nome_pac1_sound', 'ds_nome_pac3_sound','dt_nasc', 'cd_mun_res'),
    c('mae_11'),
    49
  )


# Regra 50
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('nu_cns','ds_nome_pac1_sound', 'ds_nome_mae1_sound', 'ano_nasc', 'dia_nasc', 'mae_menos5d','ignora_maria'),
    c('nu_cns'),
    50
  )

# Regra 51
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('nu_cns','ds_nome_pac1_sound', 'ds_nome_mae1_sound', 'mes_nasc', 'dia_nasc', 'mae_menos5d','ignora_maria'),
    c('nu_cns'),
    51
  )


# Regra 52
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('nu_cns','ds_nome_pac1_sound', 'ds_nome_mae1_sound', 'ano_nasc', 'mes_nasc', 'mae_menos5d'),
    c('nu_cns'),
    52
  )

# Regras 53
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('nu_cns','ds_nome_pac_sound', 'ds_nome_mae_sound', 'dt_nasc'),
    c('nu_cns'),
    53
  )


# Regra 54
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_bairro_res_sound','cd_mun_res','ds_nome_pac1_sound','ds_nome_pac2_sound', 'mae_menos5d', 'dt_nasc', 'ignora_maria'),
    c('ds_nome_pac1_sound'),
    54
  )

# Regra 55
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_bairro_res_sound','cd_mun_res','ds_nome_pac1_sound','ds_nome_pac3_sound', 'mae_menos5d', 'dt_nasc'),
    c('ds_nome_pac1_sound'),
    55
  )

# Regra 56
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_bairro_res_sound','cd_mun_res','nome_menos_5d', 'mae_menos5d', 'dt_nasc', 'ignora_maria'),
    c('nome_menos_5d'),
    56
  )


# Regra 57
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_bairro_res_sound','cd_mun_res','nome_menos_5d', 'mae_menos5d', 'dt_nasc'),
    c('nome_menos_5d'),
    57
  )

# Regra 58 - stata 54
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound','ds_nome_pac2','dt_notific', 'sg_sexo'),
    c('nu_idade_anos', 'ds_nome_mae1'),
    58
  )

# Regra 59 - stata 55 - demora no processamento
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac1_sound','ds_nome_pac_sound','ds_bairro_res', 'cd_mun_res', 'nu_idade_anos', 'dia_nasc'),
    c('dt_nasc', 'ds_nome_mae1', 'ds_nome_mae2'),
    59
  )

# Regra 60 - stata inspirado no 56
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound','faixa_etaria', 'ano_nasc', 'ds_rua_res_sound','cd_mun_not'),
    c('mae_menos5d'),
    60
  )

save(df_2, file = 'C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada_60.RData')


df_2 <- df_2 |> mutate(
  ignora_maria_pac=ifelse(ds_nome_pac1 == 'MARIA', NA, 1)
)
# Regra 61 - stata inspirado no 56
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound','mae_menos5d', 'dt_nasc', 'ds_rua_res_sound', 'faixa_etaria'),
    c('ds_nome_mae1_sound', 'ds_nome_pac1_sound'),
    61
  )

# Regra 62 - stata inspirado no 57
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac2_sound', 'nome_pac_6','faixa_etaria', 'ano_nasc', 'cd_mun_not', 'sg_sexo', 'ds_nome_mae2', 'ignora_maria'),
    c('ds_nome_pac1_sound'),
    62
  )

# Regra 63 - stata inspirado no 58
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac2_sound', 'nome_pac_6','faixa_etaria', 'mes_nasc', 'cd_mun_not', 'sg_sexo', 'ds_nome_mae2', 'ignora_maria'),
    c('ds_nome_pac1_sound'),
    63
  )

# Regra 64 - stata inspirado no 58
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac2_sound', 'ds_nome_mae1','nome_pac_6','faixa_etaria', 'dia_nasc', 'cd_mun_not', 'sg_sexo', 'ds_nome_mae2', 'ignora_maria'),
    c('ds_nome_pac1_sound'),
    64
  )

# Regra 65 - stata inspirado no 59
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('dt_nasc', 'cd_mun_not', 'sg_sexo', 'nome_menos_5d','ds_nome_pac2'),
    c('ds_nome_mae1', 'ds_nome_mae2'),
    65
  )



#save(df_2, file = 'C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada_65.RData')

load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada_65.RData')

df_2<- df_2 |>
  mutate(
    nome_meio_4_10 = substr(ds_nome_pac2, 5, 12)
  )

# Regra 66 - stata inspirado no 59
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('dt_nasc', 'cd_mun_not', 'sg_sexo', 'nome_menos_5d','ds_nome_pac2'),
    c('ds_nome_mae1', 'ds_nome_mae2'),
    66
  )

# Regra 67 - stata inspirado no 60
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('mes_nasc', 'ano_nasc', 'cd_mun_not','sg_sexo', 'ds_nome_pac1_sound', 'ds_rua_res'),
    c('ds_nome_mae2', 'ds_nome_mae1'),
    67
  )


# Regra 68 - stata inspirado no 62
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac', 'ds_nome_mae_sound', 'dt_obito'),
    c('ds_nome_mae2', 'ds_nome_mae1'),
    68
  )


# Regra 69 - stata inspirado no 63
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac1','ds_nome_pac2','ds_nome_pac3','nome_meio_4_10', 'ds_nome_mae', 'dia_nasc', 'mes_nasc'),
    c('ds_nome_mae'),
    69
  )


# Regra 70 - stata inspirado no 64
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac1','ds_nome_pac2','ds_nome_pac3','nome_meio_4_10', 'ds_nome_mae', 'dia_nasc', 'ano_nasc'),
    c('ds_nome_mae'),
    70
  )

# CHECK POINT 4
#save(df_2, file = 'C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada_70.RData')
load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada_70.RData')


# # Regra 71 - stata inspirado no 65
# df_t <- df_2 |>
#   vitallinkage::regras_linkage(
#     c('ds_nome_pac1', 'ds_nome_pac2', 'dia_nasc', 'mes_nasc', 'faixa_etaria', 'ds_nome_mae1', 'ignora_francisca_maria', 'ignora_maria'),
#     c('ds_nome_mae2'),
#     71
#   )

# Regra 71 - stata inspirado no 68
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac2', 'ds_nome_pac2_sound', 'dt_nasc', 'nu_idade_anos', 'nome_meio_4_10', 'ignora_maria'),
    c('ds_nome_mae1', 'nu_cns'),
    71
  )

# Regra 72 - stata inspirado no 69
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac3', 'ds_nome_mae_sound', 'dt_nasc', 'cd_mun_res', 'ds_nome_pac2_sound', 'nome_menos_2d'),
    c('ds_nome_pac_sound'),
    72
  )

# Regra 73 - stata inspirado no 71
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac1', 'ds_nome_mae_sound', 'cd_cnes', 'cd_cep_res', 'cd_mun_res', 'dia_nasc', 'mes_nasc', 'ignora_maria'),
    c('ds_nome_pac1'),
    73
  )


# Regra 74 - stata inspirado no 72
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_mae_sound', 'ds_nome_mae', 'ds_rua_res', 'sg_sexo', 'nu_idade_anos', 'ignora_maria' ),
    c('ds_nome_pac'),
    74
  )


# Regra 75 - stata inspirado no 73
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac1', 'ds_nome_mae_sound', 'ds_nome_mae', 'cd_cnes', 'ignora_maria', 'ano_nasc'),
    c('nome_5_12'),
    75
  )


# Regra 76 - stata inspirado no 74
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('nome_menos_5d', 'nome_meio_4_10', 'sg_sexo', 'ds_nome_mae', 'nu_idade_anos', 'cd_cnes'),
    c('ds_nome_pac1'),
    76
  )

#save(df_2, file = 'C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada_76.RData')
load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada_76.RData')

# Regra 77
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac', 'ds_nome_mae', 'nu_cns', 'ignora_maria'),
    c('nu_cns'),
    77
  )


# Regra 78 - stata inspirado no 75
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('nu_doc', 'ds_nome_pac2', 'ds_nome_pac3', 'ignora_maria', 'ano_nasc'),
    c('nu_doc'),
    78
  )

# Regra 79 - stata inspirado no 76
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('nome_5_12', 'sg_sexo', 'ds_nome_mae_sound', 'faixa_etaria', 'cd_cep_res', 'dia_nasc', 'ignora_maria'),
    c('ano_nasc'),
    79
  )

# Regra 80 - stata inspirado no 77
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('nome_5_12', 'sg_sexo', 'ds_nome_mae1', 'ds_nome_mae2_sound', 'dt_obito', 'dt_nasc'),
    c('ds_nome_mae1','ds_nome_pac1'),
    80
  )

# Regra 81 - stata inspirado no 78
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('nome_5_12', 'dt_obito', 'nu_idade_anos', 'ds_nome_pac3', 'dia_nasc', 'mes_nasc'),
    c('ds_nome_mae2'),
    81
  )

# Regra 82 - stata inspirado no 87
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac', 'ds_nome_pai', 'ds_nome_mae', 'dt_obito'),
    c('ds_nome_pai', 'ds_nome_mae'),
    82
  )

# Regra 83 - stata inspirado no 89
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac', 'ds_nome_mae', 'ds_nome_mae_sound', 'dt_nasc'),
    c('ds_nome_pac','ds_nome_mae', 'dt_obito', 'ds_nome_pai'),
    83
  )

# save(df_2, file = 'C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada_83.RData')

load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada_83.RData')

df_2 <- df_2 |>
  mutate(
    ano_obito = year(dt_obito),
    mes_obito = month(dt_obito),
    dia_obito = day(dt_obito)
  )

# Regra 84 - stata inspirado no 90
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'ds_nome_mae_sound', 'ds_nome_pai_sound', 'ano_obito', 'mes_obito'),
    c('ds_nome_mae_sound', 'ds_nome_pai_sound', 'ano_obito'),
    84
  )

# Regra 85 - stata inspirado no 91
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'ds_nome_mae_sound', 'ds_nome_pai_sound', 'ano_obito', 'ano_nasc', 'mes_nasc'),
    c('ds_nome_pac_sound', 'ds_nome_mae_sound', 'ds_nome_pai_sound', 'ano_obito'),
    85
  )

# Regra 86 - stata inspirado no 92
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_mae', 'dt_obito', 'ds_nome_pac_sound'),
    c('ds_nome_mae', 'dt_obito', 'ds_nome_pac'),
    86
  )

# Regra 87 - stata inspirado no 94
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'ds_nome_pai_sound', 'dt_obito'),
    c('ds_nome_pac_sound', 'ds_nome_pai_sound', 'dt_obito'),
    87
  )


# Regra 88 - stata inspirado no 95
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound','ds_nome_pai_sound', 'faixa_etaria', 'ano_obito', 'mes_obito', 'ano_nasc'),
    c('ds_nome_pac_sound', 'ds_nome_pai_sound', 'dt_obito'),
    88
  )


# Regra 89 - stata inspirado no 96
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'ds_nome_mae_sound', 'faixa_etaria', 'ds_bairro_res', 'ano_nasc', 'mes_nasc'),
    c('ds_nome_pac_sound', 'ds_nome_mae_sound', 'faixa_etaria', 'ds_bairro_res'),
    89
  )

#save(df_2, file = 'C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada_89.RData')
load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada_89.RData')

# Regra 90 - stata inspirado no 97 - pega irmãos
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_mae_sound', 'ds_nome_pai_sound', 'dt_obito', 'dt_nasc'),
    c('ds_nome_mae_sound', 'ds_nome_pai_sound', 'ano_nasc'),
    90
  )

# Regra 91 - stata inspirado no 99
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'ds_nome_mae_sound', 'dt_obito', 'ano_nasc'),
    c('ds_nome_pac_sound', 'ds_nome_mae_sound', 'dt_obito', 'ano_nasc'),
    91
  )



# Regra 92 - stata inspirado no 100
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'ds_nome_mae_sound', 'dt_obito', 'ano_nasc'),
    c('ds_nome_pac_sound', 'ds_nome_mae_sound', 'dt_obito', 'ano_nasc'),
    92
  )

df_2 <- df_2 |>
  mutate(mae1_menos3 = substr(ds_nome_mae, 1, nchar(ds_nome_mae) - 3))

# Regra 93
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('mae_menos5d', 'ds_nome_pac1', 'ds_nome_pac2_sound', 'dt_nasc', 'faixa_etaria', 'ds_bairro_res', 'mae1_menos3'),
    c('mae_menos5d', 'ds_nome_pac1_sound'),
    93
  )

# Regra 94
df_2 <- df_2 |>
  vitallinkage::regras_linkage(
    c('mae_menos5d', 'ds_nome_pac1', 'ds_nome_pac2_sound', 'dt_nasc', 'faixa_etaria', 'ds_bairro_res', 'mae1_menos3'),
    c('mae_menos5d', 'ds_nome_pac1_sound'),
    94
  )

# Regra 95
df_t <- df_2 |>
  vitallinkage::regras_linkage(
    c('mae_menos5d', 'ds_nome_pac1', 'ds_nome_pac2_sound', 'ano_nasc','mes_nasc', 'faixa_etaria', 'ds_bairro_res', 'mae1_menos3'),
    c('ds_nome_pac_sound'),
    95
  )


# save(df_2, file = 'C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada_95.RData')
load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/rn_linkada_89.RData')


tab_1(df_2, banco)


############# CONDIÇÕES ESPECIAIS

# 2º condição
d2_t <- df_2 |>
  mutate(mae1_menos3 = substr(ds_nome_mae, 1, nchar(ds_nome_mae) - 3)) |>
  arrange(mae_menos5d, ds_nome_pac1, ds_nome_pac2_sound, ano_nasc, faixa_etaria, ds_bairro_res, mae1_menos3) |>
  group_by(mae_menos5d, ds_nome_pac1, ds_nome_pac2_sound, ano_nasc, faixa_etaria, ds_bairro_res, mae1_menos3) |>
  mutate(nu_cpf_lag = lag(nu_cpf),
         N_par = ifelse((nu_cpf - nu_cpf_lag == 0 | is.na(nu_cpf_lag - nu_cpf) | nu_cpf == nu_cpf_lag) &
                          gemelar != 1 & ds_nome_pac1 != "RN" & ds_nome_pac1 != "FM" & ds_bairro_res != "CENTRO", n(), NA)) |>
  mutate(par_2 = ifelse(!is.na(N_par) & N_par > 1, cur_group_id(), NA)) |>
  ungroup() |>
  select(-nu_cpf_lag) |>
  meio_de_campo()



df_2$mae_men
df_t$par_c90

df_t$nome_meio
gc()

a <- a |> vitallinkage::start_linkage(c('ds_nome_pac_sound','ds_rua_res'), c('ds_nome_pac_sound', 'ds_nome_mae_sound'))

bla <- df_2 |> select(par_1, ds_nome_pac, ds_nome_pac1, ds_nome_pac2,
                      ds_nome_pac3, ds_nome_pac1_sound,
                      ds_nome_pac2_sound, ds_nome_pac3_sound,
                      dt_nasc, ds_nome_mae, ds_nome_mae1,
                      ds_nome_mae2, ds_nome_mae3, ds_nome_pai,nu_tel)

bla <- bla |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac', 'dt_nasc'),
    c('ds_nome_pac'),
    111
  )

a <- bla |> select(par_1, par_c111,ds_nome_pac, ds_nome_mae, ds_nome_pai, dt_nasc, )
df_2 <- df_t

df_t$nome_menos_5d

a <- df_2 |> filter(ds_nome_pac == 'RN') |>
  select(
    par_1, ds_nome_pac, ds_nome_pac1, ds_nome_pac2,
    ds_nome_pac3, ds_nome_pac1_sound,
    ds_nome_pac2_sound, ds_nome_pac3_sound,
    dt_nasc, ds_nome_mae, ds_nome_mae1,
    ds_nome_mae2, ds_nome_mae3, nu_tel
  )


b <- df_2 |> filter(par_1 %in% c(6859791)) |>
  select(par_1, ds_nome_pac, ds_nome_pac1, ds_nome_pac2, ds_nome_pac3, ds_nome_pac1_sound, ds_nome_pac2_sound, ds_nome_pac3_sound, dt_nasc, ds_nome_mae, ds_nome_mae1, ds_nome_mae2, ds_nome_mae3, nu_tel)
