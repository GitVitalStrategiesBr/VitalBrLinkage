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


load('C:/vitalstrategies/data_sicence/TCC/script_linkage/dados/base_concatenada_rn.RData')

concat <-
  concat |> vitallinkage::ajuste_data(2)

concat$nu_cns <- as.integer(concat$nu_cns)

concat$cd_autorizador_doc <- as.integer(concat$cd_autorizador_doc)

concat_2 <- subset(concat, grepl("^A", ds_nome_pac, ignore.case = TRUE))

df <- concat |> vitallinkage::start_linkage(c("ds_nome_pac", "dt_nasc"), c("ds_nome_pac"))


df <- df |>
  vitallinkage::regras_linkage(
    c('ds_nome_pac_sound', 'nu_doc'),
    c('ds_nome_pac_sound'),
    5
  )

df <- df |>
  vitallinkage::regras_linkage(
    c()
  )

a <- df |> select(par_1, ds_nome_pac, ds_nome_pac_sound, ds_nome_mae, dt_nasc, banco, par_c1, par_c5)
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
                          ((ds_nome_mae_sound == lag(ds_nome_mae_sound)) | (ds_nome_mae_sound == lead(ds_nome_mae_sound))),
                        n(), NA))  |>
  mutate(regra2 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |>
  mutate(par_2 = ifelse(!is.na(N_par) & N_par > 1, 1 + cur_group_id(), NA)) |>
  ungroup() #|>
 #  select(-N_par) #|>
  # vitallinkage::meio_de_campo()




par_3_t <- df |>  meio_de_campo()


a <- par_3_t |>  select(par_1, par_2, par_final_temp,par_final_final, par_final, ds_nome_pac, ds_nome_pac_sound, ds_nome_mae, dt_nasc, banco,regra1, regra2) #|> filter(par_2%in%c(167627))

b <- a |> ungroup() |> group_by(par_2) |> mutate(par_final_b = ifelse(!is.na(par_final), max(par_final, na.rm=T), NA))

c <- b |>  filter(par_2%in%c(171728, ))

par_2_t$par_1
a <- df |> select(par_1, par_2, ds_nome_pac, ds_nome_pac_sound, dt_nasc, banco,regra1, regra2 )
b <- a |> filter(is.na(regra2) & regra1 == 1)
c <- a |> filter(ds_nome_pac_sound%in%c("A535C256L500"))


# contar quantos casos ele pegou - no meio de campo

d <- df_2 |> select(par_1, par_2, ds_nome_pac, ds_nome_pac_sound, dt_nasc, banco,regra1, regra2 )
f <- d |> filter(is.na(regra1) & regra2 == 1)
e <- d |> filter(ds_nome_pac_sound%in%c("A535C256L500"))

vitaltable::tab_1()


par_list_2 <- par_list[duplicated(par_list$par_2) | duplicated(par_list$par_2, fromLast = TRUE), ]



generate_code <- function(df, variables) {

  arrange_str <- paste("arrange(", paste(variables, collapse = ", "), ")", sep = "")
  group_by_str <- paste("group_by(", paste(variables, collapse = ", "), ")", sep = "")
  na_condition <- paste("!is.na(", variables, ")", sep = "", collapse = " & ")
  mutate_N_par_str <- paste("mutate(N_par = ifelse(", na_condition, " & (",
                            paste(variables, "== lag(", variables, ")", sep = "", collapse = " | "),
                            " | ",
                            paste(variables, "== lead(", variables, ")", sep = "", collapse = " | "),
                            "), n(), NA))", sep = "")

  code <- paste("df <-", df, "|>" , arrange_str, " |> ", group_by_str, " |> ", mutate_N_par_str,
                " |> mutate(regra1 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |> mutate(par_1 = cur_group_id() * (N_par > 1)) |> ungroup() |> select(-N_par) |> mutate(par_c1 = par_1) |> arrange(par_1, banco)", sep = "")

  return(eval(parse(text = code)))
}

variables <- c("ds_nome_pac", "dt_nasc")
cat(generate_code(concat,variables))

# Chame a função para gerar o código
codigo <- generate_code(concat, variables)

# Execute o código gerado
p <- eval(parse(text = codigo))
p |> select(par_1, ds_nome_pac, ds_nome_pac_sound, dt_nasc)
rm(p)



# Conta a frequência de cada valor
frequencias <- table(par_3_t$par_final)

# Retorna os valores que aparecem apenas uma vez
valores_unicos <- names(frequencias)[frequencias == 1]


a <- par_3_t |> arrange(par_1, par_2) |>  filter(par_1%in%c(3180,   38242,  54333, 66404, 85888, 87538,  223712, 260955, 271717,
                                      295690, 306722, 348053, 356896, 358138, 368074, 371738)) |>
  select(par_1, par_2, par_final_temp, par_final, ds_nome_pac, ds_nome_pac_sound, ds_nome_mae, dt_nasc, banco,regra1, regra2)

b <- par_3_t |>
  select(par_1, par_2, par_final_temp, par_final, ds_nome_pac, ds_nome_pac_sound, ds_nome_mae, dt_nasc, cd_causabas,banco, regra1, regra2) |>
  group_by(par_1) |>
  mutate(par_final = ifelse(all(is.na(par_1)), NA, max(par_final, na.rm = TRUE)))|>
  filter(par_1%in%c(1900, 3180,   38242,  54333, 66404, 85888, 87538,  223712, 260955, 271717,
                    295690, 306722, 348053, 356896, 358138, 368074, 371738))

gc()

c <- a |> filter(is.na(par_1) & !is.na(par_2))
d <- a |> filter(par_final%in%c$par_final)

k <- b |> filter(par_final%in%c(195368)|par_1%in%c(260958, 260958)|par_2%in%c(260958, 260958))

sim <- par_3_t |> filter(banco == "SIM")

sim <- as.data.frame(table(sim$par_final))

b_2 <- b |> filter(par_final == 95264)
