library(tidyverse)
library(vitaltable)
devtools::load_all(".")


# Criando dados fictícios
dados <- data.frame(
  nome = c(NA, "JOAO PEDRO", "MARIA SANTOS", "MARIA SANTOS","PEDRO", "ANA MARIA", "AMA MARIA", "CARLOS ADAO", "JOAO P", "JAO PEDRO"),
  data_nascimento = c("1995-01-30","1990-05-15", "1985-10-20",NA, "1988-07-12", "1992-03-08", "1992-08-03", "1995-01-30", "1990-05-15", "1990-05-15"),
  nome_mae = c("ISABEL SILVA","MARIA CLARA", "SANDRA",'SANDRA', "LUCIA MARIA", "FERNANDA", "FERNANDA", "ISABEL SILVA", NA, "MARIA CLARA"),
  banco = c('SIM', "SIH", "SINAN", 'SIH', 'SIH','SIH', 'SINAN','SIH', 'SINAN', 'SIM')
)


# Criando colunas de soundex com a função soundex_linkage
dados_2 <- dados |> vitallinkage::soundex_linkage('nome') |>
  vitallinkage::soundex_linkage('nome_mae') |>
  select(-nome1, -nome2, -nome3, -nome2_sound, -nome3_sound, -nome_mae1, -nome_mae2, -nome_mae3,-nome_mae1_sound, -nome_mae2_sound, -nome_mae3_sound )

# Iniciando o linkage
dados_2 <- dados_2 |> vitallinkage::start_linkage(c("data_nascimento", "nome_sound"), c('nome_sound'))

# Começando as regras de linkage
dados_2 <- dados_2 |> vitallinkage::regras_linkage(c("nome_sound"), c('nome_mae_sound'), 2) # Regra 2
dados_2 <- dados_2 |> vitallinkage::regras_linkage(c("nome_mae_sound"), c('nome_sound'), 3) # Regra 3
dados_2 <- dados_2 |> vitallinkage::regras_linkage(c("data_nascimento"), c('nome_mae_sound'), 4) # Regra 4
dados_2 <- dados_2 |> vitallinkage::regras_linkage(c("data_nascimento"), c('nome1_sound'), 5) # Regra 5
dados_2 <- dados_2 |> select(par_1, everything()) |> arrange(par_1) # Colocando par_1 no início do df

# Preenchendo os campos vazios
dados_3 <- dados_2 |>
  group_by(par_1) |>
  mutate(
    data_nascimento_corrigida = ifelse(!is.na(par_1), max(data_nascimento, na.rm=TRUE), data_nascimento),
    nome_corrigido = ifelse(!is.na(par_1), max(nome, na.rm=TRUE), nome),
    nome_mae_corrigido = ifelse(!is.na(par_1), max(nome_mae, na.rm = TRUE), nome_mae)
  )

# Fazendo um dataframe apenas com as colunas selecionadas
dados_final <- dados_3 |>
  select(par_1, nome,nome_corrigido, nome_mae, nome_mae_corrigido, data_nascimento,data_nascimento_corrigida, banco)


dados_4 <- dados |>
  vitallinkage::soundex_linkage('nome') |>
  vitallinkage::soundex_linkage('nome_mae') |>
  select(-nome1, -nome2, -nome3, -nome1_sound, -nome2_sound, -nome3_sound, -nome_mae1,
         -nome_mae2, -nome_mae3, -nome_mae1_sound, -nome_mae2_sound, -nome_mae3_sound)

dados_4 <- dados_2 |> select(-nome1_sound,
                             par_1, nome,nome_sound, data_nascimento, nome_mae, nome_mae_sound, banco)


dados_4 <- dados_4 |> arrange(nome_sound)
dados_5 <- dados_2 |> select(-nome1_sound)

dados_3 <- dados_3 |> select(par_1, nome, nome_corrigido, data_nascimento, data_nascimento_corrigida,
                             nome_mae, nome_mae_corrigido)
