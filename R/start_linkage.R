#' Iniciar o processo de ligação de registros em um dataframe
#'
#' Esta função realiza as etapas iniciais do processo de ligação de registros em um dataframe, incluindo ordenação, agrupamento, cálculo de pares de registros semelhantes e aplicação de regras de ligação.
#'
#' @param df Um dataframe contendo os dados que serão utilizados no processo de ligação de registros.
#' @param variables Um vetor de caracteres contendo os nomes das variáveis a serem usadas no processo de ligação de registros.
#' @param chave Uma string contendo o nome da variável que servirá como chave única para identificar registros semelhantes.
#'
#' @return Retorna um dataframe com colunas adicionais contendo informações sobre a ligação de registros, como pares de registros semelhantes e identificadores de grupo.
#'
#' @export
start_linkage <- function(df, variables, chave) {

  df_name <- deparse(substitute(df))
  arrange_str <- paste("arrange(", paste(variables, collapse = ", "), ")", sep = "")
  group_by_str <- paste("group_by(", paste(variables, collapse = ", "), ")", sep = "")
  na_condition <- paste("!is.na(", variables, ")", sep = "", collapse = " & ")
  mutate_N_par_str <- paste("mutate(N_par = ifelse(", na_condition, " & (",
                            paste(chave, "== lag(", chave, ")", sep = "", collapse = " | "),
                            " | ",
                            paste(chave, "== lead(", chave, ")", sep = "", collapse = " | "),
                            "), n(), NA))", sep = "")

  code <- paste(df_name ,"|>" , arrange_str, " |> ", group_by_str, " |> ", mutate_N_par_str,
                " |> mutate(regra1 = ifelse(!is.na(N_par) & N_par > 1, 1, NA)) |> mutate(par_1 = cur_group_id() * (N_par > 1)) |> ungroup() |> select(-N_par) |> mutate(par_c1 = par_1) |> arrange(par_1)", sep = "")

  return(eval(parse(text = code)))

}


