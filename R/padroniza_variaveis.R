#' Padroniza os nomes das variáveis
#'
#' @param df Nome do dataframe
#' @param df2 Nome da tabela com os nomes padronizados
#' @param nome_base Nome da base a ser padronizada, ex: "SIM", "SINAN" ou "SIH"
#'
#' @return Retorna o dataframe com os nomes das colunas padronizadas, a coluna com o nome do Banco de dados (banco)
#' e uma coluna chamada "id_(nome do banco)". formada por: (nome do banco)_(número de 1 a n)_(código identificador)
#' @export
padroniza_variaveis <- function(df, df2, nome_base) {

  # nomes das variáveis originais e padronizadas filtradas para o SINAN
  names_info <- df2 |>
    dplyr::filter(fonte == nome_base) |>
    dplyr::select(var_names_orig, stanard_name)  # Fix typo: stanard_name to standard_name

  # Seleciona as variáveis do dataframe original
  df <- df |>
    dplyr::select(names_info$var_names_orig)

  # Renomeia as colunas do dataframe com os valores de standard_name
  names(df) <- names_info$stanard_name

  # Coluna com o nome do banco
  df <- df |>
    dplyr::mutate(
      banco = nome_base
    )

  # aplicar case para cada tipo de "nu_not"
  coluna_id <- paste0("id_", nome_base)
  n <- nrow(df)
  # Seleciona a coluna dinamicamente based on nome_base
  nu_col <- switch(
    nome_base,
    SINAN = df$nu_not,
    SIM = df$nu_do,
    SIH = df$ah_num_aih,
    # Add more cases if needed
    df$nu_not  # Default to df$nu_not if nome_base doesn't match any case
  )

  df[[coluna_id]] <- paste0(nome_base, '_', 1:n, '_', nu_col)

  return(df)
}
