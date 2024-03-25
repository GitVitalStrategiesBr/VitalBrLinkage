#' Atualiza Data Comum com Base em Condições Específicas
#'
#' Esta função atualiza a coluna `dt_comum` de um dataframe com valores de outras colunas de datas (`dt_obito`, `dt_notific`),
#' baseando-se no valor da coluna `banco`. A atualização é feita conforme as seguintes regras:
#' - Se `banco` for "SIM", `dt_comum` é atualizado para o valor de `dt_obito`.
#' - Se `banco` for "SINAN", `dt_comum` é atualizado para o valor de `dt_notific`.
#' - Se `banco` for "SIH", `dt_comum` é atualizado para o valor de `dt_internacao`.
#'
#' @param df Dataframe que contém as colunas `dt_comum`, `dt_obito`, `dt_notific` e `banco`.
#'
#' @return Dataframe com a coluna `dt_comum` atualizada conforme as condições especificadas.
#'
#' @export
atualiza_dt_comum <- function(df){

  df <- df |>
    dplyr::mutate(dt_comum = dplyr::case_when(
      banco == "SIM" ~ dt_obito,
      banco == "SINAN" ~ dt_notific,
      banco == "SIH" ~ dt_internacao,
      TRUE ~ NA # Mantém o valor original caso nenhuma condição acima seja satisfeita
    ))

  return(df)
}
