#' Função para filtrar casos novos em um conjunto de dados
#'
#' Esta função recebe um conjunto de dados e o nome de uma coluna para realizar
#' a filtragem de casos novos com base em uma variável criada previamente.
#' A função requer que a função `vitallinkage::start_linkage()` seja aplicada
#' anteriormente, pois ela cria a variável `par_1` necessária para o funcionamento
#' correto desta função.
#'
#' @param df Um data frame contendo os dados a serem filtrados.
#' @param col O nome da coluna que será utilizada para a filtragem de casos novos.
#'
#' @return Um data frame contendo apenas os casos novos conforme especificado pela coluna fornecida.
#'
#' @export
casos_novos <- function(df, col){

  df <- df |> mutate(conferir = par_1 == {{col}}) |>
    dplyr::filter(conferir == TRUE) |>
    dplyr::select(par_1, {{col}}, ds_nome_pac, dt_nasc, ds_nome_mae, nu_cpf, nu_cns, cd_mun_not ,banco)

  return(df)

}
