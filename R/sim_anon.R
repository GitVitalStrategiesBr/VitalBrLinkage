#' Anonimiza o dataframe do SIM
#'
#' @param df Nome do data frame
#'
#' @return Retorna o dataframe sem as colunas com caracteres identificaveis
#' @export
sim_anon <- function(df){

  df <- df  |>
    dplyr::select(
      -ds_nome_pac,
      -ds_nome_mae,
      -ds_nome_pai,
      -nu_cns,
      -ds_rua_res,
      -nu_num_res,
      -ds_bairro_res
      )

}
