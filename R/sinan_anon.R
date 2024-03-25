#' Anonimiza o dataframe do SINAN
#'
#' @param df Nome do data frame
#'
#' @return Retorna o dataframe sem as colunas com caracteres identificaveis
#' @export
sinan_anon <- function(df){

  sinan_anon <- df  |>
    dplyr::select(
      -ds_nome_pac,
      -ds_nome_mae,
      -ds_nome_mae,
      -nu_cns,
      -ds_rua_res,
      -nu_num_res,
      -ds_bairro_res,
      -nu_tel
      )

  return(sinan_anon)

}
