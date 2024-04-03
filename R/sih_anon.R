#' Anonimiza SIH
#'
#' @param df nome do data frame
#'
#' @return Retorna o data frame anonimizado, removendo as colunas com informações pessoais
#' @export
sih_anon <- function(df){

  df<-df |>
    dplyr::select(
      -ds_nome_pac,
      -ds_nome_mae,
      -nu_cns,
      -ds_rua_res,
      -nu_num_res,
      -ds_bairro_res,
      -nu_tel
    )

}
