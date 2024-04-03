#' Upper nos textos das colunas de Character
#'
#' @param df Nome do data frame
#'
#' @return Todas as colunas que começam com "ds_" e estiverem como Character retornarão com texto em caixa alta
#' @export
upper_case_ds <- function(df){
  df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::starts_with("ds_"),
        ~ if (is.character(.x) && !any(grepl("\\d", .x))) toupper(.x) else .x
      )
    )
}


