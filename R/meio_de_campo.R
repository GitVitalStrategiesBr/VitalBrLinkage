#' Função meio_de_campo
#'
#' Esta função manipula um quadro de dados com base em condições e operações específicas.
#'
#' @param df Um data frame contendo os dados de entrada.
#'
#' @return Um data frame modificado com ajustes feitos de acordo com as condições especificadas.
#'
#' @export
meio_de_campo <- function(df) {

  par_list <-
    df  |>
    dplyr::distinct(par_1, par_2) |>
    dplyr::filter(!is.na(par_1), !is.na(par_2)) |>
    dplyr::rename('par_temp' = par_1) |>
    dplyr::group_by(par_2)  |>
    dplyr::mutate(par_final_temp = max(par_temp)) |>
    dplyr::ungroup() |>
    dplyr::distinct(par_2, par_final_temp)

  df <- dplyr::left_join(df, par_list, by = "par_2") |>
    dplyr::mutate(par_final = dplyr::coalesce(par_final_temp, par_1, par_2)) |>
    dplyr::group_by(par_1) |>
    dplyr::mutate(par_final_final = ifelse(all(is.na(par_1)), NA, max(par_final, na.rm = TRUE)),
           par_final = dplyr::coalesce(par_final_final, par_final)) |>
    dplyr::ungroup() |>
    dplyr::select(-par_1, -par_2, -par_final_temp, -par_final_final) |>
    dplyr::rename("par_1" = par_final)



  return(df)

}
