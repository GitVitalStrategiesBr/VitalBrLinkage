#' Filter a DataFrame for a Specific 'par_c' Column
#'
#' This function filters a given dataframe based on the condition that only one specific 'par_c' column
#' should have non-NA values, while all other 'par_c' columns should have NA values. It further filters
#' rows based on a comparison between 'par_1' and the specific 'par_c' column's values.
#'
#' @param df A dataframe containing the data to be filtered.
#' @param coluna_especifica The specific 'par_c' column name to be checked for non-NA values.
#'
#' @return A dataframe filtered based on the specified conditions. The function first identifies all
#' columns starting with "par_c" and dynamically constructs a filter expression. It filters the dataframe
#' to include only rows where the specified 'par_c' column has non-NA values, and all other 'par_c' columns
#' have NA values. Additionally, it further filters rows where 'par_1' does not match the value of the
#' specified 'par_c' column.
#'
#' @export
filtro_par_c_especifico <- function(df, coluna_especifica){
  # Identificar todas as colunas que começam com "par_c"
  colunas_par_c <- grep("^par_c", names(df), value = TRUE)

  # Criar uma expressão de filtro dinâmica
  condicoes <- lapply(colunas_par_c, function(coluna) {
    if (coluna == coluna_especifica) {
      return(sprintf("!is.na(%s)", coluna))
    } else {
      return(sprintf("is.na(%s)", coluna))
    }
  })

  expressao_filtro <- paste(condicoes, collapse = " & ")

  # Filtrar o dataframe com a expressão criada
  df_filtrado <- df |>
    dplyr::filter(eval(parse(text = expressao_filtro)))

  registro_novo <- df_filtrado |> mutate(conferir = par_1 == {{coluna_especifica}}) |> dplyr::filter(conferir == FALSE) |> distinct(par_1)

  df_filtrado <- df |> dplyr::filter(par_1%in%registro_novo$par_1)



  return(df_filtrado)
}
