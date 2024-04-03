#' Ajuste nas colunas no formato de data
#'
#' @param df Nome do data frame
#' @param tipo_data Tipo do formato de data: 1 para "YYYYMMDD" ou 2 para "YYYY-MM-DD"
#'
#' @return O dataframe modificado, onde as colunas que começam com "dt_" ou "DT_"
#'          foram convertidas para o formato de data especificado pelo parâmetro tipo_data.
#' @export
ajuste_data <- function(df, tipo_data = 1){

  colunas_dt <- grep("^dt_|^DT_|^DT", names(df), value = TRUE)

  for (coluna in colunas_dt) {
    if (tipo_data == 1) {
      df[[coluna]] <- lubridate::dmy(df[[coluna]])
    } else if (tipo_data == 2) {
      df[[coluna]] <- lubridate::ymd(df[[coluna]])
    } else {
      stop("Tipo de data inv\u00e1lido. Use 1 para dmy ou 2 para ymd.")
    }
  }



  return(df)

}
