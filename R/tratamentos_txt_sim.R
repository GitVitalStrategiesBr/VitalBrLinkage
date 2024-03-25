#' Tratamento textual no SIM
#'
#' @param df Nome do data frame
#'
#' @return Colunas com os valores textuais tratados e padronizados
#' @export
tratamentos_txt_sim <- function(df){

   df$NOMEPAI <- gsub("[^A-Za-z ]", "", df$NOMEPAI)
   df$NOME <- gsub("[^A-Za-z ]", "", df$NOME)
   df$NOMEMAE <- gsub("[^A-Za-z ]", "", df$NOMEMAE)

  df <- df |>
    # dplyr::mutate_at(dplyr::vars(NOMEPAI,NOME,NOMEMAE),
    #           ~gsub("[0-9]+", "",.)) |>
    dplyr::mutate_at(dplyr::vars(NOMEPAI,NOME,NOMEMAE),
              ~gsub("RECEM NASCIDO |RN NASCIDO |NATIMORTO", "",.)) |>
    # dplyr::mutate_at(dplyr::vars(NOMEPAI,NOME,NOMEMAE),
    #           ~gsub("-", " ",.)) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character),
                  ~str_squish(.))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character),
                  ~gsub(" FILHO| NETO| SOBRINHO| JUNIOR", "",.))) |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character),
                  ~gsub(" DE | DOS | DA | DOS | DAS | DO ", " ",.)))

  return(df)
}
