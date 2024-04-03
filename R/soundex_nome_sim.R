#' Aplica SoundexBR na coluna de NOME do SIM
#'
#' @param df Nome do data frame
#'
#' @return Retorna colunas com os nomes transformados em sons para o computador
#' @export
soundex_nome_sim <- function(df){

  ## Separar componentes do nome
  df$NOME1 = stringi::stri_extract_first_words(df$NOME)
  df$NOME2 = sub(".+? ", "", df$NOME)
  df$NOME2 = sub("\\s*\\w*$", "", df$NOME2)
  df$NOME3 = stringi::stri_extract_last_words(df$NOME)

  df <- df |>
    dplyr::relocate(
      c(NOME1,NOME2,NOME3),
      .after = NOME
    )

  #SOUNDEXBR
  df$NOME1_SOUND = SoundexBR::soundexBR(df$NOME1, BR=TRUE, useBytes = FALSE)
  df$NOME2_SOUND = SoundexBR::soundexBR(df$NOME2, BR=TRUE, useBytes = FALSE)
  df$NOME3_SOUND = SoundexBR::soundexBR(df$NOME3, BR=TRUE, useBytes = FALSE)

  df <- df |>
    dplyr::relocate(
      c(NOME1_SOUND,NOME2_SOUND,NOME3_SOUND),
      .after = NOME3
    ) |>
    dplyr::mutate(
      NOME_SOUND=paste0(NOME1_SOUND,NOME2_SOUND,NOME3_SOUND)
      )


}
