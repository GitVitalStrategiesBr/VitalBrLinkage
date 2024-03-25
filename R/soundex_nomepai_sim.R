#' Aplica SoundexBR na coluna de NOMEPAI do SIM
#'
#' @param df  Nome do data frame
#'
#' @return Retorna colunas com os nomes transformados em sons para o computador
#' @export
soundex_nomepai_sim <- function(df){

  ## Separar componentes do nome
  df$NOMEPAI1 = stringi::stri_extract_first_words(df$NOMEPAI)
  df$NOMEPAI2 = sub(".+? ", "", df$NOMEPAI)
  df$NOMEPAI2 = sub("\\s*\\w*$", "", df$NOMEPAI2)
  df$NOMEPAI3 = stringi::stri_extract_last_words(df$NOMEPAI)

  df <- df |>
    dplyr::relocate(
      c(NOMEPAI1,NOMEPAI2,NOMEPAI3),
      .after = NOMEPAI
    )

  df$NOMEPAI1_SOUND = SoundexBR::soundexBR(df$NOMEPAI1, BR=TRUE, useBytes = FALSE)
  df$NOMEPAI2_SOUND = SoundexBR::soundexBR(df$NOMEPAI2, BR=TRUE, useBytes = FALSE)
  df$NOMEPAI3_SOUND = SoundexBR::soundexBR(df$NOMEPAI3, BR=TRUE, useBytes = FALSE)

    df <- df |>
    dplyr::relocate(
      c(NOMEPAI1_SOUND,NOMEPAI2_SOUND,NOMEPAI3_SOUND),
      .after = NOMEPAI3
    ) |>
      dplyr::mutate(
        NOMEPAI_SOUND=paste0(NOMEPAI1_SOUND,NOMEPAI2_SOUND,NOMEPAI3_SOUND)
      )

}
