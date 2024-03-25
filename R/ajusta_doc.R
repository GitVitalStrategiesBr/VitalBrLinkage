#' Ajusta Documentos no Dataframe
#'
#' Esta função realiza várias operações de limpeza e ajuste em colunas específicas de documentos em um dataframe.
#' Inicialmente, remove espaços em branco no início e no fim dos valores nas colunas de documentos. Em seguida, identifica
#' e marca valores que contêm caracteres não numéricos. Esses valores são então substituídos por `NA`. A função também
#' aplica uma série de regras condicionais para substituir conteúdos específicos por `NA` com base em critérios pré-definidos
#' de validade ou relevância dos números de documentos. Por fim, a função tenta padronizar e corrigir os números de documentos,
#' convertendo-os para formatos numéricos quando possível e criando novas colunas para representar documentos verificados
#' e ajustados conforme as regras aplicadas.
#'
#' @param df Dataframe contendo as colunas de documentos a serem ajustadas.
#'           Espera-se que pelo menos as colunas `nu_doc` e `nu_cns` estejam presentes no dataframe.
#'
#' @return Dataframe modificado com as colunas de documentos ajustadas.
#'         As operações incluem a remoção de caracteres não numéricos, a aplicação de regras de substituição condicional,
#'         a conversão de valores para `NA` quando apropriado, e a adição de novas colunas ajustadas `nu_doc_n`, `nu_cns_n`,
#'         e `nu_cpf` com valores possivelmente convertidos para formatos numéricos. A coluna `verificando` é adicionada
#'         para indicar valores de documentos considerados válidos após o processo de verificação.
#'
#' @export
ajusta_doc <- function(df) {
  # Identificar non_numeric em nu_doc
  df <- df |>
    dplyr::mutate(nu_doc = stringr::str_trim(nu_doc),
           non_numeric = ifelse(stringr::str_detect(nu_doc, "[^0-9.-]"), TRUE, FALSE))

  # Substituir nu_doc por "" onde non_numeric é TRUE e remover coluna non_numeric
  df <- df |>
    dplyr::mutate(nu_doc = ifelse(non_numeric, "", nu_doc)) |>
    dplyr::select(-non_numeric)

  # Substituição condicional para nu_cns, nu_doc
  vars_to_modify <- c("nu_cns", "nu_doc")

  for (var in vars_to_modify) {
    df <- df |>
      dplyr::mutate(!!dplyr::sym(var) := ifelse(nchar(!!dplyr::sym(var)) <= 5 | !!dplyr::sym(var) %in%
                                    c("999999999999999", "99999999999999", "9999999999999", "999999999999",
                                      "99999999999", "9999999999", "999999999", "99999999", "9999999",
                                      "1234567890", "12345678919", "12345678910", "1234567891", "123456789",
                                      "12345678", "10000000000", "1000000000", "100000000", "10000000",
                                      "1000000", "1234567891011", "1111111111111", "111111111111", "11111111111",
                                      "111111111", "11111111", "1111111", "111111", "12121212"), "", !!dplyr::sym(var)))
  }

  df$nu_doc[df$nu_doc == ""] <- NA
  df$nu_cns[df$nu_cns == ""] <- NA

  # Conversão e criação de novas colunas
  df <-  df |>
    dplyr::mutate(nu_doc_n = ifelse(stringr::str_detect(nu_doc, "^[0-9.]+$"), nu_doc, NA_character_),
           nu_cns_n = ifelse(stringr::str_detect(nu_cns, "^[0-9.]+$"), nu_cns, NA_character_),
           nu_cpf = nu_doc_n, # Inclusão de nu_cpf
           verificando = dplyr::case_when(
             stringr::str_detect(nu_doc, "^[0-9]+$") ~ nu_doc,
             stringr::str_detect(nu_cns, "^[0-9]+$") ~ nu_cns,
             TRUE ~ NA_character_
           ))

  # Condição específica para nu_cpf
  df <- df |>
    dplyr::mutate(nu_cpf = ifelse(is.na(nu_cpf) & !is.na(nu_doc_n) & as.numeric(nu_doc_n) > 10000000000, nu_doc, nu_cpf))

  return(df)
}
