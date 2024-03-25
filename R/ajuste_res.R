#' Ajusta as colunas de texto do dataframe
#'
#' Essa função realiza ajustes específicos nas colunas de texto de um dataframe, incluindo
#' a remoção de espaços extras, a transliteração de caracteres acentuados para ASCII,
#' a normalização para letras maiúsculas, e outras operações específicas para colunas
#' que contenham a substring "_res".
#'
#' @param df Um dataframe a ser ajustado.
#'
#' @return Retorna o dataframe com os ajustes realizados.
#'
#' @details
#' A função percorre cada coluna do dataframe e, se for uma coluna de texto e
#' contiver a substring "_res" (ignorando maiúsculas e minúsculas), realiza os seguintes ajustes:
#'   - Translitera caracteres acentuados para ASCII.
#'   - Converte o texto para letras maiúsculas.
#'   - Remove espaços extras.
#'   - Remove prefixos específicos, como "RECEM NASCIDO", "NATIMORTO", entre outros.
#'   - Remove sufixos específicos, como "FILHO", "NETO", "SOBRINHO", "JUNIOR", entre outros.
#'   - Normaliza a utilização de preposições ("DE", "DOS", "DA", "DAS", "DO", "DDAS", "E") para espaços.
#'   - Remove caracteres não alfabéticos e não numéricos.
#'   - Remove repetições excessivas de letras (mais de duas vezes).
#'   - Troca novamente 2 ou mais espaços por um único espaço.
#'
#' @examples
#' \dontrun{
#'   df <- data.frame(NOME_res = c("João da Silva", "Maria dos Santos", "José Neto Junior"))
#'   resultado <- ajuste_res(df)
#' }
#'
#' @export
ajuste_res <- function(df){

  # Remove mais de um espaço e ajusta nas colunas de texto
  for (coluna in names(df)) {
    if (is.character(df[[coluna]])) {
      # Verifica se o nome da coluna contém "nome" ou "NOME"
      if (grepl("_res", coluna, ignore.case = TRUE)) {
        # Translitera caracteres acentuados
        df[[coluna]] <- iconv(df[[coluna]], "UTF-8", "ASCII")
        df[[coluna]] <- toupper(df[[coluna]])

        # Remove espaços extras
        df[[coluna]] <- gsub("\\s{2,}", " ", df[[coluna]])

        # Ajustes específicos para colunas contendo "nome" ou "NOME"
        df[[coluna]] <- stringr::str_squish(df[[coluna]])
        df[[coluna]] <- gsub("END. ENCONTRADO|NAO INFORMADO|NAO INFORMO|NAO DECLARADO|NAO INFORMADA|ENDEREÇO NÃO ENCONTRADO OU NÃO É DO ESPÍRITO SANTO|END NÃO INFORMADO|END. NAO ENCONTRADO|END NAO INFORMADO|END INCOMPLETO|END NAO FORNECIDO|END. INCOMPLETO|END NAO ENCONTRADO|ENDERECO NAO INFORMADO|NAO IDENTIFICADO|ENDERECO INCOMPLETO|ENDEREÇO INCOMPLETO .|ENDEREÇO INCONSISTENTE |ENDERECO INFORMADO|NAO SABE INFORMAR", "", df[[coluna]])
        df[[coluna]] <- gsub("^APT |^APARTAMENTO |^AP ", "APTO", df[[coluna]],perl = TRUE)
        df[[coluna]] <- gsub("^R |^RUA |^AV |^AVENIDA |^AVENID ", "", df[[coluna]],perl = TRUE)
        df[[coluna]] <- gsub(" ZOAN RURAL|ZOA ARURAL|ZOBA RURAL|ZOINA RURAL|ZON RURAL|ZOAN RUAL|ZOAN RUARL|ZONA RURAL ES|ZONA RURRAL|ZUNA RURAL|ZZONA RURAL|ZONA RURAL S N|ZINA RURAL|ZOA RURAL|ZOANRURAL S N|Z RURAL|ZANA RUAL S N|ZANA RURAL S N|ZONA RURAL SN", "ZONA RURAL", df[[coluna]])
        # Remove caracteres não alfabéticos
        df[[coluna]] <- gsub("[^A-Za-z0-9 ]", "", df[[coluna]])
        # Troca novamente 2 ou mais espaços para um único espaço
        df[[coluna]] <- gsub("\\s{2,}", " ", df[[coluna]])
        # Se a letra repete mais de duas vezes, remove uma
        df[[coluna]] <- gsub("(.)\\1{2,}", "\\1", df[[coluna]], perl = TRUE)

        # Replace non-ASCII characters with Unicode escape sequences
        #  df[[coluna]] <- iconv(df[[coluna]], "UTF-8", "ASCII", sub = "byte")
        # df[[coluna]] <- gsub("[^\\x00-\\x7F]", "", df[[coluna]])
      }
    }
  }

  return(df)
}
