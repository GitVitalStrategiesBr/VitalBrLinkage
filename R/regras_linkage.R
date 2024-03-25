
#' Aplica Regras de Linkage em um Dataframe
#'
#' Esta função aplica regras de linkage específicas em um dataframe, organizando e agrupando dados
#' com base em variáveis específicas e critérios de chave. Ela permite a aplicação dinâmica de regras
#' de linkage, facilitando a identificação de registros correspondentes ou relacionados.
#'
#' @param df Dataframe sobre o qual as regras de linkage serão aplicadas.
#' @param variables Vetor de caracteres contendo os nomes das colunas usadas para organizar e agrupar os dados.
#' @param chave Vetor de caracteres contendo os nomes das colunas usadas como chaves para identificar registros relacionados.
#' @param num_regra Número inteiro que identifica a regra de linkage específica a ser aplicada.
#'
#' @return Dataframe modificado após a aplicação das regras de linkage. O dataframe resultante inclui colunas adicionais
#' indicando a aplicação das regras e a identificação de grupos relacionados.
#'
#' @export
regras_linkage <- function(df, variables, chave, num_regra) {

  df_name <- deparse(substitute(df)) # Captura o nome do dataframe passado como argumento

  # Cria a string para a função arrange
  arrange_str <- paste("arrange(", paste(variables, collapse = ", "), ")", sep = "")

  # Cria a string para a função group_by
  group_by_str <- paste("group_by(", paste(variables, collapse = ", "), ")", sep = "")

  # Cria a condição para verificar se as variáveis não são NA
  na_condition <- paste("!is.na(", variables, ")", sep = "", collapse = " & ")

  # Cria a string para a função mutate que calcula N_par
  mutate_N_par_str <- paste("mutate(N_par = ifelse(", na_condition, " & (",
                            paste(chave, "== lag(", chave, ")", sep = "", collapse = " | "),
                            " | ",
                            paste(chave, "== lead(", chave, ")", sep = "", collapse = " | "),
                            "), n(), NA))", sep = "")

  # Cria o código completo, incluindo as mutações específicas para a segunda condição
  code <- paste(df_name, "|>", arrange_str, "|>", group_by_str, "|>", mutate_N_par_str,
                "|> mutate(regra",num_regra, "= ifelse(!is.na(N_par) & N_par > 1, 1, NA))",
                "|> mutate(par_2 = ifelse(!is.na(N_par) & N_par > 1, (max(df$par_1, na.rm=TRUE) + cur_group_id()), NA), par_c",num_regra," = par_2)",
                "|> ungroup()",
                "|> vitallinkage::meio_de_campo()", sep = "")

  # Avalia e executa o código gerado
  return(eval(parse(text = code)))
}
