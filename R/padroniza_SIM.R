#' Tratamento da base SIM
#'
#' @param df Data frame com os dados do SIM
#' @param tipo_de_data O formato de data que deve retornar
#'
#' @return Base com os dados do SIM tratados
#' @export
padroniza_SIM <- function(df, tipo_de_data = 1){

  df <- df |>
    vitallinkage::limpa_ignorados_sim() |> # Remove textos de ignorado
    vitallinkage::padroniza_variaveis(namestand, "SIM") |> # Padronizando os nomes das variáveis
    vitallinkage::ajuste_data(tipo_data = tipo_de_data) |> # Ajustando o formato de data
    vitallinkage::ano_sim() |> # Adicionando o ano
    vitallinkage::as_char() |> # Transformando todos em character
    vitallinkage::copia_nomes() |>
    vitallinkage::ajuste_txt() |> # Ajusta as variáveis que contem "nome" na composição
    vitallinkage::gemelar("ds_nome_pac") |> # Cria coluna de gemelar
    vitallinkage::soundex_linkage("ds_nome_pac") |>
    vitallinkage::soundex_linkage("ds_nome_pai") |>
    vitallinkage::soundex_linkage("ds_nome_mae") |>
    vitallinkage::ajuste_res() |> # Ajusta as variáveis que contem "_res" na composição
    vitallinkage::soundex_linkage("ds_bairro_res") |>
    vitallinkage::soundex_linkage("ds_rua_res") |>
    vitallinkage::soundex_linkage("ds_comple_res") |>
    vitallinkage::drop_duplicados_sim_padronizado() |> # Série de lógicas para remover duplicados
    ## NOVAS VARIÁVEIS
    vitallinkage::ds_raca_sim() |> # Ajustando a raça/cor
    vitallinkage::corrige_sg_sexo() |> # Ajustando a variável sg_sexo
    vitallinkage::nu_idade_anos_sim() |> # Ajustanso a idade em anos
    vitallinkage::faixa_etaria() |>
    dplyr::mutate(morreu=1) |>
    vitallinkage::as_char() # Tudo como character

    return(df)
}
