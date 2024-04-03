#' Tratamento da base SIH
#'
#' @param df Data frame com os dados do SIH
#' @param tipo_de_data O formato de data que deve retornar
#'
#' @return Base com os dados do SIH tratados
#' @export
padroniza_SIH <- function(df, tipo_de_data =2){

  df <- df |>
    vitallinkage::upper_case_char() |> # As colunas com texto passam a ficar com letra maiuscula
    vitallinkage::padroniza_variaveis(namestand,'SIH') |> # Padroniza as variáveis
    vitallinkage::ajuste_data(tipo_data = tipo_de_data) |> # Ajustando o formato de data
    vitallinkage::ano_sih() |> #cria a coluna de ano
    vitallinkage::as_char() |> # Transformando todos em character
    vitallinkage::copia_nomes() |> # clona colunas de nome
    vitallinkage::ajuste_txt() |> # Ajusta os textos de nomes
    vitallinkage::gemelar("ds_nome_pac") |>
    #base::unique() |> # Novos valores únicos após o tratamento
    vitallinkage::soundex_linkage("ds_nome_pac") |>
    vitallinkage::soundex_linkage("ds_nome_mae") |>
    vitallinkage::ajuste_res() |>
    vitallinkage::soundex_linkage("ds_bairro_res") |>
    vitallinkage::soundex_linkage("ds_comple_res") |>
    vitallinkage::soundex_linkage("ds_nome_pac_res") |>
    vitallinkage::soundex_linkage("ds_rua_res") |>
    # NOVAS VARIÁVEIS
    vitallinkage::ds_raca_sih() |> # Cria coluna ds_raca
    vitallinkage::corrige_sg_sexo() |>
    vitallinkage::nu_idade_anos_sih() |> # Cria coluna de anos
    vitallinkage::faixa_etaria() |> # Cria coluna de faixa etária
    vitallinkage::dias_int_sih() |> # Dias de internação no SIH
    vitallinkage::as_char() # Transforma todos em character

  return(df)

}
