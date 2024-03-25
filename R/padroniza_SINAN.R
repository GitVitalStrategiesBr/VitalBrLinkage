#' Padroniza e realiza ajustes nos dados do SINAN.
#'
#' Esta função padroniza e realiza ajustes nos dados do Sistema de Informação de Agravos de Notificação (SINAN)
#'
#' @param df DataFrame contendo os dados do SINAN.
#' @param tipo_de_data (opcional) Tipo de ajuste para o formato de data. Pode ser 1 para formato DD/MM/AAAA ou 2 para AAAA-MM-DD.
#'
#' @return DataFrame contendo os dados do SINAN após as transformações.
#'
#' @export
padroniza_SINAN <- function(df, tipo_de_data = 2){

  # pipeline
  df <- df |>
    vitallinkage::drop_duplicados_sinan_1() |>  # Dropa as colunas duplicadas inicialmente
    vitallinkage::padroniza_variaveis(namestand,"SINAN") |> # Padroniza variáveis baseado no df nomestand - ADICIONAR O CRIA COLUNA ID
    vitallinkage::ajuste_data(tipo_data = tipo_de_data) |> # Ajustando o formato de data
    vitallinkage::ano_sinan() |>
    vitallinkage::nu_idade_anos_sinan() |> # ANO NASC SINAN
    vitallinkage::as_char() |>  # Transformar todos em texto
    vitallinkage::copia_nomes() |>
    vitallinkage::ajuste_txt() |> # Ajusta os textos de nomes
    vitallinkage::gemelar("ds_nome_pac") |>
    vitallinkage::limpa_ignorados_sim() |> # Remove textos de ignorado
    vitallinkage::soundex_linkage("ds_nome_pac") |>
    vitallinkage::soundex_linkage("ds_nome_mae") |>
    vitallinkage::ajuste_res() |> # Ajusta as variáveis que contem "_res" na composição
    vitallinkage::soundex_linkage("ds_bairro_res") |>
    vitallinkage::soundex_linkage("ds_rua_res") |>
    vitallinkage::soundex_linkage("ds_comple_res") |>
    vitallinkage::soundex_linkage("ds_ref_res") |>
    # NOVAS VARIÁVEIS
    vitallinkage::ds_raca_sinan() |> # Cria coluna descrevendo as raça no sinan
    vitallinkage::corrige_sg_sexo() |> # Corrige os registros de sexo Ignorado
    vitallinkage::nu_idade_anos_sinan() |> # Adiciona a coluna com a idade calculada
    vitallinkage::faixa_etaria() |>
    vitallinkage::as_char() #|>  # Transformar todos em texto

  }
