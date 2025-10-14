box::use(
  shiny[tags]
)


box::use(
  modulos/carregar_dados[brasil,
                 dados_populacao,
                 hora_da_exportacao]
)

#' @export

cabecalho <- function(ano){
  saida <- tags$div(
    tags$h1(id = "titulo",
            paste("Painel de Acompanhamento da ANQP", ano, sep = " - ")
    ),
    
    tags$p(
      paste("Dados atualizados em",
            hora_da_exportacao)
    )
  )
  
  
  return(saida)
}