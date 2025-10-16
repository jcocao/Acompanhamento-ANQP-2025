box::use(
  bslib[value_box,
        layout_column_wrap],
  bsicons[bs_icon],
  shiny[NS,
        moduleServer,
        textOutput,
        renderText],
)

box::use(
  modulos/funcoes_auxiliares[formatar_numero,
                             calcular_estatistica_coluna],
  modulos/extras[...]
)

#' @export
ui <- function(id) {
  
  ns <- NS(id)
  
  layout_column_wrap(
    width = 1,
    #total_acessos = 
    value_box(
      title = "Total de Acessos:",
      value = textOutput(ns("TotalAcessos")),
      showcase = bs_icon("people-fill",
                         size="0.5em"),
      showcase_layout = "left center",
      max_height = "150px",
      full_screen = FALSE,
      theme = tema_caixa_de_valor
    ),
    #tempo_medio_de_resposta =
    value_box(
      title = "Tempo médio de resposta:",
      value = textOutput(ns("TempoMedio")),
      showcase = bs_icon("clipboard-check-fill",
                         size="0.5em"),
      showcase_layout = "left center",
      max_height = "150px",
      full_screen = FALSE,
      theme = tema_caixa_de_valor
    ),
    #tempo_mediano_de_resposta = 
    value_box(
      title = "Tempo mediano de resposta:",
      value = textOutput(ns("TempoMediano")),
      showcase = bs_icon("percent",
                         size="0.5em"),
      showcase_layout = "left center",
      max_height = "150px",
      full_screen = FALSE,
      theme = tema_caixa_de_valor
    )
  )
}

#' @export
server <- function(id, dados_filtrado) {
  moduleServer(id, function(input, output, session) {
    
    output$TotalAcessos <- renderText({
      saida <- nrow(dados_filtrado())
        
      saida <- formatar_numero(saida)
      
      return(saida)
    })
    
    output$TempoMedio <- renderText({
      
      x <- dados_filtrado()
      
      if(nrow(x) > 0){
        saida <- calcular_estatistica_coluna(x,
                                             "tempo",
                                             "media") |>
          formatar_numero(
            digitos = 1, 
            ndigitos = 1) |> 
          paste("mins")
      }
      
      if(nrow(x) == 0) saida <- "0"
      
      return(saida)
    })
    
    output$TempoMediano <- renderText({
      
      x <- dados_filtrado()
      if(nrow(x) > 0){
        saida <- calcular_estatistica_coluna(x,
                                             "tempo",
                                             "mediana") |>
          formatar_numero(
            digitos = 1, 
            ndigitos = 1) |> 
          paste("mins")
      }
      
      if(nrow(x) == 0) saida <- "0"
      
      return(saida)
    })
    
  })
}
