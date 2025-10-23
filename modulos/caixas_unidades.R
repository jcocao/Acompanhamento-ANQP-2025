box::use(
  bslib[value_box,
        layout_column_wrap],
  bsicons[bs_icon],
  dplyr[...],
  shiny[NS,
        moduleServer,
        textOutput,
        reactive,
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
    width = "400px",
    value_box(
      title = "População Alvo da Unidade:",
      value = textOutput(ns("PopAlvoUnidade")),
      showcase = bs_icon("people-fill",
                         size="0.6em"),
      showcase_layout = "left center",
      max_height = "150px",
      full_screen = FALSE,
      theme = tema_caixa_de_valor
    ),
    value_box(
      title = "Questionários Válidos da Unidade:",
      value = textOutput(ns("ValidosUnidade")),
      showcase = bs_icon("clipboard-check-fill",
                         size="0.6em"),
      showcase_layout = "left center",
      max_height = "150px",
      full_screen = FALSE,
      theme = tema_caixa_de_valor
    ),
    value_box(
      title = "Taxa de Resposta da Unidade:",
      value = textOutput(ns("TaxaRespostaUnidade")),
      showcase = bs_icon("percent",
                         size="0.6em"),
      showcase_layout = "left center",
      max_height = "150px",
      full_screen = FALSE,
      theme = tema_caixa_de_valor
    )
  )
}

#' @export
server <- function(id, dados_filtrado, populacao_filtrada) {
  moduleServer(id, function(input, output, session) {
    
    pop_alvo <- reactive({
      populacao_filtrada() %>%
        slice(1) %>%
        pull(pop_a)
    })
    
    validos_unidade <- reactive({
    saida <- dados_filtrado()
    
    saida <- nrow(saida)
    
    return(saida)
    })
    
    output$PopAlvoUnidade <- renderText({
      saida <- pop_alvo()
      
      saida <- formatar_numero(saida,
                               ndigitos = 0)
      
      return(saida)
    })
    
    output$ValidosUnidade <- renderText({
      
      validos <- validos_unidade()
      saida <- formatar_numero(validos,
                               ndigitos = 0)
      
      return(saida)
    })
    
    output$TaxaRespostaUnidade <- renderText({
      
     validos <- validos_unidade()
     pop.alvo <- pop_alvo()
     
     saida <- validos / pop.alvo 
     
     saida <- formatar_numero(saida, percent = T,
                              digitos = 1, 
                              ndigitos = 1)
      
     return(saida)
    })
    
  })
}
