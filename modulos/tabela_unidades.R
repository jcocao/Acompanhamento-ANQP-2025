box::use(
  shiny[moduleServer, NS],
  dplyr[...],
  reactable[...],
)


#' @export
ui <- function(id) {
  ns <- NS(id)
  
  reactableOutput(ns("TblUnidadeEstatisticas"),
                  width = "100%")
  
  
}

#' @export
server <- function(id, dados, populacao) {
  moduleServer(id, function(input, output, session) {
    
    output$TblUnidadeEstatisticas <- renderReactable({

      pop_tratada <- populacao() %>%
        select(cod.unidade, Alvo = pop_a)
      
      dados_t <- dados() %>%
        #left_join(trad, by = c("DR")) %>%
        filter(!is.na(valido)) %>% 
        filter(valido == 1) %>%
        count(cod.unidade,
              name = "Validos") %>% 
        left_join(pop_tratada, by = c("cod.unidade" = "cod.unidade")) %>% 
        mutate(Taxa = Validos/Alvo)
      
      
      reactable(dados_t,
                pagination = FALSE,
                filterable = FALSE,
                highlight = FALSE,
                bordered = TRUE,
                striped = FALSE,
                height = 750,
                defaultColDef = colDef(format = colFormat(separators = TRUE,
                                                          locales = "pt-BR")),
                theme = reactableTheme(
                  color = "black",
                  highlightColor = "#8F93FF",
                  headerStyle = list(
                    color = "black",
                    fontWeight = "bold",
                    backgroundColor = "#B1D8B9",
                    fontSize = "18px"
                  )
                ),
                columns = list(
                  ead = colDef(
                    show = FALSE
                  ),
                  cod.unidade = colDef(
                    name = "Nome da Unidade"
                  ),
                  Validos = colDef(
                    filterable = FALSE,
                    name = "Total de questionários válidos",
                    align = "center",
                    style = list(
                      fontSize = "16px"
                      
                    )
                  ),
                  Alvo = colDef(
                    name = "População Alvo",
                    align = "center"
                  ),
                  Taxa = colDef(
                    name = "Taxa de resposta (%)",
                    filterable = FALSE,
                    format = colFormat(separators = TRUE,
                                       percent = TRUE,
                                       digits = 2),
                    align = "center",
                    style = list(
                      fontSize = "16px"
                    )
                  )
                )
                
      )
      
      
      
    })
    
  })
}

