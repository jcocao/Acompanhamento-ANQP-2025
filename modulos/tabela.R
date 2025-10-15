box::use(
  shiny[moduleServer, NS],
  dplyr[...],
 reactable[...],
)


#' @export
ui <- function(id) {
  ns <- NS(id)
      
      reactableOutput(ns("TblDrEstatisticas"),
               width = "100%")
    

}

#' @export
server <- function(id, dados, opcoes) {
  moduleServer(id, function(input, output, session) {
    
    output$TblDrEstatisticas <- renderReactable({
      
      trad <- data.frame(Nomes = opcoes %>% names,
                         DR = unname(opcoes),
                         stringsAsFactors = FALSE) %>% 
        mutate(Nomes = factor(Nomes))
      
      dados_t <- dados() %>%
        left_join(trad, by = c("DR")) %>%
        filter(!is.na(valido)) %>% 
        mutate(DR = Nomes,
               .keep = "unused") %>% 
        group_by(DR, ead) %>%
        filter(DR != "SG") %>%
        summarise(Validos = sum(valido == "1"),
                  Total = unique(Total),
                  Taxa = (Validos/Total))
     
                  
      
      reactable(dados_t,
                pagination = FALSE,
                filterable = FALSE,
                highlight = TRUE,
                bordered = TRUE,
                striped = FALSE,
                height = 750,
                defaultColDef = colDef(format = colFormat(separators = TRUE,
                                                          locales = "pt-BR")),
                theme = reactableTheme(
                  color = "black",
                  highlightColor = "#8aa8ff",
                  headerStyle = list(
                    color = "white",
                    fontWeight = "bold",
                    backgroundColor = "#ffa32a",
                    fontSize = "18px"
                                     )
                ),
                columns = list(
                  ead = colDef(
                    show = FALSE
                  ),
                  DR = colDef(
                    name = "Departamento Regional"
                  ),
                  Validos = colDef(
                    filterable = FALSE,
                    name = "Total de questionários válidos",
                    align = "center",
                    style = list(
                      fontSize = "16px"
                      
                    )
                                      ),
                  Total = colDef(
                    name = "População Alvo",
                    align = "center"
                  ),
                  Taxa = colDef(
                    name = "Taxa de resposta (%)",
                    filterable = FALSE,
                    format = colFormat(separators = TRUE,
                                       percent = TRUE,
                                       digits = 1),
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
    
