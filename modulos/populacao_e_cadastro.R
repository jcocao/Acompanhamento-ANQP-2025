box::use(
  bslib[value_box,
        layout_column_wrap],
  bsicons[bs_icon],
  shiny[selectInput]
)

box::use(
  modulos/carregar_dados[opcoes]
)

#' @export
saida <- layout_column_wrap(
  width = "300px",
  selectInput(
    inputId = "selecao",
    label = "Escolha o Departamento Regional",
    choices = opcoes
  ),
  value_box(
    title = "Vendas",
    value = "R$ 45.000",
    showcase = bs_icon("people-fill")
  ),
  value_box(
    title = "Clientes",
    value = "1.230",
    showcase = bs_icon("person-check-fill")
  ),
  value_box(
    title = "Satisfação",
    value = "92%",
    showcase = bs_icon("percent")
  )
)